#correct reservoir discharge based on reported design capacity of ibwt

rm(list=ls())

library(dplyr)
library(lubridate)
library(stringr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#directories
inputDir <- '../../../output/ibwt/model/'
inputDirRaw <- paste0(inputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/')
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
outputDirBias <- paste0(inputDir, '1_discharge_timeseries/1_bias_corrected/')

#raw timeseries
ts.raw.monthly <- read.csv(paste0(inputDirRaw, '1_discharge_raw_processed_monthly.csv'))
ts.raw.yearly <- read.csv(paste0(inputDirRaw, '1_discharge_raw_processed_yearly.csv'))

#reservoirs without modelled discharge
reservoirs.no.discharge <- read.csv(paste0(inputDirRaw, '2_reservoirs_no_discharge.csv'))

#### other reservoir data
# reported design capacity
discharge.data.reported <- read.csv(
  '../../../input/global_data/ibwt/ibwt_mp_volumes_reported.csv') 

#tranfer sections (reservoirs)
data.sections <- read.csv(paste0(outputDirBias, '0_information_sections.csv'))

#transfer segmetnts (intakes)
data.segments <- read.csv(paste0(inputDirElevation, '1_information_intakes.csv'))


#### processing
#first process transfers with only one reservoir
ibwt.reservoir.count <- data.sections %>% 
  group_by(transfer.name) %>% 
  summarise(count=n())

ibwt.reservoir.one <- ibwt.reservoir.count %>% 
  filter(count == 1)

ibwt.reservoir.mult <- ibwt.reservoir.count %>% 
  filter(count != 1)


# get reservoir ids of transfers with one reservoir
ids.one.reservoir <- data.sections %>% 
  filter(transfer.name %in% ibwt.reservoir.one$transfer.name)

# # check reservoirs with zero discharge where ibwt has only one reservoir
# res.nodis.one <- ids.one.reservoir$reservoir.id %in% reservoirs.no.discharge
# # seems like all transfers with one reservoir have modelled discharge

#### correct discharge of transfers with only one reservoir #### 

discharge.corrected.monthly.list <- list()
discharge.corrected.yearly.list <- list()

n.transfers <- unique(ibwt.reservoir.one$transfer.name)
n.transfers.sel <- discharge.data.reported %>%
  filter(transfer.name %in% n.transfers)

setdiff(n.transfers, n.transfers.sel$transfer.name)
#great manmade river is here excluded (no reservoirs, only groundwater)

max.discharge.list <- list()

for(i in seq(nrow(n.transfers.sel))){

  #get transfer basic information
  transfer.info <- n.transfers.sel[i,]
  transfer.sel.name <- transfer.info$transfer.name
  transfer.design.cap <- transfer.info$design.cap.km3.y
  
  transfer.sections <- data.sections %>% 
    filter(transfer.name == transfer.sel.name)
  
  #reservoir id 
  reservoir.sel <- ids.one.reservoir$reservoir.id[ids.one.reservoir$transfer.name == transfer.sel.name]
  reservoir.lat <- ids.one.reservoir$lat[ids.one.reservoir$transfer.name == transfer.sel.name]
  
  if(reservoir.lat > 0){reservoir.hemisphere = 'North'}
  else{reservoir.hemisphere = 'South'}
  
  #get raw discharge from pcrglobwb
  #monthly
  transfer.discharge.monthly <- ts.raw.monthly %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(reservoir.id == reservoir.sel) %>% 
    mutate(section.id.unique = transfer.sections$section.id.unique)
  
  #yearly
  transfer.discharge.yearly <- ts.raw.yearly %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(reservoir.id == reservoir.sel) 
  
  #get max discharge (compare maximum modelled with design capacity)
  transfer.max.raw.km3.y <- max(transfer.discharge.yearly$discharge.km3.y)
  
  max.discharge.df <- data.frame(transfer.sel.name, transfer.max.raw.km3.y)
  colnames(max.discharge.df) <- c('transfer.name','max.modelled.km3.y')
  
  max.discharge.list[[i]] <- max.discharge.df
  
  #get bias correction factor, based on yearly time series
  bias.mult.factor <-
    transfer.design.cap / transfer.max.raw.km3.y
  
  #apply correction to monthly
  ts.corrected.monthly <- transfer.discharge.monthly %>%
    mutate(discharge.m3.month.corrected = discharge.m3.month * bias.mult.factor,
           discharge.km3.month.corrected = discharge.km3.month * bias.mult.factor,
           transfer.name = transfer.sel.name) %>%
    relocate(transfer.name, .before=reservoir.id) %>% 
    relocate(section.id.unique, .before=reservoir.id) %>% 
    mutate(datetime = as.Date(datetime),
           hemisphere = reservoir.hemisphere) 
  
  #upscale to yearly
  ts.corrected.yearly <- ts.corrected.monthly %>%
      mutate(datetime = floor_date(datetime,"year")) %>%
      group_by(datetime, country, transfer.name, section.id.unique, reservoir.id) %>%
      summarise(discharge.km3.year = sum(discharge.km3.month),
                discharge.km3.year.corrected = sum(discharge.km3.month.corrected)) %>% 
    mutate(hemisphere = reservoir.hemisphere)
  
  discharge.corrected.monthly.list[[i]] <- ts.corrected.monthly
  discharge.corrected.yearly.list[[i]] <- ts.corrected.yearly
  
}

discharge.corrected.monthly.single <- do.call(rbind, discharge.corrected.monthly.list)
discharge.corrected.yearly.single <- do.call(rbind, discharge.corrected.yearly.list)

max.discharge.df.one <- do.call(rbind, max.discharge.list)



####  correct discharge of transfers with more than one reservoir ####
discharge.corrected.monthly.list <- list()
discharge.corrected.yearly.list <- list()

n.transfers <- unique(ibwt.reservoir.mult$transfer.name)
n.transfers.sel <- discharge.data.reported %>%
  filter(transfer.name %in% n.transfers)

max.discharge.list <- list()

for(i in seq(nrow(n.transfers.sel))){
  
  #get transfer basic information
  transfer.info <- n.transfers.sel[i,]
  transfer.sel.name <- transfer.info$transfer.name
  transfer.design.cap <- transfer.info$design.cap.km3.y
  
  #get unique id of reservoirs (some may repeat in different transfers)
  transfer.sections <- data.sections %>% 
    filter(transfer.name == transfer.sel.name)
  
  transfer.initial.year <- unique(
    data.sections$initial.year.transfer[data.sections$transfer.name == transfer.sel.name])
  
  
  #reservoir ids 
  reservoir.sel <- unique(data.sections$reservoir.id[data.sections$transfer.name == transfer.sel.name])

  
  #get raw discharge from pcrglobwb
  #monthly
  transfer.discharge.monthly <- ts.raw.monthly %>%
    filter(year(datetime) >= transfer.initial.year) %>% 
    mutate(datetime = as.Date(datetime)) %>%
    filter(reservoir.id %in% reservoir.sel) 
  
  #yearly
  transfer.discharge.yearly <- ts.raw.yearly %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(reservoir.id %in% reservoir.sel) 
  
  #calculate total discharge
  #monthly
  transfer.discharge.tot.monthly <- transfer.discharge.monthly %>% 
    group_by(datetime) %>% 
    summarise(discharge.m3.month =  sum(discharge.m3.month),
              discharge.km3.month = sum(discharge.km3.month))
  
  #yearly
  transfer.discharge.tot.yearly <- transfer.discharge.yearly %>% 
    group_by(datetime) %>% 
    summarise(discharge.m3.y = sum(discharge.m3.y),
              discharge.km3.y = sum(discharge.km3.y))
  
  #get maximum discharge from all reservoirs
  transfer.max.raw.km3.y <- max(transfer.discharge.tot.yearly$discharge.km3.y)
  
  max.discharge.df <- data.frame(transfer.sel.name, transfer.max.raw.km3.y)
  colnames(max.discharge.df) <- c('transfer.name','max.modelled.km3.y')
  
  max.discharge.list[[i]] <- max.discharge.df
  
  #get bias correction factor, based on yearly time series
  bias.mult.factor <-
    transfer.design.cap / transfer.max.raw.km3.y

  #apply correction to monthly
  ts.corrected.monthly <- transfer.discharge.tot.monthly %>%
    mutate(discharge.m3.month.corrected = discharge.m3.month * bias.mult.factor,
           discharge.km3.month.corrected = discharge.km3.month * bias.mult.factor,
           transfer.name = transfer.sel.name) %>%
    # relocate(transfer.name, .before=reservoir.id) %>% 
    mutate(datetime = as.Date(datetime)) 
  
  #upscale to yearly
  ts.corrected.yearly <- ts.corrected.monthly %>%
    mutate(datetime = floor_date(datetime,"year")) %>%
    group_by(datetime, transfer.name) %>%
    summarise(discharge.km3.year = sum(discharge.km3.month),
              discharge.km3.year.corrected = sum(discharge.km3.month.corrected))
  
  
  #### now corrected each reservoir separately based on the monthly or yearly contribution to the total of the transfer
  transfer.corrected.list.monthly <- list()
  transfer.corrected.list.yearly <- list()
  
  for(j in seq(length(reservoir.sel))){
    
    reservoir.id.sel <- reservoir.sel[j]
    reservoir.lat <- data.sections$lat[data.sections$reservoir.id == reservoir.id.sel]
    
    if(unique(reservoir.lat) > 0){reservoir.hemisphere = 'North'}
    else{reservoir.hemisphere = 'South'}
    
    #apply correction to monthly
    ts.reservoir.monthly <- transfer.discharge.monthly %>% 
      filter(reservoir.id == reservoir.sel[j]) %>% 
      mutate(transfer.total.km3.month.raw = ts.corrected.monthly$discharge.km3.month,
             transfer.total.km3.month.corrected = ts.corrected.monthly$discharge.km3.month.corrected) %>% 
      mutate(transfer.ratio = discharge.km3.month / transfer.total.km3.month.raw) %>% 
      mutate(discharge.km3.month.corrected = transfer.total.km3.month.corrected * transfer.ratio)  %>% 
      mutate(discharge.m3.month.corrected = discharge.km3.month.corrected * 10^9) %>% 
      mutate(section.id.unique = min(transfer.sections$section.id.unique[transfer.sections$reservoir.id == reservoir.sel[j]])) %>% 
      mutate(transfer.name = transfer.sel.name,
             hemisphere = reservoir.hemisphere)
    
    #upscale to yearly
    ts.reservoir.yearly <- ts.reservoir.monthly %>%
      mutate(datetime = floor_date(datetime,"year")) %>%
      group_by(datetime, country, transfer.name, section.id.unique, reservoir.id) %>% 
      summarise(discharge.km3.year = sum(discharge.km3.month),
                discharge.km3.year.corrected = sum(discharge.km3.month.corrected)) %>% 
      mutate(hemisphere = reservoir.hemisphere)
    
    transfer.corrected.list.monthly[[j]] <- ts.reservoir.monthly
    transfer.corrected.list.yearly[[j]] <- ts.reservoir.yearly
    
  }
  
  
  transfer.corrected.monthly <- do.call(rbind, transfer.corrected.list.monthly)
  transfer.corrected.yearly <- do.call(rbind, transfer.corrected.list.yearly)
  
  discharge.corrected.monthly.list[[i]] <- transfer.corrected.monthly
  discharge.corrected.yearly.list[[i]] <- transfer.corrected.yearly
  
}


discharge.corrected.monthly.mult <- do.call(rbind, discharge.corrected.monthly.list)
discharge.corrected.yearly.mult <- do.call(rbind, discharge.corrected.yearly.list)

max.discharge.df.mult <- do.call(rbind, max.discharge.list)

#### gather corrected discharge from all over the place ####
discharge.corrected.all.monthly <- rbind(discharge.corrected.monthly.single %>% 
                                           select(datetime, country, transfer.name,
                                                  section.id.unique, reservoir.id,
                                                  month, days.in.month,
                                                  discharge.m3.s, discharge.m3.month, 
                                                  discharge.m3.month.corrected, 
                                                  discharge.km3.month, 
                                                  discharge.km3.month.corrected,
                                                  hemisphere),
                                         discharge.corrected.monthly.mult %>% 
                                           select(datetime, country, transfer.name,
                                                  section.id.unique, reservoir.id,
                                                  month, days.in.month,
                                                  discharge.m3.s, discharge.m3.month, 
                                                  discharge.m3.month.corrected,discharge.km3.month,
                                                  discharge.km3.month.corrected,
                                                  hemisphere)) %>% 
  arrange(section.id.unique) %>% 
  filter(!reservoir.id %in% reservoirs.no.discharge$id.geodar.v11)


discharge.corrected.all.yearly <- rbind(discharge.corrected.yearly.single, 
                                        discharge.corrected.yearly.mult) %>% 
  arrange(section.id.unique) %>% 
  filter(!reservoir.id %in% reservoirs.no.discharge$id.geodar.v11)

# ids of corrected reservoirs
corrected.reservoirs <- data.frame(unique(discharge.corrected.all.monthly$reservoir.id))
colnames(corrected.reservoirs) <- 'reservoir.id'

# filter segments with corrected reservoirs 
corrected.segments <- data.segments %>% 
  filter(reservoir.id %in% corrected.reservoirs$reservoir.id) %>% 
  # #add section id to segments
  inner_join(data.sections %>% 
               select(section.id.unique, transfer.name, reservoir.id, Section) %>% 
               rename(section.id = Section))

#dataframe with maximum modelled discharge
max.discharge.df.all <- rbind(max.discharge.df.one,
                              max.discharge.df.mult)


#### save corrected discharge ####
write.csv(discharge.corrected.all.monthly,
          paste0(outputDirBias, '1_discharge_bias_corrected_monthly.csv'),
          row.names = F)

write.csv(discharge.corrected.all.yearly,
          paste0(outputDirBias, '1_discharge_bias_corrected_yearly.csv'),
          row.names = F)

#save reservoir ids of bias-corrected reservoirs
write.csv(corrected.reservoirs, 
          paste0(outputDirBias, '2_corrected_sections.csv'), row.names = F)

write.csv(corrected.segments,
          paste0(outputDirBias, '2_corrected_segments.csv'), row.names = F)
