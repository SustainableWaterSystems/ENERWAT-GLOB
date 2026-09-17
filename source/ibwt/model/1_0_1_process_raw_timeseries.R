#this script cuts the timeseries depending on the inital year of operation 
#of each ibwt

rm(list=ls())

library(dplyr)
library(lubridate)
library(stringr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


outputDir <- '../../../output/ibwt/model/'

inputDirElevation <- paste0(outputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(outputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/0_raw/')

outputDirFiltered <- paste0(outputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/')
dir.create(outputDirFiltered, recursive = T, showWarnings = F)


#### ####
data.sections <- read.csv(paste0(inputDirElevation, '0_0_sections_reservoirs.csv'))

data.sections.filtered <- data.sections %>% 
  filter(Country != 'Libya')

reservoirs.ids <- unique(data.sections.filtered$reservoir_ID)

# timeseries.raw.list <- list.files(inputDirDischarge)

ts.filtered.list <- list()
res.no.discharge <- list()

# for(i in seq(length(timeseries.raw.list))){
for(i in seq(length(reservoirs.ids))){
  
  #get string of reservoir 
  # reservoir.id <- str_match(timeseries.raw.list[i], "outlet_\\s*(.*?)\\s*.csv")[2]
  reservoir.id <- reservoirs.ids[i]
  
  #read timeseries of reservoir
  # ts.raw.sel <- read.csv(paste0(inputDirDischarge, timeseries.raw.list[i]))
  ts.raw.sel <- read.csv(paste0(inputDirDischarge, 'm3_s_outlet_', reservoir.id , '.csv'))
  
  #get initial year of operation of ibwt
  initial.year <- data.sections$initial.year[which(data.sections$reservoir_ID == reservoir.id)]
  
  country.sel <- unique(
    data.sections$Country[which(data.sections$reservoir_ID == reservoir.id)])
  
  if(length(initial.year) > 1){
    
    initial.year <- min(initial.year)
    
  }
  
  #filter timeseries
  ts.filtered <- ts.raw.sel %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    filter(year(datetime) >= initial.year) 
  
  #calculate monthly discharge
  ts.monthly <- ts.filtered %>% 
    mutate(month = month(datetime),
           days.in.month = days_in_month(datetime)) %>% 
    rename(discharge.m3.s = discharge) %>% 
    mutate(discharge.m3.month = discharge.m3.s * 60 * 60 * 24 * days.in.month) %>% 
    mutate(discharge.km3.month = discharge.m3.month / 1000000000) %>% 
    relocate(discharge.m3.s, .before=discharge.m3.month) %>% 
    mutate(country = country.sel,
           reservoir.id = reservoir.id) %>% 
    relocate(reservoir.id, .before=month) %>% 
    relocate(country, .before=reservoir.id)
  
  # cut discharge to 1980-2021
  ts.monthly.cut <- ts.monthly %>% 
    filter(year(datetime) >= 1980 & year(datetime) <= 2021)
  
  # #save filtered timeseries
  # write.csv(ts.monthly.cut, paste0(outputDirFiltered, timeseries.raw.list[i]), row.names = F)
  
  ts.filtered.list[[i]] <- ts.monthly.cut
  
  if(sum(ts.monthly.cut$discharge.km3.month) == 0){
    
    id.nodis <- unique(ts.monthly.cut$reservoir.id)
    
    res.no.discharge[[i]] <- id.nodis
    
    print(unique(ts.monthly.cut$reservoir.id))}
  
}

#explode monthly discharge list
ts.filtered.all.monthly <- do.call(rbind, ts.filtered.list) %>% 
  arrange(reservoir.id)

#group by reservoir to get yearly discharges
ts.filtered.all.yearly <- ts.filtered.all.monthly %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(datetime = floor_date(datetime,"year")) %>% 
  group_by(datetime, country, reservoir.id) %>% 
  summarise(discharge.m3.y = sum(discharge.m3.month),
            discharge.km3.y = sum(discharge.km3.month)) %>% 
  arrange(reservoir.id)
  
#get reservoirs with zero modelled discharge
res.no.discharge.df <- data.frame(do.call(rbind, res.no.discharge))
colnames(res.no.discharge.df) <- NULL
colnames(res.no.discharge.df) <- 'id.geodar.v11'

#save csvs
write.csv(ts.filtered.all.monthly, 
          paste0(outputDirFiltered, '1_discharge_raw_processed_monthly.csv'), row.names = F)
write.csv(ts.filtered.all.yearly, 
          paste0(outputDirFiltered, '1_discharge_raw_processed_yearly.csv'), row.names = F)

write.csv(res.no.discharge.df, 
          paste0(outputDirFiltered, '2_reservoirs_no_discharge.csv'), row.names = F)
