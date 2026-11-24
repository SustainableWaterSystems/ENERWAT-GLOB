rm(list=ls())

library(dplyr)
library(lubridate)
library(hydroGOF)
library(readxl)
library(tidyr)

inputDirBias <- '../../../output/ibwt/model/1_discharge_timeseries/1_bias_corrected/'
inputDirGlobal <- '../../../input/global_data/ibwt/'

#### load
# geodar.df <- read.csv('../../../input/ibwt/1_discharge_timeseries/geodar_v11.csv')

discharge.monthly <- read.csv(paste0(inputDirBias, '1_discharge_bias_corrected_monthly.csv'))
discharge.yearly <- read.csv(paste0(inputDirBias, '1_discharge_bias_corrected_yearly.csv'))

# resopsus <- read.csv(paste0(inputDirGlobal, 'ResOpsUS/ResOpsData.csv'))
# grand.storage <- read.csv(paste0(inputDirGlobal, 'ResOpsUS/dichargeDataMichele.csv'))
discharge.data.reported <- read.csv(
  '../../../input/global_data/ibwt/ibwt_mp_volumes_reported.csv') 

siddik.magni <- read.csv(paste0(inputDirGlobal, 'Siddik2023-us-canada/Siddik_merge_ibwt_filt.csv')) 

info.sections <- read.csv(paste0(inputDirBias, '2_corrected_sections.csv'))
info.segments <- read.csv(paste0(inputDirBias, '2_corrected_segments.csv'))

####  ####
# siddik.magni.filtered <- siddik.magni %>% 
#   filter(Time_serie == 'Available') %>% 
#   filter(Subject == 'Reservoir intake' | Subject == 'Power station' | Subject == 'Pumping station') %>% 
#   rename(segment.full = Segment,
#          transfer.name = layer) %>% 
#   inner_join(info.segments %>% 
#                select(transfer.name, segment.full, section.id.unique))

siddik.ids <- unique(siddik.magni$Link_ID)



df.compare.yearly.list <- list()
df.compare.monthly.list <- list()

reviewer.2.list <- list()

#### do timeseries one by one ####
for(i in seq(nrow(siddik.magni))){
  
  
  #modelled timeseries
  reservoir.id.un <- siddik.magni$section.id.unique[i]
  
  ts.model.yearly <- discharge.yearly %>%
    filter(section.id.unique == reservoir.id.un) %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    rename(discharge.model.original = discharge.km3.year,
           discharge.model.corrected = discharge.km3.year.corrected) 
  
  ts.model.monthly <- discharge.monthly %>%
    filter(section.id.unique == reservoir.id.un) %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    rename(discharge.model.original = discharge.km3.month,
           discharge.model.corrected = discharge.km3.month.corrected) %>% 
    select(datetime,country,transfer.name,section.id.unique,
           discharge.model.original, discharge.model.corrected)
  
  #siddik timeseries
  state.id <- siddik.magni$State_Prov[i]
  
  id.siddik.ts <- siddik.magni$Link_ID[i]
  
  ts.siddik <- read_excel(
    paste0(inputDirGlobal, 'Siddik2023-us-canada/timeseries-extracted.xlsx'), sheet = id.siddik.ts)
  
  colnames(ts.siddik) <- c('datetime','discharge.m3.d')
  
  ts.siddik.monthly <- ts.siddik %>%
    mutate(datetime = floor_date(datetime, 'month')) %>%
    group_by(datetime) %>%
    summarise(discharge.km3.month = sum(discharge.m3.d) / 10^9) %>% 
    rename(discharge.siddik = discharge.km3.month)
  
  ts.siddik.yearly <- ts.siddik %>%
    mutate(datetime = floor_date(datetime, 'year')) %>%
    group_by(datetime) %>%
    summarise(discharge.km3.y = sum(discharge.m3.d) / 10^9) %>% 
    rename(discharge.siddik = discharge.km3.y)

  
  #compare with model
  df.compare.yearly <- inner_join(ts.siddik.yearly, ts.model.yearly)
  df.compare.monthly <- inner_join(ts.siddik.monthly, ts.model.monthly)
  
  df.compare.yearly.list[[i]] <- df.compare.yearly
  df.compare.monthly.list[[i]] <- df.compare.monthly
  
  #### MORE INFORMATION for reviewer 2
  
  year.first <- df.compare.yearly$datetime[1]
  year.last <- df.compare.yearly$datetime[nrow(df.compare.yearly)]
  n.months <- nrow(df.compare.monthly)
  n.years <- nrow(df.compare.yearly)
  
  nrmse.og.yearly <- 
    sqrt(mean((df.compare.yearly$discharge.siddik - df.compare.yearly$discharge.model.original)^2, 
              na.rm = TRUE)) / mean(df.compare.yearly$discharge.siddik)
  
  nrmse.corrected.yearly <- 
    sqrt(mean((df.compare.yearly$discharge.siddik - df.compare.yearly$discharge.model.corrected)^2, 
              na.rm = TRUE)) / mean(df.compare.yearly$discharge.siddik)
  
  nrmse.og.monthly <- 
    sqrt(mean((df.compare.monthly$discharge.siddik - df.compare.monthly$discharge.model.original)^2, 
              na.rm = TRUE)) / mean(df.compare.monthly$discharge.siddik)
  
  nrmse.corrected.monthly <- 
    sqrt(mean((df.compare.monthly$discharge.siddik - df.compare.monthly$discharge.model.corrected)^2, 
              na.rm = TRUE)) / mean(df.compare.monthly$discharge.siddik)
  
  
  kge.og.month <- KGEnp(df.compare.monthly$discharge.siddik, 
                         df.compare.monthly$discharge.model.original, na.rm = T)
  kge.corrected.month <- KGEnp(df.compare.monthly$discharge.siddik, 
                          df.compare.monthly$discharge.model.corrected, na.rm = T)
  
  kge.og.year <- KGEnp(df.compare.yearly$discharge.siddik, 
                        df.compare.yearly$discharge.model.original, na.rm = T)
  kge.corrected.year <- KGEnp(df.compare.yearly$discharge.siddik, 
                         df.compare.yearly$discharge.model.corrected, na.rm = T)
  
  mean.yearly.obs <- mean(df.compare.yearly$discharge.siddik)
  mean.monthly.obs <- mean(df.compare.monthly$discharge.siddik)
  
  basic.info.df <- data.frame(
    id.siddik.ts,
    year.first,
    year.last,
    n.months,
    n.years,
    mean.yearly.obs,
    mean.monthly.obs,
    kge.og.month,
    kge.corrected.month,
    nrmse.og.monthly,
    nrmse.corrected.monthly,
    kge.og.year,
    kge.corrected.year,
    nrmse.og.yearly,
    nrmse.corrected.yearly

    
  )
  
  reviewer.2.list[[i]] <- basic.info.df

}

df.compare.all.yearly <- do.call(rbind, df.compare.yearly.list)
df.compare.all.monthly <- do.call(rbind, df.compare.monthly.list)

reviewer.2.df <- do.call(rbind, reviewer.2.list) %>% 
  inner_join(siddik.magni %>% 
               select(Link_ID, transfer.name) %>% 
               rename(id.siddik.ts = Link_ID))

write.csv(df.compare.all.monthly, 
          paste0(inputDirBias, '3_validation_siddik_monthly.csv'), row.names = F)
write.csv(df.compare.all.yearly, 
          paste0(inputDirBias, '3_validation_siddik_yearly.csv'), row.names = F)

#### check maximum discharge for each transfer ####
discharge.ibwt.check <- discharge.yearly %>% 
  group_by(transfer.name, datetime) %>% 
  summarise(discharge.km3.year = sum(discharge.km3.year),
            discharge.km3.year.corrected = sum(discharge.km3.year.corrected))

discharge.ibwt.max <- discharge.ibwt.check %>% 
  group_by(transfer.name) %>% 
  summarise(discharge.km.y.max = max(discharge.km3.year),
            discharge.km.y.max.corrected = max(discharge.km3.year.corrected)) %>% 
  inner_join(discharge.data.reported %>% 
               select(transfer.name, design.cap.km3.y))
