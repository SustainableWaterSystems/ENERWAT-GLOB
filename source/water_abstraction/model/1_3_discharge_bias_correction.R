rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirData <- paste0(inputDirDischarge, '1_bias_corrected/')
dir.create(outputDirData, recursive = T, showWarnings = F)

#### load ####
correction.factors <- read.csv(
  paste0(inputDirDischarge, '0_timeseries_raw/1_bias_correction_sections.csv'))

#### process ####
# #### get reservoirs that are associated to multiple sections
# n_occur <- data.frame(table(correction.factors$reservoir.id))
# 
# reservoirs.multiple <- correction.factors[correction.factors$reservoir.id %in% n_occur$Var1[n_occur$Freq > 1],]
# 
# #reservoirs that only appear once
# reservoirs.singular <- setdiff(correction.factors, reservoirs.multiple)

####
discharge.monthly.list <- list()
discharge.yearly.list <- list()

for(i in seq(nrow(correction.factors))){
  
  section.data <- correction.factors[i,]
  
  reservoir.select <- section.data$reservoir.id
  
  initial.year.transfer <- section.data$initial.year.transfer
  
  bias.correction.factor <- section.data$bias.mult.factor
  
  #read raw timeseries of reservoir 
  discharge.monthly.raw <- read.csv(paste0(
    inputDirDischarge, '0_timeseries_raw/timeseries/m3_s_outlet_',
    reservoir.select, '.csv'))
  
  discharge.monthly.process <- discharge.monthly.raw %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    #cut dates before initial date of water transfer
    filter(datetime > initial.year.transfer) %>% 
    mutate(month = month(datetime),
           days.in.month = days_in_month(datetime)) %>% 
    rename(discharge.m3.s = discharge) %>% 
    mutate(discharge.m3.s.corrected = discharge.m3.s * bias.correction.factor) %>% 
    mutate(discharge.m3.month = discharge.m3.s.corrected * 60 * 60 * 24 * days.in.month) %>% 
    mutate(section.id.unique = section.data$section.id.unique,
           country = section.data$country,
           transfer.name = section.data$transfer.name,
           section.id = section.data$section.id,
           reservoir.id = section.data$reservoir.id,
           lon = section.data$lon,
           lat = section.data$lat,
           hemisphere = section.data$hemisphere) 

  discharge.yearly.process <- discharge.monthly.process %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    mutate(datetime = floor_date(datetime, 'year')) %>% 
    group_by(datetime) %>% 
    summarise(discharge.m3.y.corrected = sum(discharge.m3.month)) %>% 
    mutate(discharge.km3.y.corrected = discharge.m3.y.corrected / 1000000000) %>% 
    mutate(section.id.unique = section.data$section.id.unique,,
           country = section.data$country,
           transfer.name = section.data$transfer.name,
           section.id = section.data$section.id,
           reservoir.id = section.data$reservoir.id,
           lon = section.data$lon,
           lat = section.data$lat,
           hemisphere = section.data$hemisphere) 
    
  discharge.monthly.list[[i]] <- discharge.monthly.process
  discharge.yearly.list[[i]] <- discharge.yearly.process
  
}

discharge.monthly.df <- do.call(rbind, discharge.monthly.list) 
discharge.yearly.df <- do.call(rbind, discharge.yearly.list)


####save####
write.csv(discharge.monthly.df, paste0(outputDirData, '0_0_discharge_bias_corrected_monthly.csv'), row.names = F)
write.csv(discharge.yearly.df, paste0(outputDirData, '0_1_discharge_bias_corrected_yearly.csv'), row.names = F)
