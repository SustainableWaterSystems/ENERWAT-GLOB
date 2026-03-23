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


outputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(outputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(outputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/0_raw/')

outputDirFiltered <- paste0(outputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/1_filtered/')
dir.create(outputDirFiltered, recursive = T, showWarnings = F)


#### ####
data.sections <- read.csv(paste0(inputDirElevation, '0_0_sections_reservoirs.csv'))

timeseries.raw.list <- list.files(inputDirDischarge)

for(i in seq(length(timeseries.raw.list))){
  
  #get string of reservoir 
  reservoir.id <- str_match(timeseries.raw.list[i], "outlet_\\s*(.*?)\\s*.csv")[2]
  
  #read timeseries of reservoir
  ts.raw.sel <- read.csv(paste0(inputDirDischarge, timeseries.raw.list[i]))
  
  #get initial year of operation of ibwt
  initial.year <- data.sections$initial.year[which(data.sections$reservoir_ID == reservoir.id)]
  
  #filter timeseries
  ts.filtered <- ts.raw.sel %>% 
    filter(year(datetime) >= initial.year)
  
  #save filtered timeseries
  write.csv(ts.filtered, paste0(outputDirFiltered, timeseries.raw.list[i]), row.names = F)
  
}