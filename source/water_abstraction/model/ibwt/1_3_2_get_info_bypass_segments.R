rm(list=ls())

#pumping lift height is calculated as 
#elevation of highest point on the transfer path 
#minus
#elevation of intake point

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
inputDirElevation <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'
outputDir <- paste0(inputDir, '2_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)

#### load 
data.sections <- read.csv(paste0(inputDir,'0_timeseries_raw/1_0_bias_correction_sections.csv'))
data.segments <- read.csv(paste0(inputDir,'0_timeseries_raw/1_1_bias_correction_segments.csv'))
data.bypass <- read.csv(paste0(inputDirElevation, '3_information/2_information_bypass.csv'))


#### process ####
bypass.filtered.list <- list()

for(i in seq(nrow(data.bypass))){
  
  bypass.select <- data.bypass[i,]
  
  data.segments.filter <- data.segments %>% 
    filter(transfer.name == bypass.select$transfer.name) %>% 
    filter(segment.full == bypass.select$segment.id)
  
  bypass.select.info <- bypass.select %>% 
    mutate(segment.id.unique = data.segments.filter$segment.id.unique,
           section.id.unique = data.segments.filter$section.id.unique,
           hemisphere = data.segments.filter$hemisphere
           )
  
  bypass.filtered.list[[i]] <- bypass.select.info
  
  
}

bypass.filtered.df <- do.call(rbind, bypass.filtered.list)

write.csv(bypass.filtered.df, paste0(outputDir, '2_information_bypass.csv'), row.names = F)