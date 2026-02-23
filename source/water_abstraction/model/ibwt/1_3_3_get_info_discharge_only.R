rm(list=ls())

library(dplyr)
# library(RANN)
library(geosphere)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
inputDirElevation <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/1_segments/'
inputDirHeads <- '../../../../input/global_data/Shah2025_GloHydroRes/'

outputDir <- paste0(inputDir, '2_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)

#### load 
data.sections <- read.csv(paste0(inputDir,'0_timeseries_raw/1_0_bias_correction_sections.csv'))
data.segments <- read.csv(paste0(inputDir,'0_timeseries_raw/1_1_bias_correction_segments.csv'))

discharge.intakes <- data.segments %>% 
  filter(intake.type == 'Reservoir intake' | intake.type == 'River intake')

write.csv(discharge.intakes, paste0(outputDir, '3_information_discharge_only.csv'), row.names = F)
