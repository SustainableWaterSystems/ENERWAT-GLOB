rm(list=ls())

library(dplyr)

#### directories ####
inputDir <- '../../../output/ibwt/model/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(inputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

####
intake.info <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/3_information_discharge_only.csv'))

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_0_discharge_bias_corrected_monthly.csv'))
discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_1_discharge_bias_corrected_yearly.csv'))


discharge.month.list <- list()
discharge.year.list <- list()


#### process ####
discharge.only.monthly <- discharge.monthly.df %>% 
  filter(section.id.unique %in% intake.info$section.id.unique)

discharge.only.yearly <- discharge.yearly.df %>% 
  filter(section.id.unique %in% intake.info$section.id.unique)

#### save ####
write.csv(discharge.only.monthly, 
          paste0(outputDirEnergy, '3_discharge_only_monthly.csv'), 
          row.names = F)

write.csv(discharge.only.yearly, 
          paste0(outputDirEnergy, '3_discharge_only_yearly.csv'), 
          row.names = F)