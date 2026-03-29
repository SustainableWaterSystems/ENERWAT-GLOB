rm(list=ls())

library(dplyr)

inputDir   <- '../../../../output/wtt/model/desalination/0_tech_categories/'
outputDir <- '../../../../output/wtt/model/desalination/2_energy/'

dir.create(outputDir, recursive = T, showWarnings = F)

#### load dataframes ####
DesalData.2019 <- read.csv(paste0(inputDir, 'DesalDataEnergy_online_2019.csv'))
  
plants.online.energy <- DesalData.2019 %>% 
  filter(Online.date <= as.Date(paste(2015, 1, 1, sep = "-")))
  
  
#### make kwh/d -> kwh/y ####
plants.online.energy$kwh.y.low <- NA
plants.online.energy$kwh.y.mean <- NA
plants.online.energy$kwh.y.high <- NA

plants.online.energy$kwh.y.low <- plants.online.energy$kwh.d.low * 365
plants.online.energy$kwh.y.mean <- plants.online.energy$kwh.d.mean * 365
plants.online.energy$kwh.y.high <- plants.online.energy$kwh.d.high * 365

plants.online.energy$twh.y.low <- plants.online.energy$kwh.y.low / (10^9)
plants.online.energy$twh.y.mean <- plants.online.energy$kwh.y.mean / (10^9)
plants.online.energy$twh.y.high <- plants.online.energy$kwh.y.high / (10^9)

plants.online.energy$ej.y.low <- plants.online.energy$twh.y.low * 3.6 / 10^3
plants.online.energy$ej.y.mean <- plants.online.energy$twh.y.mean * 3.6 / 10^3
plants.online.energy$ej.y.high <- plants.online.energy$twh.y.high * 3.6 / 10^3

write.csv(plants.online.energy, paste0(outputDir, 'energy_plants_2015_desal.csv'), 
          row.names = F)
