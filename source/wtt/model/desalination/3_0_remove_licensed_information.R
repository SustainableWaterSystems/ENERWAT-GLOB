rm(list=ls())

library(dplyr)

inputDirInputs <- '../../../../input/wtt/desalination/'

inputDirOutputs <- '../../../../output/wtt/model/desalination/'

outputDir <- '../../../../output/wtt/model/desalination/3_IO_edited_for_publication/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### inputs ####
input1 <- read.csv(paste0(inputDirInputs,'DesalData_2019.csv'))
input2 <- read.csv(paste0(inputDirInputs,'DesalData_2019_ID.csv'))
input3 <- read.csv(paste0(inputDirInputs,'DesalData_2019_ID_renamed_countries.csv'))

input1.edited <- input1 %>% 
  select(Country, Region, Subregion)
input2.edited <- input2 %>% 
  select(PLANT_ID, Country, Region, Subregion)
input3.edited <- input3 %>% 
  select(PLANT_ID, Country, Region, Subregion)

#### outputs ####
output1 <- read.csv(paste0(inputDirOutputs, '0_tech_categories/DesalDataEnergy_online_2019.csv'))
output2 <- read.csv(paste0(inputDirOutputs, '2_energy/energy_plants_2015_desal.csv'))

output1.edited <- output1 %>% 
  select(PLANT_ID, Country, Region, Subregion, 
         kwh.d.low, kwh.d.mean, kwh.d.high)

output2.edited <- output2 %>% 
  select(PLANT_ID, Country, Region, Subregion, 
         kwh.d.low, kwh.d.mean, kwh.d.high,
         kwh.y.low, kwh.y.mean, kwh.y.high,
         twh.y.low, twh.y.mean, twh.y.high,
         ej.y.low, ej.y.mean, ej.y.high)


#### save edited outputs ####
write.csv(input1.edited, paste0(outputDir, 'DesalData_2019.csv'), row.names = F)
write.csv(input2.edited, paste0(outputDir, 'DesalData_2019_ID.csv'), row.names = F)
write.csv(input3.edited, paste0(outputDir, 'DesalData_2019_ID_renamed_countries.csv'), row.names = F)

write.csv(output1.edited, paste0(outputDir, 'DesalDataEnergy_online_2019.csv'), row.names = F)
write.csv(output2.edited, paste0(outputDir, 'energy_plants_2015_desal.csv'), row.names = F)