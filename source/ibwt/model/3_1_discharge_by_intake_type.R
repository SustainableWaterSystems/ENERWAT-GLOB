rm(list=ls())

library(dplyr)

#### directories ####
inputDir <- '../../../output/ibwt/model/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(inputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
#### intakes

info.hydropower <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/0_information_hydropower.csv'))
info.pumping <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))
info.discharge.only <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/3_information_discharge_only.csv'))

#### timeseries
discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_1_discharge_bias_corrected_yearly.csv'))

energy.production.yearly <- read.csv(paste0(inputDir, '2_energy/0_energy_production_yearly.csv'))
energy.consumption.yearly <- read.csv(paste0(inputDir, '2_energy/1_energy_consumption_yearly.csv'))
discharge.only <- read.csv(paste0(inputDir, '2_energy/3_discharge_only_yearly.csv'))

#### calculate how much discharge goes through each type of intake
reservoirs.all <- unique(discharge.yearly.df$section.id.unique) 

reservoirs.pumping <- unique(info.pumping$section.id.unique)
reservoirs.production <- unique(info.hydropower$section.id.unique)
reservoirs.discharge.only <- unique(info.discharge.only$section.id.unique)

discharge.consumption <- discharge.yearly.df %>% 
  filter(section.id.unique %in% reservoirs.pumping)
discharge.production <- discharge.yearly.df %>% 
  filter(section.id.unique %in% reservoirs.production)
discharge.discharge.only <- discharge.yearly.df %>% 
  filter(section.id.unique %in% reservoirs.discharge.only)

#### make yearly summaries (total) ####
discharge.summarise.total <- discharge.yearly.df %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.total = discharge.km3.y.corrected)

discharge.summarise.consumption <- discharge.consumption %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.consumption = discharge.km3.y.corrected)

discharge.summarise.production <- discharge.production %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.production = discharge.km3.y.corrected)

discharge.summarise.discharge.only <- discharge.discharge.only %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.discharge.only = discharge.km3.y.corrected)


discharge.percs <- inner_join(discharge.summarise.total, 
                              discharge.summarise.consumption) %>% 
  inner_join(., discharge.summarise.production) %>% 
  inner_join(., discharge.summarise.discharge.only) %>% 
  mutate(ratio.c = km3.consumption / km3.total * 100,
         ratio.p = km3.production / km3.total * 100,
         ratio.do = km3.discharge.only / km3.total * 100,
         )

#### by country ####
discharge.country.total <- discharge.yearly.df %>% 
  group_by(datetime, country) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.total = discharge.km3.y.corrected)

discharge.country.consumption <- discharge.consumption %>% 
  group_by(datetime, country) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.consumption = discharge.km3.y.corrected)

discharge.country.production <- discharge.production %>% 
  group_by(datetime, country) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.production = discharge.km3.y.corrected)

discharge.country.discharge.only <- discharge.discharge.only %>% 
  group_by(datetime, country) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>% 
  rename(km3.discharge.only = discharge.km3.y.corrected)

discharge.percs.country <- merge(discharge.country.total, 
                                 discharge.country.consumption, 
                                 all=T) %>% 
  merge(., discharge.country.production, all = T) %>% 
  merge(., discharge.country.discharge.only, all = T) %>% 
  mutate(ratio.c = km3.consumption / km3.total * 100,
         ratio.p = km3.production / km3.total * 100,
         ratio.do = km3.discharge.only / km3.total * 100,
  )
  
#### save ####
write.csv(discharge.percs, 
          paste0(outputDirEnergy, 
                 '5_0_discharge_by_intake_type_timeseries.csv'), 
          row.names = F) 

write.csv(discharge.percs.country, 
          paste0(outputDirEnergy, 
                 '5_1_discharge_by_intake_type_timeseries_country.csv'), 
          row.names = F)
