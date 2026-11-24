rm(list=ls())

library(dplyr)
library(lubridate)
library(hydroGOF)
library(ggplot2)

inputDir <- '../../../output/ibwt/model/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')

inputDirGlobal <- '../../../input/global_data/ibwt/Driscoll2024/'

#### load ###

#### modelled hydropower
p.stations <- read.csv(paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))

discharge.year <- read.csv(paste0(inputDirDischarge, '1_bias_corrected/1_discharge_bias_corrected_yearly.csv'))

pumping.year <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

pumping.month <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_monthly.csv'))%>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(
    year.continuous = (year(datetime)),
    year.factor = factor(year(datetime)),     # use year to define separate curves
    date = update(datetime, year = 1)  # use a constant year for the x-axis
  )


energy.driscoll <- read.csv(paste0(inputDirGlobal, 'validation_megaprojects.csv'), sep=';')

#### the truth ####
transfers.driscoll <- unique(energy.driscoll$IBT.Project)

discharge.driscoll <- discharge.year %>% 
  filter(transfer.name %in% transfers.driscoll) %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  group_by(transfer.name, datetime) %>% 
  summarise(discharge.km3.year = sum(discharge.km3.year),
            discharge.km3.year.corrected = sum(discharge.km3.year.corrected))

pumping.driscoll <- pumping.year %>% 
  filter(transfer.name %in% transfers.driscoll) %>% 
  group_by(datetime, transfer.name) %>% 
  summarise(energy.twh.low = sum(energy.kwh.low) / 10^9,
            energy.twh.mean = sum(energy.kwh.mean)/ 10^9,
            energy.twh.high = sum(energy.kwh.high)/ 10^9
            ) %>% 
  inner_join(discharge.driscoll)

energy.driscoll.format <- energy.driscoll %>% 
  select(Years, IBT.Project, Total.electricity.use..kWh., Total.deliveries..acre.feet..if.known.) %>% 
  rename(datetime = Years, 
         transfer.name = IBT.Project, 
         energy.kwh.reported = Total.electricity.use..kWh.,
         acrefeet.year = Total.deliveries..acre.feet..if.known.) %>% 
  mutate(datetime = as.Date(paste0(datetime, "0101"), format = "%Y%m%d")) %>% 
  mutate(km3.year = acrefeet.year * 0.00000123348) %>% 
  mutate(energy.twh.reported = energy.kwh.reported / 10^9) %>% 
  select(-energy.kwh.reported)

df.compare <- inner_join(pumping.driscoll, energy.driscoll.format)


write.csv(df.compare, 
          paste0(inputDirEnergy, '7_validation_driscoll_yearly.csv'), row.names = F)