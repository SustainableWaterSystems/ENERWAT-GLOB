rm(list=ls())

library(dplyr)
library(tidyr)

#### directories ####
inputDir <- '../../../output/ibwt/model/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
outputDirEnergy <- paste0(inputDir, '2_energy/')

#### load ####

energy.production.monthly <- read.csv(paste0(inputDir, '2_energy/0_energy_production_monthly.csv'))
energy.consumption.monthly <- read.csv(paste0(inputDir, '2_energy/1_energy_consumption_monthly.csv'))
energy.savings.monthly <- read.csv(paste0(inputDir, '2_energy/2_energy_savings_monthly.csv'))

energy.production.yearly <- read.csv(paste0(inputDir, '2_energy/0_energy_production_yearly.csv'))
energy.consumption.yearly <- read.csv(paste0(inputDir, '2_energy/1_energy_consumption_yearly.csv'))
energy.savings.yearly <- read.csv(paste0(inputDir, '2_energy/2_energy_savings_yearly.csv'))

#### process ####
energy.consumption.summarise.month <- energy.consumption.monthly %>% 
  group_by(datetime, transfer.name, country) %>% 
  summarise(discharge.km3.month.c = mean(discharge.km3.month),
            twh.low.c = sum(energy.twh.low),
            twh.mean.c = sum(energy.twh.mean),
            twh.high.c = sum(energy.twh.high)
  )

energy.production.summarise.month <- energy.production.monthly %>% 
  group_by(datetime, transfer.name, country) %>% 
  summarise(discharge.km3.month.p = mean(discharge.km3.month),
            twh.low.p = sum(energy.twh.low),
            twh.mean.p = sum(energy.twh.mean),
            twh.high.p = sum(energy.twh.high)
  )

energy.consumption.summarise.year <- energy.consumption.yearly %>% 
  group_by(datetime, transfer.name, country) %>% 
  # summarise(discharge.km3.y.c = mean(discharge.km3.y.corrected),
  summarise(
            twh.low.c = sum(energy.twh.low),
            twh.mean.c = sum(energy.twh.mean),
            twh.high.c = sum(energy.twh.high)
            )

energy.production.summarise.year <- energy.production.yearly %>% 
  group_by(datetime, transfer.name, country) %>% 
  # summarise(discharge.km3.y.p = mean(discharge.km3.y.corrected),
  summarise(
            twh.low.p = sum(energy.twh.low),
            twh.mean.p = sum(energy.twh.mean),
            twh.high.p = sum(energy.twh.high)
  )


#### transfers that have both consumption and production
energy.budget.month <- inner_join(energy.consumption.summarise.month, energy.production.summarise.month) %>% 
  mutate(twh.low.net = twh.low.p - twh.low.c,
         twh.mean.net = twh.mean.p - twh.mean.c,
         twh.high.net = twh.high.p - twh.high.c,
         ) %>% 
  mutate(ratio.low = twh.low.p / twh.low.net * 100,
          ratio.mean = twh.mean.p / twh.mean.net * 100,
          ratio.high = twh.high.p / twh.high.net * 100
          ) %>% 
  mutate(transfer.type = 'Energy mixed')

energy.budget.year <- inner_join(energy.consumption.summarise.year, energy.production.summarise.year) %>% 
  mutate(twh.low.net = twh.low.p - twh.low.c,
         twh.mean.net = twh.mean.p - twh.mean.c,
         twh.high.net = twh.high.p - twh.high.c,
  ) %>% 
  mutate(ratio.low = twh.low.p / twh.low.net * 100,
         ratio.mean = twh.mean.p / twh.mean.net * 100,
         ratio.high = twh.high.p / twh.high.net * 100
  ) %>% 
  mutate(transfer.type = 'Energy mixed')

transfers.both <- unique(energy.budget.year$transfer.name)

#### only production
transfers.production.month <- energy.production.summarise.month %>%
  filter(!transfer.name %in% transfers.both) %>%
  mutate(twh.low.net = twh.low.p,
         twh.mean.net = twh.mean.p,
         twh.high.net = twh.high.p,
  ) %>%
  mutate(ratio.low = twh.low.net / twh.low.net * 100,
         ratio.mean = twh.mean.net / twh.mean.net * 100,
         ratio.high = twh.high.net / twh.high.net * 100
  ) %>%
  mutate(transfer.type = 'Energy producer')
transfers.production.year <- energy.production.summarise.year %>%
  filter(!transfer.name %in% transfers.both) %>%
  mutate(twh.low.net = twh.low.p,
         twh.mean.net = twh.mean.p,
         twh.high.net = twh.high.p,
  ) %>%
  mutate(ratio.low = twh.low.net / twh.low.net * 100,
         ratio.mean = twh.mean.net / twh.mean.net * 100,
         ratio.high = twh.high.net / twh.high.net * 100
  ) %>%
  mutate(transfer.type = 'Energy producer')

#### only consumption
transfers.consumption.year <- energy.consumption.summarise.year  %>%
  filter(!transfer.name %in% transfers.both) %>%
  mutate(twh.low.net = twh.low.c,
         twh.mean.net = twh.mean.c,
         twh.high.net = twh.high.c,
  ) %>%
  mutate(ratio.low = twh.low.net / twh.low.net * 100,
         ratio.mean = twh.mean.net / twh.mean.net * 100,
         ratio.high = twh.high.net / twh.high.net * 100
  ) %>%
  mutate(transfer.type = 'Energy consumer')

transfers.consumption.month <- energy.consumption.summarise.month  %>%
  filter(!transfer.name %in% transfers.both) %>%
  mutate(twh.low.net = twh.low.c,
         twh.mean.net = twh.mean.c,
         twh.high.net = twh.high.c,
  ) %>%
  mutate(ratio.low = twh.low.net / twh.low.net * 100,
         ratio.mean = twh.mean.net / twh.mean.net * 100,
         ratio.high = twh.high.net / twh.high.net * 100
  ) %>%
  mutate(transfer.type = 'Energy consumer')


#### AN UNPLEASANT TASK THAT WAS TAKING FOREVER ####
budget.df.month <- rbind(transfers.production.month,
                         transfers.consumption.month,
                         energy.budget.month)

budget.df.year <- rbind(transfers.production.year,
                        transfers.consumption.year,
                        energy.budget.year)

write.csv(budget.df.month, 
          paste0(outputDirEnergy, '4_energy_budget_monthly.csv'), 
          row.names = F)

write.csv(budget.df.year, 
          paste0(outputDirEnergy, '4_energy_budget_yearly.csv'), 
          row.names = F)
