rm(list=ls())

library(dplyr)

inputDirGlobal <- '../../../../input/global_data/'
inputDirEnergy <- '../../../../output/water_abstraction/model/ibwt/2_energy/'

#### load ####
electricity.country.ts <- read.csv(
  paste0(inputDirGlobal, 'eia_countries_energy_consumption_pivot.csv')) %>% 
  mutate(datetime = as.Date(datetime))

ibwt.budget <- read.csv(paste0(inputDirEnergy, '4_energy_budget_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

#### process ####
#### change country names to match EIA
electricity.country.ts$Country[
  which(electricity.country.ts$Country == 'United States of America')] <- 'USA'

electricity.country.ts$Country[
  which(electricity.country.ts$Country == 'South Africa')] <- 'South_Africa'


#### ####
countries.energy <- unique(ibwt.budget$country)

energy.list <- list()

for(i in seq(length(countries.energy))){
  
  # get modelled energy budget ibwts
  country.budget <- ibwt.budget %>% 
    filter(country == countries.energy[i])
  
  country.years <- country.budget %>% 
    group_by(datetime) %>% 
    summarise(twh.low.c = sum(twh.low.c, na.rm = T),
              twh.mean.c = sum(twh.mean.c, na.rm = T),
              twh.high.c = sum(twh.high.c, na.rm = T),
              twh.low.p = sum(twh.low.p, na.rm = T),
              twh.mean.p = sum(twh.mean.p, na.rm = T),
              twh.high.p = sum(twh.high.p, na.rm = T),
              ) %>% 
    mutate(twh.low.net = twh.low.p - twh.low.c,
           twh.mean.net = twh.mean.p - twh.mean.c,
           twh.high.net = twh.high.p - twh.high.c
           )
  
  #### get reported total electricity consumption
  country.eia <- electricity.country.ts %>% 
    filter(Country == countries.energy[i])
  
  #### join tables 
  country.energy.df <- inner_join(country.eia, country.years) 
  
  country.energy.ratios.df <- country.energy.df %>%
    mutate(ratio.primary.low.c = (twh.low.c / primary.twh) * 100,
           ratio.primary.mean.c = (twh.mean.c / primary.twh) * 100,
           ratio.primary.high.c = (twh.high.c / primary.twh) * 100,
           ratio.electricity.low.c = (twh.low.c / net.electricity.twh) * 100,
           ratio.electricity.mean.c = (twh.mean.c / net.electricity.twh) * 100,
           ratio.electricity.high.c = (twh.high.c / net.electricity.twh) * 100,
           ratio.electricity.low.p = (twh.low.p / net.electricity.twh) * 100,
           ratio.electricity.mean.p = (twh.mean.p / net.electricity.twh) * 100,
           ratio.electricity.high.p = (twh.high.p / net.electricity.twh) * 100)
    
  energy.list[[i]] <- country.energy.ratios.df
  
}

energy.ratios.df <- do.call(rbind, energy.list)

write.csv(energy.ratios.df, paste0(inputDirEnergy, '6_energy_countries_1980_2021.csv'), row.names = F)


#### paper stuff ####
energy.summarise <- energy.ratios.df %>% 
  group_by(Country) %>% 
  summarise(ratio.primary.low.c = mean(ratio.primary.low.c, na.rm = T),
            ratio.primary.mean.c = mean(ratio.primary.mean.c, na.rm = T),
            ratio.primary.high.c = mean(ratio.primary.high.c, na.rm = T),
            ratio.electricity.low.c = mean(ratio.electricity.low.c, na.rm = T),
            ratio.electricity.mean.c = mean(ratio.electricity.mean.c, na.rm = T),
            ratio.electricity.high.c = mean(ratio.electricity.high.c, na.rm = T),
            ratio.electricity.low.p = mean(ratio.electricity.low.p, na.rm = T),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p, na.rm = T),
            ratio.electricity.high.p = mean(ratio.electricity.high.p, na.rm = T))

global.ratio <- energy.ratios.df %>% 
  group_by(datetime) %>% 
  summarise(twh.low.c = sum(twh.low.c),
            twh.mean.c = sum(twh.mean.c),
            twh.high.c = sum(twh.high.c),
            twh.low.p = sum(twh.low.p),
            twh.mean.p = sum(twh.mean.p),
            twh.high.p = sum(twh.high.p)) %>% 
  inner_join(electricity.country.ts %>% 
               filter(Country == 'World')) %>%
  mutate(ratio.primary.low.c = (twh.low.c / primary.twh) * 100,
         ratio.primary.mean.c = (twh.mean.c / primary.twh) * 100,
         ratio.primary.high.c = (twh.high.c / primary.twh) * 100,
         ratio.electricity.low.c = (twh.low.c / net.electricity.twh) * 100,
         ratio.electricity.mean.c = (twh.mean.c / net.electricity.twh) * 100,
         ratio.electricity.high.c = (twh.high.c / net.electricity.twh) * 100,
         ratio.electricity.low.p = (twh.low.p / net.electricity.twh) * 100,
         ratio.electricity.mean.p = (twh.mean.p / net.electricity.twh) * 100,
         ratio.electricity.high.p = (twh.high.p / net.electricity.twh) * 100)

write.csv(global.ratio, paste0(inputDirEnergy, '6_energy_global_1980_2021.csv'), row.names = F)
