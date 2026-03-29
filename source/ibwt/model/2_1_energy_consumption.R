rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

#### costants to calculate energy consumption ####
g <- 9.81 #m s-2
rho <- 1000 #density of water
delta.time <- 1 #year 
p.efficiency.low <- 0.5 #pump efficiency (low value for uncertainty)
p.efficiency.high <- 0.9 #pump efficiency (high value for uncertainty)

#### directories ####
inputDir <- '../../../output/ibwt/model/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(inputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
pumping.lift.info <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_0_discharge_bias_corrected_monthly.csv'))
discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_1_discharge_bias_corrected_yearly.csv'))

#### process ####
energy.month.list <- list()
energy.year.list <- list()

for(i in seq(nrow(pumping.lift.info))){
  
  pumping.select <- pumping.lift.info[i,]
  
  reservoir.id.select <- pumping.select$section.id.unique
  
  discharge.select.monthly <- discharge.monthly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  discharge.select.yearly <- discharge.yearly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  #### monthly ####
  energy.j.low.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * pumping.select$elevation.lift) * (rho * delta.time) / p.efficiency.high
  
  energy.j.high.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * pumping.select$elevation.lift) * (rho * delta.time) / p.efficiency.low
  
  #tidy dataframe monthly
  energy.month.segment.df <- discharge.select.monthly %>% 
    mutate(segment.id = pumping.select$segment.full) %>% 
    relocate(segment.id, .after = reservoir.id) %>% 
    mutate(pumping.lift.m = pumping.select$elevation.lift) %>% 
    mutate(energy.j.low = energy.j.low.month,
           energy.j.mean = (energy.j.low.month + energy.j.high.month) / 2,
           energy.j.high = energy.j.high.month) %>% 
    mutate(energy.kwh.low = energy.j.low / (3.6 * 10^6),
           energy.kwh.mean = energy.j.mean / (3.6 * 10^6),
           energy.kwh.high = energy.j.high / (3.6 * 10^6)) %>%
    mutate(energy.twh.low = energy.kwh.low / 10^9,
           energy.twh.mean = energy.kwh.mean / 10^9,
           energy.twh.high = energy.kwh.high / 10^9) %>%
    mutate(energy.kwh.m3.low = energy.kwh.low / discharge.m3.month,
           energy.kwh.m3.mean = energy.kwh.mean / discharge.m3.month,
           energy.kwh.m3.high = energy.kwh.high / discharge.m3.month) %>%
    mutate(energy.kwh.m3.km.low = energy.kwh.m3.low / max(pumping.select$segment.length),
           energy.kwh.m3.km.mean = energy.kwh.m3.mean / max(pumping.select$segment.length),
           energy.kwh.m3.km.high = energy.kwh.m3.high/ max(pumping.select$segment.length)) 
  
  #### yearly ####
  #### calculate yearly consumption as upscale from monthly
  energy.year.segment.sums <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, section.id, reservoir.id, segment.id, hemisphere, pumping.lift.m) %>% 
    summarise_at(vars(energy.j.low:energy.twh.high), ~ sum(.x, na.rm = TRUE))
  
  energy.year.segment.means <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, section.id, reservoir.id, segment.id, hemisphere, pumping.lift.m) %>% 
    summarise_at(vars(energy.kwh.m3.low:energy.kwh.m3.km.high), ~ mean(.x, na.rm = TRUE))
  
  energy.year.segment.df <- inner_join(energy.year.segment.sums, 
                                       energy.year.segment.means)
  
  energy.month.list[[i]] <- energy.month.segment.df
  energy.year.list[[i]] <- energy.year.segment.df
  
}

energy.df.month <- do.call(rbind, energy.month.list)
energy.df.year <- do.call(rbind, energy.year.list)

write.csv(energy.df.month, 
          paste0(outputDirEnergy, '1_energy_consumption_monthly.csv'), 
          row.names = F)

write.csv(energy.df.year, 
          paste0(outputDirEnergy, '1_energy_consumption_yearly.csv'), 
          row.names = F)
