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
g <- 9.81 #m s-2 g
rho <- 1000 #kg m-3 density of water
delta.time <- 1 #year 
t.efficiency.low <- 0.9 #pump efficiency (low value for uncertainty)
t.efficiency.high <- 1 #pump efficiency (high value for uncertainty)
capacity.factor.low <- 0.3
capacity.factor.high <- 0.6

#### directories ####
inputDir <- '../../../output/ibwt/model/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(inputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
hydropower.head.info <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/0_information_hydropower.csv'))

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_0_discharge_bias_corrected_monthly.csv'))

discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/2_1_discharge_bias_corrected_yearly.csv'))


#### process ####
energy.month.list <- list()
energy.year.list <- list()

hydropower.turbines.info <- list()

for(i in seq(nrow(hydropower.head.info))){
  
  hydropower.select <- hydropower.head.info[i,]
  
  reservoir.id.select <- hydropower.select$section.id.unique
  
  discharge.select.monthly <- discharge.monthly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  discharge.select.yearly <- discharge.yearly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  
  #### calculate peak yearly production for later adjustments
  # j
  energy.j.high.raw.year <-
    discharge.select.yearly$discharge.m3.y.corrected * 
    (g * hydropower.select$head.m) * (rho * delta.time) * t.efficiency.high * capacity.factor.high
  
  energy.j.high.raw.month <- 
    discharge.select.monthly$discharge.km3.month * 10^9 * 
    (g * hydropower.select$head.m) * (rho * delta.time) * t.efficiency.high * capacity.factor.high
  
  #### get "number of turbines" based on reported plant capacity
  #mean modelled energy production peak for the highest year
  energy.peak.year.modelled.j <- max(energy.j.high.raw.year)
  energy.peak.year.modelled.kwh <- energy.peak.year.modelled.j / (3.6 * 10^6) 
  
  energy.peak.month.modelled.j <- max(energy.j.high.raw.month)
  energy.peak.month.modelled.kwh <- energy.peak.month.modelled.j / (3.6 * 10^6) 
  
  #theoretical max production in one year 
  plant.capacity.mw <- hydropower.select$capacity.mw
  plant.capacity.kw <- plant.capacity.mw * 1000
  month.hours <- 30*24
  year.hours <- 365*24
  
  #kwh
  energy.theoretical.kwh.year <- plant.capacity.kw * year.hours
  energy.theoretical.kwh.month <- plant.capacity.kw * month.hours
  
  #### number of turbines
  # n.turbines <- ceiling(energy.theoretical.twh / energy.peak.year.modelled.twh)
  
  correction.factor.kwh.month <- energy.theoretical.kwh.month / energy.peak.month.modelled.kwh
  correction.factor.kwh.year <- energy.theoretical.kwh.year / energy.peak.year.modelled.kwh
  
  #### multiply modelled timeseries with n of turbines :) ####
  #### monthly ####
  energy.j.low.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * hydropower.select$head.m) * (rho * delta.time) * t.efficiency.low #* capacity.factor.low
  
  energy.j.high.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * hydropower.select$head.m) * (rho * delta.time) * t.efficiency.high #* capacity.factor.high
  
  #tidy dataframe monthly
  energy.month.segment.df <- discharge.select.monthly %>% 
    mutate(segment.id = hydropower.select$segment.full) %>% 
    relocate(segment.id, .after = reservoir.id) %>% 
    mutate(head.m = hydropower.select$head.m) %>% 
    mutate(energy.j.low = energy.j.low.month,
           energy.j.mean = (energy.j.low.month + energy.j.high.month) / 2,
           energy.j.high = energy.j.high.month) %>% 
    mutate(energy.kwh.low = energy.j.low / (3.6 * 10^6) * correction.factor.kwh.month,
           energy.kwh.mean = energy.j.mean / (3.6 * 10^6) * correction.factor.kwh.month,
           energy.kwh.high = energy.j.high / (3.6 * 10^6) * correction.factor.kwh.month
    ) %>% 
    mutate(energy.twh.low = energy.kwh.low / 10^9,
           energy.twh.mean = energy.kwh.mean / 10^9,
           energy.twh.high = energy.kwh.high / 10^9) %>%
    mutate(energy.m3.kwh.low = discharge.m3.month / energy.kwh.high,
           energy.m3.kwh.mean = discharge.m3.month / energy.kwh.mean,
           energy.m3.kwh.high = discharge.m3.month / energy.kwh.low) %>%
    mutate(energy.m3.kwh.km.low = energy.m3.kwh.low / max(hydropower.select$segment.length),
           energy.m3.kwh.km.mean = energy.m3.kwh.mean / max(hydropower.select$segment.length),
           energy.m3.kwh.km.high = energy.m3.kwh.high/ max(hydropower.select$segment.length)) 
  
  #### yearly ####
  #### calculate yearly consumption as upscale from monthly
  energy.year.segment.sums <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, section.id, reservoir.id, segment.id, hemisphere, head.m) %>% 
    summarise_at(vars(energy.j.low:energy.twh.high), ~ sum(.x, na.rm = TRUE))
  
  energy.year.segment.means <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, section.id, reservoir.id, segment.id, hemisphere, head.m) %>% 
    summarise_at(vars(energy.m3.kwh.low:energy.m3.kwh.km.high), ~ mean(.x, na.rm = TRUE))
  
  energy.year.segment.df <- inner_join(energy.year.segment.sums, 
                                       energy.year.segment.means)
    
 
  #### add final dfs to monthly and yearly dfs
  
  energy.month.list[[i]] <- energy.month.segment.df
  energy.year.list[[i]] <- energy.year.segment.df
  
}


energy.df.month <- do.call(rbind, energy.month.list)
energy.df.year <- do.call(rbind, energy.year.list)

write.csv(energy.df.month, 
          paste0(outputDirEnergy, '0_energy_production_monthly.csv'), 
          row.names = F)

write.csv(energy.df.year, 
          paste0(outputDirEnergy, '0_energy_production_yearly.csv'), 
          row.names = F)
