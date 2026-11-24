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
  paste0(inputDirDischarge, '2_infrastructure/2_information_bypass.csv'))

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/1_discharge_bias_corrected_monthly.csv'))

discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/1_discharge_bias_corrected_yearly.csv'))

#### process ####
energy.month.list <- list()
energy.year.list <- list()

for(i in seq(nrow(pumping.lift.info))){
  
  pumping.select <- pumping.lift.info[i,]
  
  reservoir.id.select <- pumping.select$section.id.unique
  
  if(!reservoir.id.select %in% discharge.monthly.df$section.id.unique){ 
    
    next
    
  }
  
  bypass.type.select <- pumping.select$bypass.type
  
  discharge.select.monthly <- discharge.monthly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  discharge.select.yearly <- discharge.yearly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  
  #### distribute discharge to parallel routings
  # if(pumping.select$parallel.count != 0){
  #   
  #   discharge.m3.month <- discharge.select.monthly$discharge.m3.month.corrected / pumping.select$parallel.count
  #   
  # }
  # 
  # else{discharge.m3.month <- discharge.select.monthly$discharge.m3.month.corrected}
  
  discharge.m3.month <- discharge.select.monthly$discharge.m3.month.corrected
  
  #### monthly ####
  energy.j.low.month <- 
    discharge.m3.month * 
    (g * pumping.select$net.bypass) * (rho * delta.time) / p.efficiency.high
  
  energy.j.high.month <- 
    discharge.m3.month * 
    (g * pumping.select$net.bypass) * (rho * delta.time) / p.efficiency.low
  
  #tidy dataframe monthly
  energy.month.segment.df <- discharge.select.monthly %>% 
    mutate(segment.id = pumping.select$segment.name) %>% 
    relocate(segment.id, .after = reservoir.id) %>% 
    mutate(bypass.type = bypass.type.select) %>% 
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
           energy.kwh.m3.high = energy.kwh.high / discharge.m3.month) 
  
  #### yearly ####
  #### calculate yearly consumption as upscale from monthly
  energy.year.segment.sums <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, reservoir.id, segment.id, hemisphere, bypass.type) %>% 
    summarise_at(vars(discharge.m3.s:energy.twh.high), ~ sum(.x, na.rm = TRUE))
  
  energy.year.segment.means <- energy.month.segment.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, section.id.unique, country, transfer.name, reservoir.id, segment.id, hemisphere, bypass.type) %>% 
    summarise_at(vars(energy.kwh.m3.low:energy.kwh.m3.high), ~ mean(.x, na.rm = TRUE))
  
  energy.year.segment.df <- inner_join(energy.year.segment.sums, 
                                       energy.year.segment.means,
                                       multiple = 'all')
  
  energy.month.list[[i]] <- energy.month.segment.df
  energy.year.list[[i]] <- energy.year.segment.df
  
}

energy.df.month <- do.call(rbind, energy.month.list)
energy.df.year <- do.call(rbind, energy.year.list) %>% 
  rename(discharge.m3.year = discharge.m3.month,
         discharge.m3.year.corrected = discharge.m3.month.corrected,
         discharge.km3.year = discharge.km3.month,
         discharge.km3.year.corrected = discharge.km3.month.corrected)

write.csv(energy.df.month, 
          paste0(outputDirEnergy, '2_energy_savings_monthly.csv'), 
          row.names = F)

write.csv(energy.df.year, 
          paste0(outputDirEnergy, '2_energy_savings_yearly.csv'), 
          row.names = F)

# #### check 
check.discharge <- energy.df.year %>%
  group_by(transfer.name) %>%
  summarise(discharge.km3.year.corrected = mean(discharge.km3.year.corrected),
            energy.twh.mean = mean(energy.twh.mean))
