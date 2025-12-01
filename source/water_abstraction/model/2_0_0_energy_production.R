library(dplyr)
library(tidyr)
# library(lubridate)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

#### costants to calculate energy consumption ####
g <- 9.81 #m s-2
rho <- 1000 #density of water
delta.time <- 1 #year 
t.efficiency.low <- 0.9 #turbine efficiency (low value for uncertainty)
t.efficiency.high <- 1.0 #turbine efficiency (high value for uncertainty)

#### directories ####
outputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(outputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(outputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(outputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
hydropower.head.info <- read.csv(paste0(inputDirElevation, '2_0_information_hydropower.csv'))
intake.data.df <- read.csv(paste0(inputDirDischarge, '0_timeseries_raw/2_bias_correction_segments.csv'))
discharge.monthly.df <- read.csv(paste0(inputDirDischarge, '1_bias_corrected/0_0_discharge_bias_corrected_monthly.csv'))
discharge.yearly.df <- read.csv(paste0(inputDirDischarge, '1_bias_corrected/0_1_discharge_bias_corrected_yearly.csv'))

#### process ####
hydropower.df <- intake.data.df %>% 
  filter(intake.type == 'Power station') %>% 
  inner_join(., hydropower.head.info %>% select(transfer.name, segment.full, hydropower.head))

energy.month.list <- list()
energy.year.list <- list()

for(i in seq(nrow(intake.data.df))){
  
  hydropower.select <- hydropower.df[i,]
  
  reservoir.id.select <- hydropower.select$section.id.unique
  
  discharge.select.monthly <- discharge.monthly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  discharge.select.yearly <- discharge.yearly.df %>% 
    filter(section.id.unique == reservoir.id.select)
  
  #### monthly ####
  energy.j.low.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * hydropower.select$hydropower.head) * (rho * delta.time) * t.efficiency.low
  
  energy.j.high.month <- 
    discharge.select.monthly$discharge.m3.month * 
    (g * hydropower.select$hydropower.head) * (rho * delta.time) * t.efficiency.high
  
  #tidy dataframe monthly
  energy.month.segment.df <- discharge.select.monthly %>% 
    mutate(segment.id = hydropower.select$segment.full) %>% 
    relocate(segment.id, .after = reservoir.id) %>% 
    mutate(energy.j.low = energy.j.low.month,
           energy.j.mean = (energy.j.low.month + energy.j.high.month) / 2,
           energy.j.high = energy.j.high.month) %>% 
    mutate(energy.kwh.low = energy.j.low / (3.6 * 10^6),
           energy.kwh.mean = energy.j.mean / (3.6 * 10^6),
           energy.kwh.high = energy.j.high / (3.6 * 10^6)) %>%
    mutate(energy.twh.low = energy.kwh.low / 10^9,
           energy.twh.mean = energy.kwh.mean / 10^9,
           energy.twh.high = energy.kwh.high / 10^9) 
  
  #### yearly ####
  #calculate yearly energy consumption range (high - low efficiency)
  energy.j.low.year <-
    discharge.select.yearly$discharge.m3.y.corrected * 
    (g * hydropower.select$hydropower.head) * (rho * delta.time) / t.efficiency.high
  
  energy.j.high.year <-
    discharge.select.yearly$discharge.m3.y.corrected * 
    (g * hydropower.select$hydropower.head) * (rho * delta.time) / t.efficiency.low
  
  #tidy dataframe
  energy.year.segment.df <- discharge.select.yearly %>% 
    mutate(segment.id = hydropower.select$segment.full) %>% 
    relocate(segment.id, .after = reservoir.id) %>% 
    mutate(energy.j.low = energy.j.low.year,
           energy.j.mean = (energy.j.low.year + energy.j.high.year) / 2,
           energy.j.high = energy.j.high.year) %>% 
    mutate(energy.kwh.low = energy.j.low / (3.6 * 10^6),
           energy.kwh.mean = energy.j.mean / (3.6 * 10^6),
           energy.kwh.high = energy.j.high / (3.6 * 10^6)) %>%
    mutate(energy.twh.low = energy.kwh.low / 10^9,
           energy.twh.mean = energy.kwh.mean / 10^9,
           energy.twh.high = energy.kwh.high / 10^9) 
  
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
