rm(list=ls())

library(readxl)
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
t.efficiency.low <- 0.9 #turbine efficiency (low value for uncertainty)
t.efficiency.high <- 1 #turbine efficiency (high value for uncertainty)
capacity.factor.low <- 0.3
capacity.factor.high <- 0.8
capacity.factor.mean <- 0.55

#### directories ####
inputDir <- '../../../output/ibwt/model/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(inputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
hydropower.head.info <- read.csv(
  paste0(inputDirDischarge, '2_infrastructure/0_information_hydropower.csv'))

hydropower.cf.info <- read_excel('../../../input/ibwt/hydropower_cf.xlsx')

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/1_discharge_bias_corrected_monthly.csv'))

discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/1_discharge_bias_corrected_yearly.csv'))


#### process ####

hydropower.info.complete <- inner_join(hydropower.head.info, hydropower.cf.info)

energy.month.list <- list()
energy.year.list <- list()


for(i in seq(nrow(hydropower.head.info))){
  
  hydropower.select <- hydropower.info.complete[i,]
  
  print(hydropower.select$plant.name)
  
  reservoir.id.select <- hydropower.select$section.id.unique
  reservoir.lat <- hydropower.select$reservoir.lat
  reservoir.id.shah <- hydropower.select$id.shah2025
  
  if(!reservoir.id.select %in% discharge.monthly.df$section.id.unique){ 
    
    next
    
  }
  
  if(reservoir.lat > 0){hemisphere.sel <- 'North'}
  else{hemisphere.sel <- 'South'}
  
  #calculate design discharge if plants operate at maximum capacity
  #assuming best turbine efficiency of 1
  plant.capacity.mw <- hydropower.select$capacity.mw
  plant.head.m <- hydropower.select$head.m
  
  
  #get reported or default capacity factors
  plant.cf.low <- hydropower.select$cf.reported.low
  plant.cf.mean <- hydropower.select$cf.reported.mean
  plant.cf.high <- hydropower.select$cf.reported.high
  
  if(is.na(plant.cf.low)){
    
    plant.cf.low <- plant.cf.mean - (plant.cf.mean * 0.2)
    plant.cf.high <- plant.cf.mean + (plant.cf.mean * 0.2)
    
  }
  
  plant.cf.low <- max(0, plant.cf.low)
  plant.cf.high <- min(1, plant.cf.high)
  
  #### cap modelled monthly discharge
  discharge.select.monthly <- discharge.monthly.df %>% 
    filter(section.id.unique == reservoir.id.select) 
  
  discharge.select.yearly <- discharge.yearly.df %>% 
    filter(section.id.unique == reservoir.id.select) %>% 
    mutate(discharge.km3.year.max = max(discharge.km3.year.corrected)) %>%
    mutate(distributor.ratio = discharge.km3.year.corrected / discharge.km3.year.max)

  # check
  
  #### distribute discharge to parallel routings
  if(hydropower.select$parallel.count != 0){
    
    discharge.m3.month.select <- discharge.select.monthly$discharge.m3.month.corrected / hydropower.select$parallel.count
    discharge.m3.year.select <- discharge.select.yearly$discharge.km3.year.corrected * 10^9 / hydropower.select$parallel.count
    
  }
  
  else{
    

    discharge.m3.month.select <- discharge.select.monthly$discharge.m3.month.corrected 
    discharge.m3.year.select <- discharge.select.yearly$discharge.km3.year.corrected * 10^9 
    
    }

  
  
  discharge.monthly <- discharge.select.monthly %>% 
    mutate(discharge.m3.month = discharge.m3.month.select) %>% 
    select(datetime:days.in.month, hemisphere, discharge.m3.month)
  
  discharge.yearly <- discharge.select.yearly %>% 
    mutate(discharge.m3.year = discharge.m3.year.select) %>% 
    select(datetime:reservoir.id, hemisphere, discharge.m3.year, distributor.ratio)
  
  
  #tidy dataframe monthly
  energy.month.segment.df <- discharge.monthly %>% 
    mutate(id.shah2025 = reservoir.id.shah,
           segment.id = hydropower.select$segment.full,
           plant.capacity.mw = hydropower.select$capacity.mw) %>%
    relocate(segment.id, .after = reservoir.id) %>%
    mutate(head.m = hydropower.select$head.m,
           cf.low = plant.cf.low,
           cf.mean = plant.cf.mean,
           cf.high = plant.cf.high,
           hemisphere = hemisphere.sel
           )  %>%
    relocate(hemisphere, .after = transfer.name) %>%
    relocate(id.shah2025:cf.mean, .after = days.in.month) 
  
  
  
  #### loop per year to assign the energy production proportionally to the monthly discharge
  energy.month.reassign.list <- list()
  
  for(year.date in seq(nrow(discharge.select.yearly))){
    
    year.select <- discharge.select.yearly$datetime[year.date]
    
    year.distributor <- discharge.select.yearly$distributor.ratio[year.date]
    
    energy.monthly.year.select <- energy.month.segment.df %>% 
      filter(year(datetime) == year(year.select)) 
    
    #get days in a year
    days.in.year <- sum(energy.monthly.year.select$days.in.month)
    
    #get energy production in that year [mwh] based on 
    #plant capacity, hours, capacity factor and discharge relative to the whole period
    energy.production.year.select.mwh.low <- 
      plant.capacity.mw * days.in.year * 24 * plant.cf.low * year.distributor
    
    energy.production.year.select.mwh.mean <- 
      plant.capacity.mw * days.in.year * 24 * plant.cf.mean * year.distributor
    
    energy.production.year.select.mwh.high <- 
      plant.capacity.mw * days.in.year * 24 * plant.cf.high * year.distributor
    
    energy.monthly.year.select.final <- energy.monthly.year.select %>% 
      mutate(month.distributor = discharge.m3.month / sum(discharge.m3.month)) %>% 
      mutate(energy.mwh.low = energy.production.year.select.mwh.low * month.distributor,
             energy.mwh.mean = energy.production.year.select.mwh.mean * month.distributor,
             energy.mwh.high = energy.production.year.select.mwh.high * month.distributor)
    
    energy.month.reassign.list[[year.date]] <- energy.monthly.year.select.final

  }
  
  
  energy.month.reassign.df <- do.call(rbind, energy.month.reassign.list) %>% 
    mutate(energy.kwh.low = energy.mwh.low * 1000,
           energy.kwh.mean = energy.mwh.mean * 1000,
           energy.kwh.high = energy.mwh.high * 1000) %>%
    mutate(energy.twh.low = energy.mwh.low / 1000000,
           energy.twh.mean = energy.mwh.mean / 1000000,
           energy.twh.high = energy.mwh.high / 1000000) %>% 
    #calculate discharge based on modelled energy independent of absolute modelled discharge
    # mutate(discharge.retake.m3.month.low = energy.kwh.low * 3.6 * 10^6 /(t.efficiency.low * rho * g * plant.head.m),
    #        discharge.retake.m3.month.mean = energy.kwh.mean * 3.6 * 10^6 /(t.efficiency.low * rho * g * plant.head.m),
    #        discharge.retake.m3.month.high = energy.kwh.high * 3.6 * 10^6 /(t.efficiency.low * rho * g * plant.head.m)) %>% 
    # mutate(discharge.retake.km3.month.low = discharge.retake.m3.month.low / 10^9,
    #        discharge.retake.km3.month.mean = discharge.retake.m3.month.mean / 10^9,
    #        discharge.retake.km3.month.high = discharge.retake.m3.month.high / 10^9) %>% 
    # mutate(energy.m3.kwh.low = discharge.retake.m3.month.high / energy.kwh.low,
    #        energy.m3.kwh.mean = discharge.retake.m3.month.mean / energy.kwh.mean,
    #        energy.m3.kwh.high = discharge.retake.m3.month.low / energy.kwh.high) %>%
    mutate(energy.m3.kwh.low = discharge.m3.month.select / energy.kwh.low,
           energy.m3.kwh.mean = discharge.m3.month.select / energy.kwh.mean,
           energy.m3.kwh.high = discharge.m3.month.select / energy.kwh.high) %>%
    mutate(energy.m3.kwh.km.low = energy.m3.kwh.low / max(hydropower.select$segment.length),
           energy.m3.kwh.km.mean = energy.m3.kwh.mean / max(hydropower.select$segment.length),
           energy.m3.kwh.km.high = energy.m3.kwh.high/ max(hydropower.select$segment.length)) 
  

  #### yearly ####
  #### calculate yearly consumption as upscale from monthly
  energy.year.segment.sums <- energy.month.reassign.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, id.shah2025, section.id.unique, country, transfer.name, reservoir.id, segment.id, plant.capacity.mw, head.m, hemisphere) %>% 
    summarise_at(vars(energy.mwh.low:energy.twh.high), ~ sum(.x, na.rm = TRUE))
    # summarise_at(vars(energy.mwh.low:discharge.retake.km3.month.high), ~ sum(.x, na.rm = TRUE))
  
  energy.year.segment.means <- energy.month.reassign.df %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    mutate(datetime=floor_date(datetime,'year')) %>% 
    group_by(datetime, id.shah2025, section.id.unique, country, transfer.name, reservoir.id, segment.id, plant.capacity.mw, head.m, hemisphere) %>% 
    summarise_at(vars(energy.m3.kwh.low:energy.m3.kwh.km.high), ~ mean(.x, na.rm = TRUE))
  
  energy.year.segment.df <- inner_join(energy.year.segment.sums, 
                                       energy.year.segment.means)
  
 
  #### add final dfs to monthly and yearly dfs
  
  energy.month.list[[i]] <- energy.month.reassign.df
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

# ###checks
# energy.below.100 <- energy.df.year %>% 
#   filter(plant.capacity.mw < 100) %>% 
#   group_by(datetime) %>% 
#   summarise(energy.twh.low.below = sum(energy.twh.low),
#             energy.twh.mean.below = sum(energy.twh.mean),
#             energy.twh.high.below = sum(energy.twh.high))
# 
# energy.above.100 <- energy.df.year %>% 
#   filter(plant.capacity.mw >= 100) %>% 
#   group_by(datetime) %>% 
#   summarise(energy.twh.low.above = sum(energy.twh.low),
#             energy.twh.mean.above = sum(energy.twh.mean),
#             energy.twh.high.above = sum(energy.twh.high))
# 
# energy.compare.100 <- inner_join(energy.above.100, energy.below.100) %>% 
#   mutate(ratio.low = energy.twh.low.below * 100 / (energy.twh.low.below + energy.twh.low.above),
#          ratio.mean = energy.twh.mean.below * 100 / (energy.twh.mean.below + energy.twh.mean.above),
#          ratio.high = energy.twh.high.below * 100 / (energy.twh.high.below + energy.twh.high.above))
# 
# summary(energy.compare.100$ratio.low)
# summary(energy.compare.100$ratio.mean)
# summary(energy.compare.100$ratio.high)
# 
# nrow(hydropower.info.complete %>% 
#   filter(capacity.mw < 100))
# nrow(hydropower.info.complete %>% 
#        filter(capacity.mw >= 100))
