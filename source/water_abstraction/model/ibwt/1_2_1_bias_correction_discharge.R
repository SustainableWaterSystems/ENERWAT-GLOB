rm(list=ls())

library(dplyr)
library(lubridate)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

rsq <- function (x, y) cor(x, y) ^ 2

#### directories ####
inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

outputDirData <- paste0(inputDirDischarge, '1_bias_corrected/')
dir.create(outputDirData, recursive = T, showWarnings = F)

#### load ####
correction.factors <- read.csv(
  paste0(inputDirDischarge, '0_timeseries_raw/1_0_bias_correction_sections.csv'))

#### process ####
discharge.monthly.list <- list()
discharge.yearly.list <- list()

for(i in seq(nrow(correction.factors))){
  
  section.data <- correction.factors[i,]
  
  reservoir.select <- section.data$reservoir.id
  
  initial.year.transfer <- section.data$initial.year.transfer
  
  bias.correction.factor <- section.data$bias.mult.factor
  
  #read raw timeseries of reservoir 
  discharge.monthly.raw <- read.csv(paste0(
    inputDirDischarge, '0_timeseries_raw/timeseries/m3_s_outlet_',
    reservoir.select, '.csv'))
  
  discharge.monthly.process <- discharge.monthly.raw %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    #cut dates before initial date of water transfer
    filter(datetime > initial.year.transfer) %>% 
    mutate(month = month(datetime),
           days.in.month = days_in_month(datetime)) %>% 
    rename(discharge.m3.s = discharge) %>% 
    mutate(discharge.m3.s.corrected = discharge.m3.s * bias.correction.factor) %>% 
    mutate(discharge.m3.month = discharge.m3.s.corrected * 60 * 60 * 24 * days.in.month) %>% 
    mutate(discharge.km3.month = discharge.m3.month / 1000000000) %>% 
    mutate(section.id.unique = section.data$section.id.unique,
           country = section.data$Country,
           transfer.name = section.data$transfer.name,
           section.id = section.data$section.id,
           reservoir.id = section.data$reservoir.id,
           lon = section.data$lon,
           lat = section.data$lat,
           hemisphere = section.data$hemisphere) 

  discharge.yearly.process <- discharge.monthly.process %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    mutate(datetime = floor_date(datetime, 'year')) %>% 
    group_by(datetime) %>% 
    summarise(discharge.m3.y.corrected = sum(discharge.m3.month)) %>% 
    mutate(discharge.km3.y.corrected = discharge.m3.y.corrected / 1000000000) %>% 
    mutate(section.id.unique = section.data$section.id.unique,,
           country = section.data$Country,
           transfer.name = section.data$transfer.name,
           section.id = section.data$section.id,
           reservoir.id = section.data$reservoir.id,
           lon = section.data$lon,
           lat = section.data$lat,
           hemisphere = section.data$hemisphere) 
    
  discharge.monthly.list[[i]] <- discharge.monthly.process
  discharge.yearly.list[[i]] <- discharge.yearly.process
  
}

discharge.monthly.df <- do.call(rbind, discharge.monthly.list) 
discharge.yearly.df <- do.call(rbind, discharge.yearly.list)

#### cut discharge to 1980-2021 ####

discharge.monthly.cut <- discharge.monthly.df %>% 
  filter(year(datetime) >= 1980 & year(datetime) <= 2021)

discharge.yearly.cut <- discharge.yearly.df %>% 
  filter(year(datetime) >= 1980 & year(datetime) <= 2021)

####save####
write.csv(discharge.monthly.cut, paste0(outputDirData, '0_0_discharge_bias_corrected_monthly.csv'), row.names = F)
write.csv(discharge.yearly.cut, paste0(outputDirData, '0_1_discharge_bias_corrected_yearly.csv'), row.names = F)


#### validation ####

section.information <- correction.factors

discharge.yearly.corrected <- discharge.yearly.cut

discharge.corrected.summarise <- discharge.yearly.corrected %>% 
  group_by(section.id.unique) %>% 
  summarise(discharge.km3.y.mean.corrected = mean(discharge.km3.y.corrected))

validation.df <- inner_join(section.information, discharge.corrected.summarise) 

#### check number of reservoirs per transfer
reservoirs.n <- validation.df %>% 
  group_by(transfer.name) %>% 
  summarise(reservoirs.count=n())

#### validation dataframe

validation.df.alt <- validation.df %>% 
  group_by(transfer.name) %>% 
  summarise(discharge.km3.y.mean = mean(discharge.km3.y.mean),
            discharge.km3.y.mean.corrected = sum(discharge.km3.y.mean.corrected),
            volumes.km3.y = mean(volumes.km3.y)
  ) 

validation.df.res <- inner_join(validation.df.alt, reservoirs.n)

validation.df.singel <- validation.df.res %>% 
  filter(reservoirs.count == 1)
validation.df.multiple <- validation.df.res %>% 
  filter(reservoirs.count != 1)

#### check correlations
#before bias correction
rsq(validation.df.alt$discharge.km3.y.mean, validation.df.alt$volumes.km3.y) #r2 original
#after bias correction (all)
rsq(validation.df.alt$discharge.km3.y.mean.corrected, validation.df.alt$volumes.km3.y) #r2 bias corrected
#after bias correction (one reservoir)
rsq(validation.df.singel$discharge.km3.y.mean.corrected, validation.df.singel$volumes.km3.y) #r2 bias corrected
#after bias correction (multiple reservoirs)
rsq(validation.df.multiple$discharge.km3.y.mean.corrected, validation.df.multiple$volumes.km3.y) #r2 bias corrected


sum(validation.df.res$discharge.km3.y.mean.corrected)
sum(validation.df.res$volumes.km3.y)

sum(validation.df.singel$discharge.km3.y.mean.corrected)
sum(validation.df.singel$volumes.km3.y)

sum(validation.df.multiple$discharge.km3.y.mean)
sum(validation.df.multiple$discharge.km3.y.mean.corrected)
sum(validation.df.multiple$volumes.km3.y)
