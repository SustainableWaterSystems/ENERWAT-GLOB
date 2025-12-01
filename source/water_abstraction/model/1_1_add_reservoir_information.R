rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


outputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(outputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(outputDir, '1_discharge_timeseries/0_timeseries_raw/')


#### ####
data.sections <- read.csv(paste0(inputDirElevation, '0_0_sections_reservoirs.csv'))
data.segments <- read.csv(paste0(inputDirElevation, '1_information_intakes.csv'))


#### collect average discharge 1979-2023 for each section ####
section.info.list <- list()

for(i in seq(nrow(data.sections))){
  
  section.select <- data.sections[i,]
  
  section.segments <- data.segments %>% 
    filter(reservoir.id == section.select$reservoir_ID,
           section.id == section.select$Section)
  
  #### types of intake per section
  intake.types.section <- unique(section.segments$intake.type)
  
  #### assign ids to intake mixes
  if(length(intake.types.section) == 1){
    
    if(intake.types.section == 'Power station'){
      intake.type.id <- 1
    }
    
    else if(intake.types.section == 'Pumping station'){
      intake.type.id <- 2
    }
    
    else if(intake.types.section == 'Reservoir intake' | 
            intake.types.section == 'River intake' ){
      intake.type.id <- 3
    }
    
  }
  
  else if(length(intake.types.section) > 1){
    
    if(all(c('Pumping station','Power station') %in% intake.types.section)==T){
      intake.type.id <- 4
    }
    
    else(intake.type.id <- 5)
    
  }
  
  #### discharge
  initial.year.transfer.date <- as.Date(paste(section.select$initial.year, 1, 1, sep = "-"))

  discharge.monthly.raw <- read.csv(paste0(
    inputDirDischarge, 'timeseries/m3_s_outlet_',
    section.select$reservoir_ID , '.csv'))
  
  discharge.monthly.process <- discharge.monthly.raw %>% 
    mutate(datetime = as.Date(datetime)) %>% 
    mutate(month = month(datetime),
           days.in.month = days_in_month(datetime)) %>% 
    rename(discharge.m3.s = discharge) %>% 
    mutate(discharge.m3.month = discharge.m3.s * 60 * 60 * 24 * days.in.month) %>%
    #cut dates before initial date of water transfer
    filter(datetime > initial.year.transfer.date) 
  
  discharge.year.raw <- discharge.monthly.process %>% 
    mutate(datetime = floor_date(datetime, 'year')) %>% 
    group_by(datetime) %>% 
    summarise(discharge.m3.y = sum(discharge.m3.month)) %>% 
    mutate(discharge.km3.y = discharge.m3.y / 1000000000) %>% 
    mutate(country = section.select$Country,
           reservoir.id = section.select$reservoir_ID,
           lon = section.select$lon,
           lat = section.select$lat) %>% 
    mutate(hemisphere = case_when(lat < 0 ~ "South",
                                  lat > 0 ~ "North"))
  
  discharge.km3.y.mean.raw <- round(mean(discharge.year.raw$discharge.km3.y),3)
  
  #### make avg discharge dataframe
  
  section.discharge.info <- section.select %>% 
    mutate(discharge.km3.y.mean = discharge.km3.y.mean.raw,
           intake.type.id = intake.type.id,
           hemisphere = unique(discharge.year.raw$hemisphere)) %>% 
    rename(transfer.name = ibwt.project.name,
           reservoir.id = reservoir_ID,
           initial.year.transfer = initial.year)
  
  section.info.list[[i]] <- section.discharge.info
  
}

section.info.df <- do.call(rbind, section.info.list) %>% 
  mutate(intake.type.string = case_when(intake.type.id == 1 ~ "Hydropower only",
                                        intake.type.id == 2 ~ "Pumping only",
                                        intake.type.id == 3 ~ "Discharge only",
                                        intake.type.id == 4 ~ "Mixed energy",
                                        intake.type.id == 5 ~ "Mixed energy + Discharge only"))

#### save ####
write.csv(section.info.df, paste0(inputDirDischarge, '0_information_sections.csv'), row.names = F)