rm(list=ls())

#----------- script to calculate energy use of RO using a simple physical model -----------#
library(dplyr)
library(vroom)

#--- functions
replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

formatterlog10 <- function(x){ 
  10^x
}


#### load desaldata and RO fits ####
inputDir <- '../../../../input/wtt/desalination/'
outputDir <- '../../../../output/wtt/model/desalination/0_tech_categories/'

DesalData <- read.csv(paste0(inputDir, 'DesalData_2019_ID_renamed_countries.csv')) 

#### filter #### 
desal.filtered <- DesalData %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  filter(!is.na(Capacity..m3.d.)) %>% 
  filter(!is.na(Feedwater)) %>% 
  filter(!is.na(Online.date))

RO <-  desal.filtered[grepl("RO", desal.filtered$Technology), ] 

#### function to calculate time evolving RO energy intensity ####
source('fun_0_2_energy_ranges_RO_time_evolving.R')

write.csv(energy.data %>%
            relocate(Year, .before = Prediction),
          paste0(outputDir, 'RO_time_evolving_all_feeds.csv'), row.names = F)

#### calculate yearly energy use per feedwater ####
RO.time.evolving.table.list <- list()

#plot data contains all fits (for all feeds for all years)
years <- unique(energy.data$Year)
feeds <- unique(energy.data$Feedwater)

#loop over years
for(idx.year in seq(1, length(years))){
  
  # filter plants that only exists in a given year
  year.online <- as.Date(energy.data$Year[idx.year])
  
  RO.year <- RO %>%
    filter(Online.date <= year.online)
  
  year.feed.list <- list()
  
  #loop over feeds
  for(idx.feed in seq(1, length(feeds))){
    
    replaceMessage(paste0('Year: ', idx.year, '/', length(years),
                          ' - Feed: ', idx.feed, '/', length(feeds)))
    
    RO.year.feed <- RO.year %>% 
      filter(grepl(feeds[idx.feed], Feedwater))
    
    #read energy of selected year for selected feed
    RO.year.feed.energy <- energy.data %>%
      filter(Year == year.online) %>% 
      filter(Feedwater == feeds[idx.feed])
    
    #assign energy ass fitted for the selected year 
    RO.year.feed.calc <- RO.year.feed %>%
      mutate(kwh.d.low=Capacity..m3.d.* RO.year.feed.energy$Lower) %>%
      mutate(kwh.d.mean=Capacity..m3.d.* RO.year.feed.energy$Prediction) %>%
      mutate(kwh.d.high=Capacity..m3.d.* RO.year.feed.energy$Upper)
    
    year.feed.list[[idx.feed]] <- RO.year.feed.calc
    
  }
  
  year.feed <- do.call(rbind, year.feed.list) %>% 
    mutate(year.exponential = year.online)
  
  RO.time.evolving.table.list[[idx.year]] <- year.feed
  
}

RO.time.evolving.table <- do.call(rbind, RO.time.evolving.table.list) %>% 
  mutate(tech = 'RO')

vroom_write(RO.time.evolving.table,
            paste0(outputDir, 'DesalDataEnergy_RO_time_evolving.csv'), ',')
