rm(list=ls())

#pumping lift height is calculated as 
#elevation of highest point on the transfer path 
#minus
#elevation of intake point

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
inputDirElevation <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/1_segments/'

outputDir <- paste0(inputDir, '2_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)

#### load 
data.sections <- read.csv(paste0(inputDir,'1_bias_corrected/1_0_bias_correction_sections.csv'))
data.segments <- read.csv(paste0(inputDir,'1_bias_corrected/1_1_bias_correction_segments.csv'))

pumping.intakes <- data.segments %>% 
  filter(intake.type == 'Pumping station')

#### process ####
n.countries <- unique(pumping.intakes$Country)

list.countries <- list()

for(i in seq(length(n.countries))){
  
  country <- n.countries[i] 
  
  n.transfers.df <- pumping.intakes %>% 
    filter(Country == country) 
  
  n.transfers <- unique(n.transfers.df$transfer.name)
  
  list.transfers <- list()
  
  #### loop per transfer
  for(j in seq(length(n.transfers))){
    
    transfer.name.j <- n.transfers[j]
    
    #### get elevation data of all transfer segments ####
    segments.folder <- paste0(inputDirElevation, country, '/',
                              transfer.name.j, '/')
    
    
    n.segments.df <- n.transfers.df %>% 
      filter(transfer.name == transfer.name.j)
    
    n.segments <- n.segments.df$segment.full
    
    list.segments <- list()
    
    for(k in seq(length(n.segments))){
      
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name.j, ' - ',
                            'Segment: ', k, '/', length(n.segments)))
      
      #### segment info
      segment.info <- n.segments.df[k,]
      
      #### get elevation lift and bind
      segment.elevation.data <- read.csv(paste0(segments.folder, 'segment_', 
                                                n.segments[k], '.csv'))
      
      intake.elevation <- segment.elevation.data$elevation.aster.m[
        which(!is.na(segment.elevation.data$Subject))
      ]
      
      maximum.elevation <- max(segment.elevation.data$elevation.aster.m)

      pumping.lift <- maximum.elevation - intake.elevation

      #### bind
      pumping.df <- segment.info %>% 
        mutate(elevation.intake = intake.elevation,
               elevation.max = maximum.elevation,
               elevation.lift = pumping.lift)
      
      
      list.segments[[k]] <- pumping.df 
         
    }
    
    segments.pumping.df <- do.call(rbind, list.segments)
    
    list.transfers[[j]] <- segments.pumping.df
      
  }
  
  country.transfers.df <- do.call(rbind, list.transfers)
  
  country.transfers.df <- country.transfers.df %>% 
    mutate(Country = country) %>% 
    relocate(Country, .before = transfer.name)
  
  list.countries[[i]] <- country.transfers.df
  
}

pumping.dataframe.df <- do.call(rbind, list.countries)

write.csv(pumping.dataframe.df, paste0(outputDir, '1_information_pumping.csv'), row.names = F)