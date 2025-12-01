rm(list=ls())

#calculate hydropower head as elevation of intake minus
#the elevation of a few pixel later (assume 10?)

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '1_segments/')

outputDir <- paste0(inputDir, '3_information/')


#### 
intake.data <- read.csv(paste0(outputDir,'1_information_intakes.csv'))

power.intakes <- intake.data %>% 
  filter(intake.type == 'Power station')

n.countries <- unique(power.intakes$Country)

list.countries <- list()

for(i in seq(length(n.countries))){
  
  country <- n.countries[i] 
  
  n.transfers.df <- power.intakes %>% 
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
      
      segment.elevation.data <- read.csv(paste0(segments.folder, 'segment_', 
                                                n.segments[k], '.csv'))
      
      reservoir.id.df <- intake.data %>% 
        filter(transfer.name == transfer.name.j) %>% 
        filter(segment.full == unique(segment.elevation.data$segment.full))
      
      intake.type <- segment.elevation.data$Subject[1]
      
      intake.elevation <- segment.elevation.data$elevation.aster.m[
        which(!is.na(segment.elevation.data$Subject))
      ]
      
      #### get various statistics of the segment
      after.dam.elevation <- min(segment.elevation.data$elevation.aster.m)

      hydropower.head <- intake.elevation - after.dam.elevation

      power.df <- data.frame(
        unique(segment.elevation.data$segment.full),
        unique(segment.elevation.data$section.id),
        unique(segment.elevation.data$segment.id),
        reservoir.id.df$reservoir.id,
        intake.type,
        intake.elevation,
        after.dam.elevation,
        hydropower.head
        
        )
     
      colnames(power.df) <- c('segment.full', 'section.id', 'segment.id',
                              'reservoir.id',
                              'intake.type','intake.elevation',
                              'after.dam.elevation',
                              'hydropower.head')
      
      
      list.segments[[k]] <- power.df 
         
    }
    
    segments.power.df <- do.call(rbind, list.segments)
    
    segments.power.df <- segments.power.df %>% 
      mutate(transfer.name = transfer.name.j) %>% 
      relocate(transfer.name, .before = segment.full)
    
    list.transfers[[j]] <- segments.power.df
      
  }
  
  country.transfers.df <- do.call(rbind, list.transfers)
  
  country.transfers.df <- country.transfers.df %>% 
    mutate(Country = country) %>% 
    relocate(Country, .before = transfer.name)
  
  list.countries[[i]] <- country.transfers.df
  
}

power.dataframe.df <- do.call(rbind, list.countries)

write.csv(power.dataframe.df, paste0(outputDir, '2_0_information_hydropower.csv'), row.names = F)
