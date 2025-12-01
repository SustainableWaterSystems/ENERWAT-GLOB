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

inputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '1_segments/')

outputDir <- paste0(inputDir, '3_information/')


#### 
intake.data <- read.csv(paste0(outputDir,'1_information_intakes.csv'))

pumping.intakes <- intake.data %>% 
  filter(intake.type == 'Pumping station')

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
      maximum.elevation <- max(segment.elevation.data$elevation.aster.m)

      pumping.lift <- maximum.elevation - intake.elevation

      segment.length <- unique(segment.elevation.data$segment.length.km)

      pumping.df <- data.frame(
        unique(segment.elevation.data$segment.full),
        unique(segment.elevation.data$section.id),
        unique(segment.elevation.data$segment.id),
        reservoir.id.df$reservoir.id,
        intake.type,
        intake.elevation,
        maximum.elevation,
        pumping.lift,
        segment.length
        )
     
      colnames(pumping.df) <- c('segment.full', 'section.id', 'segment.id', 'reservoir.id',
                                'intake.type','intake.elevation',
                                'maximum.elevation',
                                'pumping.lift', 'segment.length')
      
      
      list.segments[[k]] <- pumping.df 
         
    }
    
    segments.pumping.df <- do.call(rbind, list.segments)
    
    segments.pumping.df <- segments.pumping.df %>% 
      mutate(transfer.name = transfer.name.j) %>% 
      relocate(transfer.name, .before = segment.full)
    
    list.transfers[[j]] <- segments.pumping.df
      
  }
  
  country.transfers.df <- do.call(rbind, list.transfers)
  
  country.transfers.df <- country.transfers.df %>% 
    mutate(Country = country) %>% 
    relocate(Country, .before = transfer.name)
  
  list.countries[[i]] <- country.transfers.df
  
}

pumping.dataframe.df <- do.call(rbind, list.countries)

write.csv(pumping.dataframe.df, paste0(outputDir, '2_1_information_pumping.csv'), row.names = F)
