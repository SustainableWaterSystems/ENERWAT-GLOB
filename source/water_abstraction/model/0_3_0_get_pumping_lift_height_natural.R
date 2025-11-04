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

inputDirElevation <- paste0(inputDir, '1_ibwt_starting_points/')

outputDir <- paste0(inputDir, '3_pumping_elevations/')
dir.create(outputDir, recursive = T, showWarnings = F)

n.countries <- list.dirs(inputDirElevation, full.names = F, recursive = F)

list.countries <- list()

for(i in seq(length(n.countries))){
  
  country <- n.countries[i] 
  
  n.transfers <- list.dirs(paste0(inputDirElevation, country, '/'), full.names = F, recursive = F)
  
  list.transfers <- list()
  
  #### loop per transfer
  for(j in seq(length(n.transfers))){
    
    transfer.name <- n.transfers[j]
    
    #### get elevation data of all transfer segments ####
    segments.folder <- paste0(inputDirElevation, country, '/', 
                              transfer.name, '/')
    
    segments.n <- list.files(segments.folder, full.names = F)
    
    list.segments <- list()
    
    for(k in seq(length(segments.n))){
      
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name, ' - ',
                            'Segment: ', k, '/', length(segments.n)))
      
      segment.elevation.data <- read.csv(paste0(segments.folder, segments.n[k]))
      
      intake.type <- segment.elevation.data$Subject[1]
      
      intake.elevation <- segment.elevation.data$elevation.aster.m[
        which(!is.na(segment.elevation.data$Subject))
      ]
      
      #### get various statistics of the segment
      maximum.elevation <- max(segment.elevation.data$elevation.aster.m)
      
      pumping.lift <- maximum.elevation - intake.elevation
      
      segment.length <- unique(segment.elevation.data$segment.length.km)
      
      maximum.location <- which(
        segment.elevation.data$elevation.aster.m == maximum.elevation)
      
      if(length(maximum.location)>1){
        maximum.location <- maximum.location[length(maximum.location)]
      }
      
      distance.to.maximum <- segment.elevation.data$distance.km.total[maximum.location]
      
      pumping.df <- data.frame(
        unique(segment.elevation.data$segment.full),
        unique(segment.elevation.data$section.id),
        unique(segment.elevation.data$segment.id),
        intake.type,
        intake.elevation,
        maximum.elevation,
        pumping.lift,
        segment.length,
        segment.elevation.data$lon[1],
        segment.elevation.data$lat[1],
        segment.elevation.data$lon[maximum.location],
        segment.elevation.data$lat[maximum.location],
        distance.to.maximum
        
        )
     
      colnames(pumping.df) <- c('segment.full', 'section.id', 'segment.id',
                                'intake.type','intake.elevation','maximum.elevation',
                                'pumping.lift', 'segment.length', 
                                'intake.lon','intake.lat',
                                'maximum.height.lon','maximum.height.lat', 
                                'distance.to.maximum')
      
      
      list.segments[[k]] <- pumping.df 
         
    }
    
    segments.pumping.df <- do.call(rbind, list.segments)
    
    segments.pumping.df <- segments.pumping.df %>% 
      mutate(transfer.name = transfer.name) %>% 
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

write.csv(pumping.dataframe.df, paste0(outputDir, 'intake_information.csv'), row.names = F)
