rm(list=ls())

#### script to get the type of intake at the first point of the ibwt segment

library(readxl)
library(dplyr)
library(terra)
library(sf)
library(geosphere)
library(tidyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

#### directories 
inputDir <- '../../../../input/water_abstraction/ibwt/'

inputDirIBWT <- paste0(inputDir, '0_elevation_profiles/Transfer_segments/')

outputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'

inputDirElevation <- paste0(outputDir, '0_elevation/')

outputDirPumping <- paste0(outputDir, '1_segments/')


#### loop by country ####
n.countries <- list.dirs(inputDirIBWT, full.names = F, recursive = F)

for(i in seq(length(n.countries))){
  
  country <- n.countries[i]
  
  #create output directory for the country
  outputDirCountry <- paste0(outputDirPumping, country, '/')
  dir.create(outputDirCountry, showWarnings = F, recursive = T)
  
  #### loop by transfers (can be multiple in the same country)
  country.transfers <- list.dirs(paste0(inputDirIBWT, country),
                                 full.names = F, recursive = F)
  
  #### loop per transfer
  for(j in seq(length(country.transfers))){
    
    transfer.name <- country.transfers[j]
    
    #create output directory for the transfer
    outputDirTransfer <- paste0(outputDirCountry, transfer.name, '/')
    dir.create(outputDirTransfer, showWarnings = F, recursive = T)
    
    
    # load infrastructure points of transfer
    infrastructure.points <- st_read(paste0(inputDirIBWT,
                                            country, '/', transfer.name, '/',
                                            'infrastructure/',
                                            transfer.name, '.shp'))
    
    infrastructure.coordinates.list <- infrastructure.points$geometry
    infrastructure.coordinates <- data.frame(
      do.call(rbind, infrastructure.coordinates.list)) %>%
      select(X1, X2) %>%
      rename(lon = X1, lat = X2) 
    
    infrastructure.points.df <- data.frame(
      infrastructure.points$Subject,
      infrastructure.points$Segment,
      infrastructure.coordinates
      
    )
    
    colnames(infrastructure.points.df)[1:2] <- 
      c('Subject', 'Segment')
    
    #### get elevation data of all transfer segments ####
    segments.folder <- paste0(inputDirElevation, country, '/', 
                              transfer.name, '/')
    
    segments.n <- list.files(segments.folder, full.names = F)
    
    
    #### loop over segments 
    for(k in seq(length(segments.n))){
      
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name, ' - ',
                            'Segment: ', k, '/', length(segments.n)))
      
      segment.elevation.data <- read.csv(paste0(segments.folder, segments.n[k]))#,
                                         # colClasses=c("segment.full"="character"))
      
      segment.name <- unique(segment.elevation.data$segment.full)
      
      #### first point of a inter-basin transfer segment can be
      # pumping station : the water is pumped uphill
      # power station : the water is used for hydropower production
      # reservoir intake : there is a reservoir in GeoDAR but no verified power station at the outlet
      # river intake : the first point simply diverts water from a river (-> no reservoir)
      
      segment.pumping.station <- infrastructure.points.df %>% 
        filter(Subject == 'Pumping station' | Subject == 'Power station' |
                 Subject == 'Reservoir intake' |
                 Subject == 'River intake' ) %>% 
        filter(Segment == segment.name)
      
      intake.type <- segment.pumping.station$Subject
      
      #match with closest point on transfer path
      #get coordinates of infrastructure points (to be matched on transfer )
      coord.target <- cbind(segment.pumping.station$lon,
                            segment.pumping.station$lat)
      
      
      #get coordinates of all segment pieces (destination match)
      coord.path <- cbind(segment.elevation.data$lon, 
                          segment.elevation.data$lat)
      
      #calculate distance between all coordinates to find the minimums
      infrastructure.distances <- data.frame(
        distm(coord.target, coord.path,  fun = distHaversine))
      
      #get minimum distance amongst all transfer points
      dist.min <- min(infrastructure.distances)
      
      #get index of minimum distance
      dist.min.idx <- which(infrastructure.distances == dist.min)
      
      if(length(dist.min.idx) > 1){dist.min.idx <- dist.min.idx[1]}
      
      if(intake.type == 'Pumping station'){
        
        dist.min.df <- data.frame(dist.min.idx) %>% 
          rename(point.id = dist.min.idx) %>%
          mutate(Subject = 'Pumping station')
        
        pumping.station.df <- merge(segment.elevation.data,
                                    dist.min.df,
                                    all=T)
        
      }
      
      else if(intake.type == 'Power station'){
        
        dist.min.df <- data.frame(dist.min.idx) %>% 
          rename(point.id = dist.min.idx) %>%
          mutate(Subject = 'Power station')
        
        pumping.station.df <- merge(segment.elevation.data,
                                    dist.min.df,
                                    all=T)
        
      }
      
      else if(intake.type == 'Reservoir intake'){
        
        dist.min.df <- data.frame(dist.min.idx) %>% 
          rename(point.id = dist.min.idx) %>%
          mutate(Subject = 'Reservoir intake')
        
        pumping.station.df <- merge(segment.elevation.data,
                                    dist.min.df,
                                    all=T)
        
      }
      
      else{
        
        dist.min.df <- data.frame(dist.min.idx) %>% 
          rename(point.id = dist.min.idx) %>%
          mutate(Subject = 'River intake')
        
        pumping.station.df <- merge(segment.elevation.data,
                                    dist.min.df,
                                    all=T)
        
      }
      
      
      #check if intake point is in first half or second half of the dataframe
      #if intake is in second half then segment coordinates must be inverted
      
      if(which(!is.na(pumping.station.df$Subject)) > (nrow(pumping.station.df)/2)){
        
        #invert
        pumping.station.df <- pumping.station.df[rev(1:nrow(pumping.station.df)), ]
        
        #select only from pumping station onwards
        pumping.station.df.sel <- pumping.station.df[
          which(!is.na(pumping.station.df$Subject)):nrow(pumping.station.df),]
        
        #recalculate total distance
        pumping.station.df.sel$distance.km.total <- cumsum(pumping.station.df.sel$distance.km.unit)
        
      }
      
      else{
        #select only from pumping station onwards
        pumping.station.df.sel <- pumping.station.df[
          which(!is.na(pumping.station.df$Subject)):nrow(pumping.station.df),]
      }
      
      point.id.new <- seq(nrow(pumping.station.df.sel))
      
      pumping.station.df.sel <- pumping.station.df.sel %>% 
        mutate(point.id.new = point.id.new) %>% 
        relocate(point.id.new, .before = point.id)
      
      write.csv(pumping.station.df.sel, paste0(outputDirTransfer, segments.n[k]), row.names= F)
      
    }
  }
}
