#script to place aqueducts and tunnel on transfer paths
#and interpolate elevation between beginning and end of infrastructure
#bypass of mountains and valleys

library(dplyr)
library(sf)
library(geosphere)
library(tidyr)
library(zoo)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDirIBWT <- 
  '../../../../input/water_abstraction/ibwt/0_elevation_profiles/Transfer_segments/'

inputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '1_ibwt_starting_points/')

outputDir <- paste0(inputDir, '2_elevation_with_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)


#### processing ####

n.countries <- list.dirs(inputDirElevation, full.names = F, recursive = F)

for(i in seq(length(n.countries))){
  
  country <- n.countries[i] 
  
  #create output directory for the country
  outputDirCountry <- paste0(outputDir, country, '/')
  dir.create(outputDirCountry, showWarnings = F, recursive = T)
  
  n.transfers <- list.dirs(paste0(inputDirElevation, country, '/'), full.names = F, recursive = F)
  
  #### loop per transfer
  for(j in seq(length(n.transfers))){
    
    transfer.name <- n.transfers[j]
    
    #create output directory for the transfer
    outputDirTransfer <- paste0(outputDirCountry, transfer.name, '/')
    dir.create(outputDirTransfer, showWarnings = F, recursive = T)
    
    # load infrastructure points of transfer
    infrastructure.points <- st_read(paste0(inputDirIBWT,
                                            country, '/', transfer.name, '/',
                                            'infrastructure/',
                                            transfer.name, '.shp'),
                                     quiet = T)
    
    #filter tunnels and aqueducts
    tunnels.aqueducts.points <- infrastructure.points %>% 
      filter(Subject == 'Tunnel begin' | Subject == 'Tunnel end' |
               Subject == 'Aqueduct begin' | Subject == 'Aqueduct end')
    
    #### get elevation data of all transfer segments ####
    segments.folder <- paste0(inputDirElevation, country, '/', 
                              transfer.name, '/')
    
    segments.n <- list.files(segments.folder, full.names = F)
    
    for(k in seq(length(segments.n))){
      
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name, ' - ',
                            'Segment: ', k, '/', length(segments.n)))
      
      
      #### read natural elevation
      segment.elevation.data <- read.csv(paste0(segments.folder, segments.n[k]))
      
      segment.name <- unique(segment.elevation.data$segment.full)
      
      # if there are no tunnels or aqueducts -> elevation is same as natural
      # therefore save original output
      if(nrow(tunnels.aqueducts.points) == 0){

        infrastructure.output <- segment.elevation.data %>%
          mutate(infrastructure = 0)

        write.csv(infrastructure.output, paste0(outputDirTransfer, segments.n[k]), row.names = F)

      }
      

      # otherwise match to path and interpolate elevations 
      # between beginning and end of paths
      else{
      
        #get coordinates of tunnels and aqueducts to match on elevation profile
        infrastructure.coordinates.list <- tunnels.aqueducts.points$geometry
        infrastructure.coordinates <- data.frame(
          do.call(rbind, infrastructure.coordinates.list)) %>%
          select(X1, X2) %>%
          rename(lon = X1, lat = X2)

        infrastructure.points.df <- data.frame(
          tunnels.aqueducts.points$Subject,
          tunnels.aqueducts.points$Segment,
          infrastructure.coordinates)

        colnames(infrastructure.points.df)[1:2] <-
          c('Subject', 'Segment')
        
        #get intake information
        segment.intake <- segment.elevation.data[1,]
        
        #remove first point to avoid matching with intake type
        segment.for.infrastructure <- segment.elevation.data[
          2:nrow(segment.elevation.data),] %>% 
          select(-Subject)

        tunnels.aqueducts.segment <- infrastructure.points.df %>% 
          filter(Segment == segment.name)
        
        if(nrow(tunnels.aqueducts.segment) == 0){
          
          infrastructure.output <- segment.elevation.data %>%
            mutate(infrastructure = 0)
          
          write.csv(infrastructure.output, paste0(outputDirTransfer, segments.n[k]), row.names = F)
          
        }
        
        
        else{
          
          list.match <- list()
        
          #place tunnels and aqueducts on path
          for(l in seq(nrow(tunnels.aqueducts.segment))){
            
            point.select <- tunnels.aqueducts.segment[l,]
            
            #match with closest point on transfer path
            #get coordinates of infrastructure points (to be matched on transfer )
            coord.target <- cbind(point.select$lon,
                                  point.select$lat)
            
            #get coordinates of all segment pieces (destination match)
            coord.path <- cbind(segment.for.infrastructure$lon, 
                                segment.for.infrastructure$lat)
            
            #calculate distance between all coordinates to find the minimums
            infrastructure.distances <- data.frame(
              distm(coord.target, coord.path,  fun = distHaversine))
            
            #get minimum distance amongst all transfer points
            dist.min <- apply(infrastructure.distances, 1, FUN = min)
            
            #get index of minimum distance
            #add one as first row was removed (segment intake)
            dist.min.idx <- which(infrastructure.distances == dist.min)
            
            match.idx <- dist.min.idx + 1
            
            if(length(match.idx > 1)){match.idx <- match.idx[1]}
            
            list.match[[l]] <- match.idx
            
          }
          
          matches.idxs <- do.call(rbind, list.match)
          
          #match tunnels, aqueducts onto transfer path
          infrastructure.seg.df <- data.frame(tunnels.aqueducts.segment, matches.idxs) %>% 
            rename(point.id.new = matches.idxs) %>% 
            arrange(point.id.new)
          
          #place tunnels and aqueducts
          infrastructure.elevation.df <- merge(segment.elevation.data %>% 
                                                 select(-Subject),
                                               infrastructure.seg.df %>% 
                                                 select(point.id.new, Subject), all = T )
          
          #re-add intake type
          infrastructure.elevation.df$Subject[1] <- segment.intake$Subject
          
          #NOW polish elevation between tunnel/aqueduct begin and end
          #interpolate between elevations at start and end point
          
          infrastructure.elevation.df <- infrastructure.elevation.df %>% 
            #create dummy variable to bypass natural elevation
            mutate(boolean.fill = case_when(
              Subject == 'Tunnel begin'  ~ 1, 
              Subject == 'Tunnel end'  ~ 0, 
              Subject == 'Aqueduct begin'  ~ 1, 
              Subject == 'Aqueduct end'  ~ 0))
          
          
          #add boolean at intake
          infrastructure.elevation.df$boolean.fill[1] <- 0
          
          #fill boolean to later remove natural elevation along tunnels/aqueducts
          infrastructure.elevation.df <- infrastructure.elevation.df %>% 
            #fill dummy variable
            fill(boolean.fill) %>% 
            #create dummy elevation
            mutate(elevation.infrastructure = elevation.aster.m) 
          
          #remove natural elevation from tunnels / aqueducts
          infrastructure.elevation.df$elevation.infrastructure[infrastructure.elevation.df$boolean.fill == 1] <- NA
          
          #interpolate elevation at tunnels / aqueducts
          elevation.interpolated <- na.approx(infrastructure.elevation.df$elevation.infrastructure)
          
          infrastructure.output <- cbind(infrastructure.elevation.df, elevation.interpolated)
          infrastructure.output$elevation.interpolated <- 
            round(infrastructure.output$elevation.interpolated) 
          
          infrastructure.output$infrastructure <- 1
          
          write.csv(infrastructure.output, paste0(outputDirTransfer, segments.n[k]), row.names = F)
        
        }
      }
    }
  }
}
