#pumping lift height is calculated as 
#elevation of highest point on the transfer path 
#minus
#elevation of intake point

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep("                                                                                   ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '2_elevation_with_infrastructure/')

outputDir <- paste0(inputDir, '3_pumping_elevations/')

#### first collect information dataframe on quantity of bypasses per segment
n.countries <- list.dirs(inputDirElevation, full.names = F, recursive = F)

list.countries <- list()



for(i in seq(length(n.countries))){
  
  replaceMessage('Gathering bypass information...')
  
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
      
      segment.elevation.data <- read.csv(paste0(segments.folder, segments.n[k]))
      
      segment.bypasses.n <- sum(segment.elevation.data$Subject == 'Aqueduct begin', na.rm = T) +
        sum(segment.elevation.data$Subject == 'Tunnel begin', na.rm = T)
      
      tunnels.n <- sum(segment.elevation.data$Subject == 'Tunnel begin', na.rm = T)
      aqdcts.n <- sum(segment.elevation.data$Subject == 'Aqueduct begin', na.rm = T)
      
      segment.name <- segment.elevation.data$segment.full[1]
      
      segment.df <- data.frame(segment.name, segment.bypasses.n,
                               tunnels.n, aqdcts.n)
      
      list.segments[[k]] <- segment.df
      
    }
    
    all.segments.df <- do.call(rbind, list.segments) %>% 
      mutate(transfer.name = transfer.name)
    
    list.transfers[[j]] <- all.segments.df
    
  }
  
  transfer.df <- do.call(rbind, list.transfers) %>% 
    mutate(Country = country)
  
  list.countries[[i]] <- transfer.df
  
}

segments.bypass.info <- do.call(rbind, list.countries)

# break

#### tunnels firts ####

tunnels.info <- segments.bypass.info %>% 
  filter(tunnels.n > 0)

aqdcts.info <- segments.bypass.info %>% 
  filter(aqdcts.n > 0)

sum(tunnels.info$tunnels.n)

#########

all.info.list <- list()

for(i in seq(nrow(tunnels.info))){
  
  replaceMessage('Gathering tunnels specifics...')
  
  country <- tunnels.info$Country[i]
  transfer.name <- tunnels.info$transfer.name[i]
  segment.name <- tunnels.info$segment.name[i]
    
  segments.folder <- paste0(inputDirElevation, country, '/', transfer.name, '/')
  
  segment.elevation.data <- read.csv(paste0(segments.folder,
                                            'segment_', 
                                            segment.name,'.csv'))


  #### get locations of tunnels and aqueducts ####
  tnn.b.idx <- which(segment.elevation.data$Subject == 'Tunnel begin')
  tnn.e.idx <- which(segment.elevation.data$Subject == 'Tunnel end')
  
  tunnels.info.list <- list()
  
  for(j in seq(tunnels.info$tunnels.n[i])){
    
    #take segment part between beginning and end of tunnel
    tunnel.segment <- segment.elevation.data[tnn.b.idx[j]:tnn.e.idx[j],]

    #get elevation of tunnel at its first point
    #to imagine if it was a pumping station
    elevation.tunnel.begin <- tunnel.segment$elevation.aster.m[1]

    #take maximum elevation of natural elevation
    #to calculate the total elevation bypassed
    elevation.og.max <- max(tunnel.segment$elevation.aster.m)

    #calculate bypassed elevation
    net.elevation.bypass <- elevation.og.max - elevation.tunnel.begin

    tunnel.length <- tunnel.segment$distance.km.total[nrow(tunnel.segment)] -
      tunnel.segment$distance.km.total[1]

    tunnel.df <- data.frame(country,
                            transfer.name,
                            segment.name,
                            segment.elevation.data$segment.full[1],
                            'Tunnel',
                            j,
                            tunnel.length,
                            elevation.tunnel.begin,
                            elevation.og.max,
                            net.elevation.bypass)

    colnames(tunnel.df) <- c('Country','transfer.name','segment.name', 'segment.id','bypass.type', 'bypass.id',
                             'bypass.length.km','elevation.intake','elevation.max',
                             'net.bypass')
    
    tunnels.info.list[[j]] <- tunnel.df 
    
  }
  
  tunnels.info.segment <- do.call(rbind, tunnels.info.list)
  
  all.info.list[[i]] <- tunnels.info.segment
  
}

tunnel.info.df <- do.call(rbind, all.info.list)


#### aqueducts ####

all.info.list <- list()

for(i in seq(nrow(aqdcts.info))){
  
  replaceMessage('Gathering aqueduct specifics...')
  
  country <- aqdcts.info$Country[i]
  transfer.name <- aqdcts.info$transfer.name[i]
  segment.name <- aqdcts.info$segment.name[i]
  
  segments.folder <- paste0(inputDirElevation, country, '/', transfer.name, '/')
  
  segment.elevation.data <- read.csv(paste0(segments.folder,
                                            'segment_', 
                                            segment.name,'.csv'))
  
  
  #### get locations of tunnels and aqueducts ####
  tnn.b.idx <- which(segment.elevation.data$Subject == 'Aqueduct begin')
  tnn.e.idx <- which(segment.elevation.data$Subject == 'Aqueduct end')
  
  aqdcts.info.list <- list()
  
  for(j in seq(aqdcts.info$aqdcts.n[i])){
    
    #take segment part between beginning and end of tunnel
    tunnel.segment <- segment.elevation.data[tnn.b.idx[j]:tnn.e.idx[j],]
    
    #get elevation of tunnel at its first point
    #to imagine if it was a pumping station
    elevation.tunnel.begin <- tunnel.segment$elevation.aster.m[1]
    
    #take maximum elevation of natural elevation
    #to calculate the total elevation bypassed
    elevation.og.max <- max(tunnel.segment$elevation.aster.m)
    
    #calculate bypassed elevation
    net.elevation.bypass <- elevation.og.max - elevation.tunnel.begin
    
    tunnel.length <- tunnel.segment$distance.km.total[nrow(tunnel.segment)] -
      tunnel.segment$distance.km.total[1]
    
    tunnel.df <- data.frame(country,
                            transfer.name,
                            segment.name,
                            segment.elevation.data$segment.full[1],
                            'Aqueduct',
                            j,
                            tunnel.length,
                            elevation.tunnel.begin,
                            elevation.og.max,
                            net.elevation.bypass)
    
    colnames(tunnel.df) <- c('Country','transfer.name','segment.name', 'segment.id','bypass.type', 'bypass.id',
                             'bypass.length.km','elevation.intake','elevation.max',
                             'net.bypass')
    
    aqdcts.info.list[[j]] <- tunnel.df 
    
  }
  
  tunnels.info.segment <- do.call(rbind, aqdcts.info.list)
  
  all.info.list[[i]] <- tunnels.info.segment
  
}

aqdcts.info.df <- do.call(rbind, all.info.list)

#### ####

bypass.info.df <- rbind(tunnel.info.df, aqdcts.info.df)


write.csv(
  bypass.info.df, paste0(outputDir, 'bypass_information.csv'),
  row.names = F)


