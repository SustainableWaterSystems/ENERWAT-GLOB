rm(list=ls())

#gather information on the intake point type for each segment

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../output/ibwt/model/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '2_infrastructure/')

outputDir <- paste0(inputDir, '3_information/')
dir.create(outputDir, recursive = T, showWarnings = F)

###
sections.information <- read.csv(paste0(outputDir, '0_0_sections_reservoirs.csv'))
sections.unmatched <- read.csv(paste0(outputDir, '0_1_sections_unmatched.csv'))

#### process ####

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
      
      segment.section <- segment.elevation.data$section.id[1]
      
      segment.name <- segment.elevation.data$segment.full[1]
      
      segment.df <- data.frame(segment.section, segment.name, segment.bypasses.n,
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
  section.name <- tunnels.info$segment.section[i]
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
    
    #get minimum as well (to avoid conflicts with aqueducts dataframe)
    #aqueducts will need minimum elevation 
    elevation.og.min <- min(tunnel.segment$elevation.aster.m)
    
    #calculate bypassed elevation
    net.elevation.bypass <- elevation.og.max - elevation.tunnel.begin
    
    tunnel.length <- tunnel.segment$distance.km.total[nrow(tunnel.segment)] -
      tunnel.segment$distance.km.total[1]
    
    intake.lon <- tunnel.segment$lon[1]
    intake.lat <- tunnel.segment$lat[1]
    
    tunnel.df <- data.frame(country,
                            transfer.name,
                            section.name,
                            segment.name,
                            segment.elevation.data$segment.full[1],
                            'Tunnel',
                            j,
                            intake.lon,
                            intake.lat,
                            tunnel.length,
                            elevation.tunnel.begin,
                            elevation.og.max,
                            elevation.og.min,
                            net.elevation.bypass)
    
    colnames(tunnel.df) <- c('Country','transfer.name','section.id',
                             'segment.name', 'segment.id','bypass.type', 'bypass.id',
                             'intake.lon','intake.lat',
                             'bypass.length.km','elevation.intake','elevation.max',
                             'elevation.min',
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
  section.name <- aqdcts.info$segment.section[i]
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
    
    elevation.og.min <- min(tunnel.segment$elevation.aster.m)
    
    #calculate bypassed elevation at queducts
    #minimum point in the valley is required
    net.elevation.bypass <- elevation.og.max - elevation.og.min
    
    tunnel.length <- tunnel.segment$distance.km.total[nrow(tunnel.segment)] -
      tunnel.segment$distance.km.total[1]
    
    intake.lon <- tunnel.segment$lon[1]
    intake.lat <- tunnel.segment$lat[1]
    
    tunnel.df <- data.frame(country,
                            transfer.name,
                            section.name,
                            segment.name,
                            segment.elevation.data$segment.full[1],
                            'Aqueduct',
                            j,
                            intake.lon,
                            intake.lat,
                            tunnel.length,
                            elevation.tunnel.begin,
                            elevation.og.max,
                            elevation.og.min,
                            net.elevation.bypass)
    
    colnames(tunnel.df) <- c('Country','transfer.name','section.id',
                             'segment.name', 'segment.id','bypass.type', 'bypass.id',
                             'intake.lon','intake.lat',
                             'bypass.length.km','elevation.intake','elevation.max',
                             'elevation.min',
                             'net.bypass')
    
    aqdcts.info.list[[j]] <- tunnel.df 
    
  }
  
  tunnels.info.segment <- do.call(rbind, aqdcts.info.list)
  
  all.info.list[[i]] <- tunnels.info.segment
  
}

aqdcts.info.df <- do.call(rbind, all.info.list)

#### ####

bypass.info.df <- rbind(tunnel.info.df, aqdcts.info.df)

section.info.sel <- sections.information %>%
  select(Country, ibwt.project.name, Section, reservoir_ID, Purpose.main) %>%
  rename(transfer.name = ibwt.project.name,
         section.id = Section,
         reservoir.id = reservoir_ID)

bypass.reservoirs.df <- inner_join(bypass.info.df, section.info.sel)

write.csv(
  bypass.reservoirs.df, paste0(outputDir, '2_information_bypass.csv'),
  row.names = F)