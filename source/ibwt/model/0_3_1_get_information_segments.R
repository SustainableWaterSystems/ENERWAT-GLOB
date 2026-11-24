rm(list=ls())

#gather information on the intake point type for each segment

library(dplyr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../output/ibwt/model/0_elevation_profiles/'

inputDirElevation <- paste0(inputDir, '1_segments/')

outputDir <- paste0(inputDir, '3_information/')
dir.create(outputDir, recursive = T, showWarnings = F)

###
sections.information <- read.csv(paste0(outputDir, '0_0_sections_reservoirs.csv'))
sections.unmatched <- read.csv(paste0(outputDir, '0_1_sections_unmatched.csv'))

#### process ####
parallel.sections.count <- sections.information %>% 
  filter(Section.Parallel == 1) %>% 
  group_by(reservoir_ID) %>% 
  summarise(count=n())

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
    
    transfer.sections <- sections.information %>% 
      filter(ibwt.branch == transfer.name)
    
    list.segments <- list()
    
    for(k in seq(length(segments.n))){
      
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name, ' - ',
                            'Segment: ', k, '/', length(segments.n)))
      
      segment.elevation.data <- read.csv(paste0(segments.folder, segments.n[k]))
      
      intake.type <- segment.elevation.data$Subject[1]
      reservoir.id <- transfer.sections$reservoir_ID[
        which(transfer.sections$Section == unique(segment.elevation.data$section.id))]
      
      segment.parallel <- transfer.sections$Section.Parallel[
        which(transfer.sections$Section == unique(segment.elevation.data$section.id))]
      
      section.paralell.count <- parallel.sections.count$count[
        which(parallel.sections.count$reservoir_ID == reservoir.id)]
      
      
      if(length(segment.parallel)==0){segment.parallel=0}
      if(length(section.paralell.count)==0){section.paralell.count=0}

      segment.info.df <- data.frame(
        unique(segment.elevation.data$segment.full),
        unique(segment.elevation.data$section.id),
        unique(segment.elevation.data$segment.id),
        intake.type,
        round(segment.elevation.data$lon[1], 2),
        round(segment.elevation.data$lat[1], 2),
        round(unique(segment.elevation.data$segment.length.km),2),
        segment.parallel,
        section.paralell.count
        )
     
      colnames(segment.info.df) <- c('segment.full', 'section.id', 'segment.id', 
                                'intake.type','intake.lon','intake.lat', 'segment.length.km',
                                'segment.parallel', 'parallel.count')
      
      list.segments[[k]] <- segment.info.df 
      
      
         
    }
    
    
    segments.transfer.df <- do.call(rbind, list.segments)
    
    segments.transfer.df <- segments.transfer.df %>% 
      mutate(transfer.name = transfer.name) %>% 
      relocate(transfer.name, .before = segment.full)
    
    list.transfers[[j]] <- segments.transfer.df
      
  }
  
  country.transfers.df <- do.call(rbind, list.transfers)
  
  country.transfers.df <- country.transfers.df %>% 
    mutate(Country = country) %>% 
    relocate(Country, .before = transfer.name)
  
  list.countries[[i]] <- country.transfers.df
  
}

intake.dataframe.df <- do.call(rbind, list.countries)

section.info.sel <- sections.information %>% 
  select(Country, ibwt.project, ibwt.branch, initial.year, 
         Section, reservoir_ID, Purpose.main,
         lon, lat) %>% 
  rename(transfer.name = ibwt.branch,
         section.id = Section,
         reservoir.id = reservoir_ID,
         reservoir.lon = lon,
         reservoir.lat = lat) 

intake.info.reservoir <- inner_join(intake.dataframe.df, section.info.sel)

write.csv(intake.info.reservoir, paste0(outputDir, '1_information_intakes.csv'), row.names = F)

####
sections.length <- intake.info.reservoir %>% 
  filter(Country != 'Libya') %>% 
  group_by(Country, transfer.name, section.id) %>% 
  summarise(section.length = sum(segment.length.km))

summary(sections.length$section.length)
