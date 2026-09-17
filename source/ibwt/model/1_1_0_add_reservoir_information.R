rm(list=ls())

library(dplyr)
library(lubridate)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../output/ibwt/model/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/0_timeseries_pcrglobwb/1_filtered/')

outputDirData <- paste0(inputDir, '1_discharge_timeseries/1_bias_corrected/')
dir.create(outputDirData, recursive = T, showWarnings = F)

#### ####
data.sections <- read.csv(paste0(inputDirElevation, '0_0_sections_reservoirs.csv'))
data.segments <- read.csv(paste0(inputDirElevation, '1_information_intakes.csv'))


#### collect average discharge 1979-2023 for each section ####
section.info.list <- list()

for(i in seq(nrow(data.sections))){
  
  section.select <- data.sections[i,]
  
  section.segments <- data.segments %>% 
    filter(reservoir.id == section.select$reservoir_ID,
           section.id == section.select$Section)
  
  #### types of intake per section
  intake.types.section <- unique(section.segments$intake.type)
  
  #### assign ids to intake mixes
  if(length(intake.types.section) == 1){
    
    if(intake.types.section == 'Power station'){
      intake.type.id <- 1
    }
    
    else if(intake.types.section == 'Pumping station'){
      intake.type.id <- 2
    }
    
    else if(intake.types.section == 'Reservoir intake' | 
            intake.types.section == 'River intake' ){
      intake.type.id <- 3
    }
    
  }
  
  else if(length(intake.types.section) > 1){
    
    if(all(c('Pumping station','Power station') %in% intake.types.section)==T){
      intake.type.id <- 4
    }
    
    else(intake.type.id <- 5)
    
  }
  
  #### discharge
  initial.year.transfer.date <- as.Date(paste(section.select$initial.year, 1, 1, sep = "-"))
  
  #### make avg discharge dataframe
  
  section.discharge.info <- section.select %>% 
    mutate(
           intake.type.id = intake.type.id
           ) %>% 
    rename(transfer.name = ibwt.branch,
           reservoir.id = reservoir_ID,
           initial.year.transfer = initial.year)
  
  section.info.list[[i]] <- section.discharge.info
  
}

section.info.df <- do.call(rbind, section.info.list) %>% 
  mutate(intake.type.string = case_when(intake.type.id == 1 ~ "Hydropower only",
                                        intake.type.id == 2 ~ "Pumping only",
                                        intake.type.id == 3 ~ "Discharge only",
                                        intake.type.id == 4 ~ "Mixed energy",
                                        intake.type.id == 5 ~ "Mixed energy + Discharge only")) #%>% 
  # mutate(section.id.unique = seq(nrow(data.sections))) %>% 
  # relocate(section.id.unique, .before=Country)

#add unique id to sections
transfer.reservoirs <- section.info.df %>% 
  group_by(transfer.name, reservoir.id) %>% 
  summarise(count=n()) %>% 
  select(-count)

id.vector <- data.frame(seq(nrow(transfer.reservoirs)))
colnames(id.vector) <- 'section.id.unique'

transfer.reservoirs.unique <- cbind(id.vector, transfer.reservoirs)

data.sections.unique <- inner_join(section.info.df, transfer.reservoirs.unique) %>% 
 relocate(section.id.unique, .after=reservoir.id)

#add unique id to segments
data.segments.unique <- data.segments %>% 
  mutate(segment.id.unique = seq(nrow(data.segments))) %>% 
  relocate(segment.id.unique, .before=Country)

#### save ####
write.csv(data.sections.unique, paste0(outputDirData, '0_information_sections.csv'), row.names = F)
write.csv(data.segments.unique, paste0(outputDirData, '0_information_segments.csv'), row.names = F)
