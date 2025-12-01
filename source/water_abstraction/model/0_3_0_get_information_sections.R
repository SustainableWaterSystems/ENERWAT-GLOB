rm(list=ls())

#### script to match the GeoDAR reservoirs 
#### with a PCR-GLOBWB zone to later extract reservoir discharge

library(readxl)
library(vroom)
library(dplyr)
library(tidyr)

inputDir <- '../../../../input/water_abstraction/ibwt/'

inputDirElevation <- 
  '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/0_elevation/'

inputDirTS <- '../../../../input/water_abstraction/ibwt/1_discharge_timeseries/'

outputDir <- 
  '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/3_information/'
dir.create(outputDir, recursive = T, showWarnings = F)

#load stuff
megaprojects.outlets <-
  read_excel(paste0(inputDir, 'geodar_outlets_5arcmin_at_ibwt.xlsx'), sheet = 'Sections') %>%
  mutate_if(is.character, ~na_if(., '')) %>%
  fill(Country, ibwt.project.name, Purpose.main, Purpose.mixed, initial.year)

zones.5arcmin <- vroom(paste0(inputDirTS, 'maskGlobal.csv')) %>% 
  filter(zone_ID != 0)

reservoirs.5arcmin <- read.csv(paste0(inputDirTS, 'outlet_map_geodar_lakes_dams_unique.csv'))      


#### join reservoirs to zone ####
reservoirs.zone.5arcmin <- inner_join(
  
  reservoirs.5arcmin %>% select(cell_ID, reservoir_ID, lon, lat),
  
  zones.5arcmin %>% select(cell_ID, zone_ID)
  
)

reservoirs.in.zone <- inner_join(megaprojects.outlets %>%
                                   rename(reservoir_ID = id.geodar.v11),
                                 reservoirs.zone.5arcmin) 

reservoirs.in.zone$lon <- round(reservoirs.in.zone$lon, 2)
reservoirs.in.zone$lat <- round(reservoirs.in.zone$lat, 2)

#### reservoirs or rivers that are unmatched in PCR-GLOBWB
other.intakes <- megaprojects.outlets %>% 
  filter(!id.geodar.v11 %in% reservoirs.in.zone$reservoir_ID)

#### add beginning and end latlon of whole sections #### 

transfer.basedata <- reservoirs.in.zone

#### get countries for which I created the natural elevation profile
countries.all <- list.dirs(inputDirElevation, full.names = F, recursive = F)
countries.n <- unique(transfer.basedata$Country)

countries.complete.data.list <- list()

for(i in seq(length(countries.n))){
  
  # i=1
  country <- countries.n[i]
  
  #get country transfers
  country.transfers.sections <- transfer.basedata %>% 
    filter(Country == country) 
  
  country.transfers <- unique(country.transfers.sections$ibwt.project.name)
  
  country.list <- list()
  
  #### loop per transfer
  for(j in seq(length(country.transfers))){
    
    # j=1
    
    transfer.name <- country.transfers[j]
    
    #### get elevation data of all sections ####
    sections.folder <- paste0(inputDirElevation, country, '/', transfer.name, '/')
    sections.n <- list.files(sections.folder, full.names = F)
    
    transfer.sections.list <- list()
    
    for(k in seq(length(sections.n))){
      
      section <- k
      
      ibwt.vector.section <- read.csv(paste0(
        inputDirElevation, country, '/', transfer.name, '/', sections.n[k]))
      
      section.info.df <- data.frame(
        transfer.name,
        k, 
        round(ibwt.vector.section$lon[1],2),
        round(ibwt.vector.section$lat[1],2),
        round(ibwt.vector.section$lon[nrow(ibwt.vector.section)],2),
        round(ibwt.vector.section$lat[nrow(ibwt.vector.section)],2)
      )
      
      colnames(section.info.df) <- 
        c('ibwt.project.name','Section',
          'from.lon', 'from.lat', 'to.lon','to.lat')
      
      transfer.sections.list[[k]] <- section.info.df
      
    }
    
    transfer.sections.data <- do.call(rbind, transfer.sections.list)
    
    country.list[[j]] <- transfer.sections.data
    
  }
  
  country.all.transfers <- do.call(rbind, country.list)
  
  countries.complete.data.list[[i]] <- country.all.transfers
  
}

countries.complete.data <- do.call(rbind, countries.complete.data.list)

plot.df <- merge(transfer.basedata, countries.complete.data) %>% 
  relocate(Country, .before=ibwt.project.name) %>% 
  arrange(Country)


write.csv(plot.df, paste0(outputDir, '0_0_sections_reservoirs.csv'), row.names = F)
write.csv(other.intakes, paste0(outputDir, '0_1_sections_unmatched.csv'), row.names = F)
