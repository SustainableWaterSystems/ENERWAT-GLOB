rm(list=ls())

library(dplyr)
library(geosphere)
library(data.table)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
inputDirElevation <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/1_segments/'
inputDirHeads <- '../../../../input/global_data/ibwt/Shah2025_GloHydroRes/'

outputDir <- paste0(inputDir, '2_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)

#### load 
shah2025 <- read.csv(paste0(inputDirHeads, 'GloHydroRes_vs1.csv'))

data.sections <- read.csv(paste0(inputDir,'1_bias_corrected/1_0_bias_correction_sections.csv'))
data.segments <- read.csv(paste0(inputDir,'1_bias_corrected/1_1_bias_correction_segments.csv'))

power.intakes <- data.segments %>% 
  filter(intake.type == 'Power station')


#### process ####
### change USA naming in Shah
shah2025$country[which(shah2025$country == 'United States of America')] <- 'USA'

#get ibwt countries
countries.ibwt <- unique(power.intakes$Country)

list.country.matches <- list()
for(i in seq(length(countries.ibwt))){
  
  #select country
  country.sel <- countries.ibwt[i]
  
  #filter ibwt and shah datasets for that country
  country.ibwt <- power.intakes %>% 
    filter(Country == country.sel)
  
  country.shah <- shah2025 %>% 
    filter(country == country.sel)
    
  ### get coordinates of both datasets
  coordinates.ibwt <- country.ibwt %>% 
    select(intake.lon, intake.lat)
  
  coordinates.shah <- country.shah %>% 
    select(plant_lon, plant_lat)

  #### calculate distances haversine
  country.distances <- distm(coordinates.ibwt, coordinates.shah, fun = distHaversine)
  
  # get minimum distance
  distances.min <- apply(country.distances, 1, FUN = min)
  
  list.matches <- list()
  #get match per power plant
  for(j in seq(nrow(country.ibwt))){
    
    #get distances for single power plant
    distance.vector <- data.frame(matrix(country.distances[j,]))
    colnames(distance.vector) <- 'distance.m'
    
    #get index of match in shah dataset
    country.idx <- which(distance.vector$distance.m == distances.min[j])
    
    #get plant data from shah 
    shah.match <- country.shah[country.idx,] %>%
      select(ID, name, year, capacity_mw, plant_type, 
             dam_height_m, head_m,
             plant_lat, plant_lon) %>% 
      mutate(distance.match = distances.min[j])
    
    colnames(shah.match) <- c('id.shah2025','plant.name', 'year.shah2025', 'capacity.mw',
                              'plant.type', 'dam.height.m','head.m',
                              'lat.shah2025','lon.shah2025', 'match.distance.m'
                              )
    
    list.matches[[j]] <- shah.match
    
    
  }
  
  #unlist shah matches 
  shah.matches.country <- do.call(rbind, list.matches) 
  
  #add information to original dataframe
  country.ibwt.match <- cbind(country.ibwt, shah.matches.country)
  
  list.country.matches[[i]] <- country.ibwt.match
  
}

ibwt.shah.df <- do.call(rbind, list.country.matches)

####  if head does is not reported use dam height ####
#### if both head and dam height dont exist -> remove hydropower plant ?
ibwt.shah.df.heads <- ibwt.shah.df %>%
  filter(!is.na(dam.height.m) | !is.na(head.m)) 

ibwt.shah.df.heads$head.m[is.na(ibwt.shah.df.heads$head.m)] <- 
  ibwt.shah.df.heads$dam.height[is.na(ibwt.shah.df.heads$head.m)]


write.csv(ibwt.shah.df.heads, paste0(outputDir, '0_information_hydropower.csv'), row.names = F)
