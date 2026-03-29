rm(list=ls())

#### script to remove pixels that do not have either a plant or a volume 
#### increases speed later 

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

#### paths ####
#input/output paths
inputDir <- 
  '../../../../output/wtt/model/wwt/0_volumes/'

inputDirHybas <- paste0(inputDir, '0_raw/1_hybas_higher_levels/') 

inputDirNonMatching <- paste0(inputDir, '0_raw/2_non_matching_countries/') 

outputDirInput <- paste0(inputDir, '1_flows_model_input/model_input/')

outputDirRemoved <-  paste0(inputDir, '1_flows_model_input/removed_data/')

#non matching countries 
nonMatchingCountries <- read.csv(paste0(inputDirNonMatching, 'global.csv'))

#continents and upper levels of hydroBASINS
hybas_continents <- c('af','ar', 'as','au','eu', 'na','sa','si')
hybas_levels <- c('02','03','04','05','06','07','08','09','10') 


#### loop  #### 
# common.cell.ids.continent <- list()

for(hybas_continent in seq(1, length(hybas_continents))){

  continent <- hybas_continents[hybas_continent]

  for(level in seq(1, length(hybas_levels))){
      
    replaceMessage(
      paste0('continent ', hybas_continent, '/', length(hybas_continents),
                          ': ', continent, ' - level ', hybas_levels[level]))
    
    lev <- hybas_levels[level]
    
    continentDataPath <- paste0(inputDirHybas, continent, '/')
    
    #get continent non-matching countries 
    nonMatchingContinent <- nonMatchingCountries %>% 
      filter(hybas_continent == continent)
    
    #get raw continent plants and levels
    level.plants <- read.csv(paste0(continentDataPath, 'lev', lev, '_plants.csv'))
    level.volumes <- read.csv(paste0(continentDataPath, 'lev', lev, '_volumes.csv'))
    
    #remove plants and volumes in non matching countries 
    level.plants.filtered <- level.plants[!
      level.plants$country_ID %in% nonMatchingContinent$country_ID, ]
    level.volumes.filtered <-level.volumes[!
      level.volumes$country_ID %in% nonMatchingContinent$country_ID, ]
    
    #get removed plants and volumes 
    level.plants.removed <- setdiff(level.plants, level.plants.filtered)
    level.volumes.removed <- setdiff(level.volumes, level.volumes.filtered)
    
    #set and create continent output path
    outputDirContinentInput <- paste0(outputDirInput, continent, '/')
    outputDirContinentRemoved <- paste0(outputDirRemoved, continent, '/')
    
    dir.create(outputDirContinentInput, showWarnings = F, recursive = T)
    dir.create(outputDirContinentRemoved, showWarnings = F, recursive = T)
    
    #save things
    write.csv(level.plants.filtered, 
              paste0(outputDirContinentInput, 'lev', lev, '_input_plants.csv'),
              row.names = F)
    write.csv(level.volumes.filtered, 
              paste0(outputDirContinentInput, 'lev', lev, '_input_volumes.csv'),
              row.names = F)
    write.csv(level.plants.removed, 
              paste0(outputDirContinentRemoved, 'lev', lev, '_removed_plants.csv'),
              row.names = F)
    write.csv(level.volumes.removed, 
              paste0(outputDirContinentRemoved, 'lev', lev, '_removed_volumes.csv'),
              row.names = F)
  
  }
}
