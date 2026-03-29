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
#input
inputDirWorld <- '../../../../input/global_data/'

inputDirCatchments <- paste0(
  inputDirWorld, 'general/HydroBASINS_5arcmin/1_hybas_higher_levels/')

#output from previous script 
inputDirVolumes <- 
  '../../../../output/wtt/model/wwt/0_volumes/0_raw/0_hybas_lev01/'

#output and create folders
outputDirNonMatching <- 
  '../../../../output/wtt/model/wwt/0_volumes/0_raw/2_non_matching_countries/'
dir.create(outputDirNonMatching, showWarnings = F, recursive = T)

#### variable sto loop ####
#country names
country.names <- read.csv(
  paste0(inputDirWorld, 'general/countries_id_regions.csv'))

#must analyse n of catchments vs. hybas_level per continent
hybas_levels <- c('02','03','04','05','06','07','08','09','10') 

#select continent
hybas_continents <- c('af','ar', 'as','au','eu', 'na','sa','si')

nonmatching.global.list <- list()

for(hybas_continent in seq(1, length(hybas_continents))){
  
  continent <- hybas_continents[hybas_continent]
  
  #set output path for continent
  outputDir <- paste0(
    '../../../../output/wtt/model/wwt/0_volumes/0_raw/1_hybas_higher_levels/', 
    continent, '/')
  
  #create output path for continent
  dir.create(outputDir, showWarnings = F, recursive = T)
  
  #### read files ####

  
  #read continent pixels and countries and wastewater volumes
  wwt.continent <- vroom(paste0(inputDirVolumes, 'wwt_', continent, '.csv'),
                         col_names = TRUE, show_col_types = FALSE)
  
  #### processing ####
  
  #remove pixels where plants and production volumes == 0 
  wwt.continent.plants <- wwt.continent[wwt.continent$wwtp != 0,]
  wwt.continent.volumes <- wwt.continent[wwt.continent$wwtv != 0,]

  
  #### merge volume file and hybas level pixel table####
  # --------------> loop down here over levels <--------------
  
  # list hybas levels
  
  #hybas level 1 is the whole continent
  #level 10 will be the starting point
  #from level 11 most catchments do not split completely in hydrobasins anymore
  
  for(lev in seq(1, length(hybas_levels))){
    
    replaceMessage(paste0('continent ', hybas_continent, '/', length(hybas_continents),
                          ': ', continent, ' - level ', hybas_levels[lev], '/10'))
    
    # read catchment level cell IDs
    lev.file <- read.csv(paste0(inputDirCatchments, continent, '/lev', hybas_levels[lev], '.csv'))
    
    #join with plants and volumes table
    lev.plants <- inner_join(wwt.continent.plants, lev.file, by = 'cell_ID') %>% 
      #rename, may remove later when merging shp attribute tables
      rename(PFAF_ID=catchment)
    lev.volumes <- inner_join(wwt.continent.volumes, lev.file, by = 'cell_ID') %>% 
      #rename, may remove later when merging shp attribute tables
      rename(PFAF_ID=catchment)
    
    write.csv(lev.plants, paste0(outputDir, 'lev', 
                                 hybas_levels[lev], '_plants.csv'))
    
    write.csv(lev.volumes, paste0(outputDir, 'lev', 
                                  hybas_levels[lev], '_volumes.csv'))
    
  }
  
  
  #get countries in continent
  countries.id.plants <- unique(wwt.continent.plants$country_ID)
  countries.id.volumes <- unique(wwt.continent.volumes$country_ID)
  
  #country names
  countries.continent.plants <- country.names$Country[country.names$ID %in% countries.id.plants]
  countries.continent.volumes <- country.names$Country[country.names$ID %in% countries.id.volumes]

  #### get countries with plants in Hydrowaste but no wastewater production data ####
  #skip arctic -> no non-matching countries
  if(continent == 'ar' ){next}
  
  diff.countries.plants <- as.data.frame(setdiff(countries.continent.plants,
                                                 countries.continent.volumes))
  
  diff.countries.plants$plants <- 1
  diff.countries.plants$production <- 0
  
  colnames(diff.countries.plants)[1] <- 'Country'
  # diff.countries.plants

  #and viceversa
  diff.countries.volumes <- as.data.frame(setdiff(countries.continent.volumes,
                                                  countries.continent.plants))
  
  if(continent == 'si'){
    
    df.diff.countries <- diff.countries.plants
    
  }
  
  else{
    
    diff.countries.volumes$plants <- 0
    diff.countries.volumes$production <- 1
  
    colnames(diff.countries.volumes)[1] <- 'Country'
    # diff.countries.volumes
    
    df.diff.countries <- rbind(diff.countries.plants, diff.countries.volumes)
    df.diff.countries
  
  }
  
  #save file of non-matching countries for later reference
  
  write.csv(df.diff.countries, paste0(outputDirNonMatching, continent, '.csv'), row.names = F)

  nonmatching.global.list[[hybas_continent]] <- df.diff.countries %>%
    mutate(hybas_continent = continent)
  
}

nonmatching.global <- do.call(rbind, nonmatching.global.list) %>% 
  inner_join(country.names)
write.csv(nonmatching.global, paste0(outputDirNonMatching, 'global.csv'), row.names = F)
