rm(list=ls())

# script to direct wastewater volumes to plants in the same catchment #
# at all hydroBASINS levels (catchments get smaller for high levels 1-10) #

library(dplyr)
library(FNN)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

#### paths and files ####
#input and output paths for wastewater volumes
inputDir <- '../../../../output/wtt/model/wwt/0_volumes/1_flows_model_input/model_input/'
outputDir <- '../../../../output/wtt/model/wwt/0_volumes/2_wastewaters_to_plants/'
dir.create(outputDir, showWarnings = F, recursive = T)

#continents and upper levels of hydroBASINS
hybas_continents <- c('af','ar', 'as','au','eu', 'na','sa','si')
hybas_levels <- c('02','03','04','05','06','07','08','09','10') 

#### processing : assign volumes cells to plants cells ####
continent.assigned.stats.list <- list()
#- loop by continent -#
for(hybas_continent in 3){
# for(hybas_continent in seq(length(hybas_continents))){
  
  #select continent
  # hybas_continent = 1
  continent <- hybas_continents[hybas_continent]
  
  flows.same.catchment.level.list <- list()
  volumes.non.shared.continent.list <- list()
  level.assigned.stats.list <- list()

    #- loop by level -#
    for(level in seq(1, length(hybas_levels))){
      
    #select level
    # level = 9
    lev <- hybas_levels[level]
    
    #get plants and volume for level 
    continentLevelPlants <- read.csv(
      paste0(inputDir, continent, '/lev', lev, '_input_plants.csv'))
    continentLevelVolumes <- read.csv(
      paste0(inputDir, continent, '/lev', lev, '_input_volumes.csv'))
    
    #get countries of plants and volumes -> these must match
    countries.plants <- sort(unique(continentLevelPlants$country_ID))
    countries.volumes <- sort(unique(continentLevelVolumes$country_ID))

    #- loop by country -#
    countries.level <- countries.plants
    
    for(country.idx in seq(1, length(countries.level))){
      
      #select country
      # country.idx = 1
      country.id <- countries.level[country.idx]
      
      #get plants and volumes for selected country
      country.plants <- continentLevelPlants[
        continentLevelPlants$country_ID == country.id,]
      country.volumes <- continentLevelVolumes[
        continentLevelVolumes$country_ID == country.id,]
      
      #get plants' and volumes' catchments per country 
      # if these do not match, the non-matching volumes will go towards downstream assignment (next script)
      country.plants.catchments <- unique(country.plants$PFAF_ID)
      country.volumes.catchments <- unique(country.volumes$PFAF_ID)
      
      #get shared catchments
      shared.catchments <- intersect(
        country.plants.catchments, country.volumes.catchments)
      
      #get plants and volumes in shared catchments -> assign in this script
      shared.plants <- country.plants[
        country.plants$PFAF_ID %in% shared.catchments,]
      shared.volumes <- country.volumes[
        country.volumes$PFAF_ID %in% shared.catchments,]
      
      #store the rest of the volumes for downstream script
      volumes.non.shared.country <- setdiff(
        country.volumes, shared.volumes)
      
      volumes.non.shared.continent.list[[country.idx]] <-
        volumes.non.shared.country
      
      # if there are no shared catchments -> go to next country
      if(length(shared.catchments) == 0){
        
        next
        
      }
      
      flows.same.catchment.country.list <- list()
      
      #- loop by shared catchments in each country -#
      for(cc in seq(1, length(shared.catchments))){
      
        replaceMessage(
          paste0(continent, ' - level ', level, '/', length(hybas_levels),
                 ' - country ', country.idx, '/', length(countries.level),
                 ' - catchment ', cc, '/', length(shared.catchments)
                 ))
        
        #select catchment
        # cc = 1
        catchment <- shared.catchments[cc]
  
        #get cell id of plants and volumes in selected shared catchment
        cell.id.sink <- shared.plants$cell_ID[shared.plants$PFAF_ID == catchment]
  
        cell.id.source <- shared.volumes$cell_ID[shared.volumes$PFAF_ID == catchment]
        
        # case 1: 1 plant per catchment: 
        # assign all ww volumes in that catchment to that plant
        if(length(cell.id.sink) == 1){
          
          #bind cell ids of sources and sinks
          cell.id.df <- as.data.frame(
            cbind(cell.id.source, cell.id.sink))
          
        } else{
          
        # case 2: > 1 plant per catchment: assign volumes to nearest plant (FNN)
          
          # cell ID of > 1 plants in a catchment for later binding
          cor_sink_id <- as.data.frame(cbind(
            shared.plants$cell_ID[shared.plants$PFAF_ID == catchment]))

          #coordinates of > 1 plants in a catchment (without ID for FNN computation)
          cor_sink <- as.data.frame(cbind(
            shared.plants$lon[shared.plants$PFAF_ID == catchment],
            shared.plants$lat[shared.plants$PFAF_ID == catchment]))
          
          #coordinates of wastewater sources (without ID for FNN computation)
          cor_source <- as.data.frame(cbind(
            shared.volumes$lon[shared.volumes$PFAF_ID == catchment],
            shared.volumes$lat[shared.volumes$PFAF_ID == catchment]))

          #find nearest neighbour (plant)
          matches <- knnx.index(cor_sink, cor_source, k = 1)
          
          #find neareast neighbour cell ID
          # HERE WAS THE BUG !!! 
          # Was getting cell ID from all country rows, not in the single catchment
          cell.id.sink.multiple.plants <- as.data.frame(
            cor_sink_id$V1[matches[,1]])
          
          #make dataframe of sources to sink cell ID
          cell.id.df <- as.data.frame(
            cbind(cell.id.source, cell.id.sink.multiple.plants))

        }

        colnames(cell.id.df) <- c('cell_ID_source', 'cell_ID_sink')

        #add country ID
        cell.id.df$country_ID <- country.id
        
        #store flows per catchment
        flows.same.catchment.country.list[[cc]] <- cell.id.df

      }
      
      #explode country list
      flows.same.catchment.country <- do.call(rbind, flows.same.catchment.country.list)
      
      #store flows per country
      flows.same.catchment.level.list[[country.idx]] <- flows.same.catchment.country
    
    }
      
    #explode continent list 
    flows.same.catchment.level <- do.call(rbind, flows.same.catchment.level.list)
    
    outputDirSameCatchmentLevel <- paste0(
      outputDir, '0_0_flows_same_catchment/', continent, '/')
    dir.create(outputDirSameCatchmentLevel, showWarnings = F, recursive = T)
    
    write.csv(flows.same.catchment.level, 
              paste0(outputDirSameCatchmentLevel, 
                     'lev', lev, '.csv'), 
              row.names = F)
  
    # save non-shared volumes per continent 
    outputDirNonSharedVolumes <- paste0(
      outputDir, '0_1_volumes_for_downstream/', continent, '/')
    dir.create(outputDirNonSharedVolumes, showWarnings = F, recursive = T)
    
    volumes.non.shared.continent <- do.call(rbind, 
                                            volumes.non.shared.continent.list)
    write.csv(volumes.non.shared.continent, 
              paste0(outputDirNonSharedVolumes, 
                     'lev', lev, '.csv'), 
              row.names = F)
    
    #get how many rows were assigned at each level and not
    level.stats <- as.data.frame(cbind(
      continent,
      nrow(continentLevelVolumes),
      lev,
      nrow(flows.same.catchment.level),
      nrow(volumes.non.shared.continent)
    ))

    colnames(level.stats) <- c('continent','tot.pixels','hb.lvl',
                                   'assigned','not.assigned')
    
    #store assigned pixels per level
    level.assigned.stats.list[[level]] <- level.stats
    
    }
  
  #explode level statistics
  level.assigned.stats <- do.call(rbind, level.assigned.stats.list)
  
  #store continent statistics
  continent.assigned.stats.list[[hybas_continent]] <- level.assigned.stats
  
}

flows.stats <- do.call(rbind, continent.assigned.stats.list)

write.csv(flows.stats, paste0(outputDir, '0_0_flows_same_catchment/assigned_stats.csv'),
          row.names = F)