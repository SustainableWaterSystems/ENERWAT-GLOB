rm(list=ls())

library(dplyr)
library(FNN)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#### paths and files ####
inputDirWorld <- '../../../../input/global_data/'

inputDirShp <- paste0(
  inputDirWorld, 'general/HydroBASINS_tables/')

inputDirModel <- 
  '../../../../output/wtt/model/wwt/0_volumes/'

inputDirPlants <- paste0(inputDirModel, '1_flows_model_input/model_input/')

inputDirVolumes <- paste0(inputDirModel, '2_wastewaters_to_plants/0_1_volumes_for_downstream/')

outputDirDownstream <- paste0(inputDirModel, '2_wastewaters_to_plants/1_0_flows_downstream/')
dir.create(outputDirDownstream, showWarnings = F, recursive = T)

#country_names
country.names <- read.csv(
  paste0(inputDirWorld, 'general/countries_id_regions.csv')) 

#continents
hybas_continents <- c('af','ar', 'as','au','eu', 'na','sa','si')

#hybas level
hybas_levels <- c('03','04','05','06','07','08','09','10')

### processing ####
for(hybas_continent in seq(1, length(hybas_continents))){
  
  # #select continent
  continent <- hybas_continents[hybas_continent]
  
  for(level in seq(1, length(hybas_levels))){
    
    #select level
    lev <- hybas_levels[level]
    
    #empty list for continent
    downstream.flows.continent.list <- list()
    

    #read catchment shapefile table
    hybas.level.shp <-  read.csv(paste0(inputDirShp, continent, '/', 
                                        'lev', lev, '.csv'))
    
    #open plants table
    continent.plants <- read.csv(paste0(inputDirPlants,  continent, '/lev', lev, 
                                        '_input_plants.csv'))
    
    #open table of volumes not assigned in same catchment
    continent.volumes.for.downstream <- read.csv(
      paste0(inputDirVolumes, continent,'/lev', lev, '.csv'))
    
    #### processing: search downstream catchments ####
    #get unique catchments -> these may not match 
    catchments.plants <- sort(unique(continent.plants$PFAF_ID))
    catchments.volumes <- sort(unique(continent.volumes.for.downstream$PFAF_ID))
    
    #get unique countries -> if these do not match, all volumes should have been assigned at same catchment??
    continent.countries.plants <- sort(unique(continent.plants$country_ID))
    continent.countries.volumes <- sort(unique(continent.volumes.for.downstream$country_ID))
    
    # #check complete volume assignment of mismatching countries
    # countries.mismatch <- as.data.frame(
    #   setdiff(continent.countries.plants, continent.countries.volumes)) 
    # colnames(countries.mismatch) <- 'country_ID'
    # countries.mismatch <- inner_join(countries.mismatch, country.names)
    # 
    # all.continent.volumes <- read.csv(
    #   paste0('../../../input/wastewater_treatment/1_wastewater_volumes/2_ww_hybas_higher_levels/',
    #     continent, '/lev10_volumes.csv')) %>% 
    #   filter(country_ID %in% countries.mismatch)
    # same.catchment.ids <- read.csv(paste0(inputPathWW, '1_flows_same_catchment/',
    #                                       continent, '.csv')) %>% 
    #   filter(country_ID %in% countries.mismatch)
    # 
    # if(nrow(all.continent.volumes == nrow(same.catchment.ids))){
    #   print(paste0('all wastewaters were allocated in the same catchment in the following countries: ',
    #               countries.mismatch$Country))}
    # # else if(nrow(all.continent.volumes != nrow(same.catchment.ids){print('bug in same catchment allocation')}
    
    
    #--------- loop over volumes' countries (avoid extra processing) ---------#
    
    for(country in seq(1, length(continent.countries.volumes))){
  
      #empty list for country
      downstream.flows.country.list <- list()
      
      #get plants pixels for a country
      country.plants <- continent.plants[
        continent.plants$country_ID == continent.countries.volumes[country],]
      
      #get wastewater volumes for a country
      country.volumes <- continent.volumes.for.downstream[
        continent.volumes.for.downstream$country_ID == continent.countries.volumes[country],]
      
      #get unique catchments where plants or production are
      catchments.plants.country <- unique(country.plants$PFAF_ID)
      catchments.volumes.country <- unique(country.volumes$PFAF_ID)
      
      # #check catchments in common -> should be empty
      # common.catchments <- intersect(
      #   catchments.plants.country, catchments.volumes.country)
      # common.catchments
      
      #----- loop over volumes' catchments -----#
      
      for(cc in seq(1, length(catchments.volumes.country))){
        
        replaceMessage(
          paste0(continent, ' - level ', level, '/', length(hybas_levels),
                 ' - country ', country, '/', length(continent.countries.volumes), ' : ', 
                 country.names$Country[country.names$country_ID == 
                                         continent.countries.volumes[country]],
                 ' - Catchment ', cc, '/', length(catchments.volumes.country)
          ))
        
        catchment <- catchments.volumes.country[cc]
        
        # #this should always be empty because there are no plants in these volumes catchments
        # cell.id.sink <- plants.country$cell_ID[plants.country$PFAF_ID == catchment]
        
        #----- search downstream catchment for the selected catchment from shapefile table -----#
        
        #get cell id and pfaf id of volumes pixel per catchment
        cell.id.source <- as.data.frame(
          country.volumes[country.volumes$PFAF_ID == catchment,]) %>% 
          select(cell_ID, PFAF_ID)
    
        
        #repeat until a catchment is found or until last subbasin 
        # -> then break repeat and assign cell ID of volume either 
        # to a plant or to removed pixels list
        # HYBAS_DOWN == 0 OR HYBAS_ID == MAIN_BAS
        repeat{
        
          source.catchment <- hybas.level.shp[
            hybas.level.shp$PFAF_ID==unique(cell.id.source$PFAF_ID),]
          
          #get first downstream catchment HYBAS_ID (NEXT_DOWN)
          downstream.catchment.hybas.id <- hybas.level.shp$NEXT_DOWN[
            hybas.level.shp$PFAF_ID == unique(cell.id.source$PFAF_ID)]
        
          #get first downstream catchment dataframe
          downstream.catchment <- hybas.level.shp[
            hybas.level.shp$HYBAS_ID == downstream.catchment.hybas.id,]
          
          #------------- cases -------------#
          
      
          # if the first downstream catchment has at least one treatment plant
          if(any(country.plants$PFAF_ID %in% downstream.catchment) == TRUE |
             
             # OR IF next_down = 0 (no more downstream catchments)
             (source.catchment$NEXT_DOWN == 0 &
             # AND there are no plants in this final catchment
             sum(country.plants$PFAF_ID %in% source.catchment) == 0)){
        
            # break repeat loop to try and find a suitable plant
            break
            
          }
      
          #otherwise assign new pfaf_id to cell_ID to repeat loop 
          #search one more downstream
          else{
        
            cell.id.source$PFAF_ID <- downstream.catchment$PFAF_ID
        
          }
        
        }
        
        
        #if no plants are found -> add cell.id.source to non.assignable.pixels list
        if(nrow(downstream.catchment) == 0){
      
          cell.id.sink <- 0
          
          cell.id.df <- as.data.frame(
            cbind(cell.id.source$cell_ID, cell.id.sink))
    
    
        }
    
        # if 1 plant -> assign volume to that plant
        else if(sum(country.plants$PFAF_ID %in% downstream.catchment) == 1){
    
          cell.id.sink <- country.plants$cell_ID[
            country.plants$PFAF_ID == downstream.catchment$PFAF_ID]
    
          #bind cell ids of sources and sinks
          cell.id.df <- as.data.frame(
            cbind(cell.id.source$cell_ID, cell.id.sink))
    
          
          }
    
    
        # if more than 1 plant, assign to closest plant
        else{
    
          #coordinates of > 1 plants in a catchments 
          cor_sink <- as.data.frame(cbind(
            country.plants$lon[country.plants$PFAF_ID == downstream.catchment$PFAF_ID],
            country.plants$lat[country.plants$PFAF_ID == downstream.catchment$PFAF_ID]))
          
          #coordinates of wastewater sources
          cor_source <- as.data.frame(cbind(
            country.volumes$lon[country.volumes$cell_ID %in% cell.id.source$cell_ID],
            country.volumes$lat[country.volumes$cell_ID %in% cell.id.source$cell_ID]))
          
          #find nearest neighbour (plant)
          matches <- knnx.index(cor_sink, cor_source, k = 1)
          
          #find neareast neighbour cell ID
          cell.id.sink.multiple.plants <- as.data.frame(
            country.plants$cell_ID[matches[,1]])
          
          #make dataframe of sources to sink cell ID
          cell.id.df <- cbind(cell.id.source$cell_ID, cell.id.sink.multiple.plants)
    
    
        }
        
        colnames(cell.id.df) <- c('cell_ID_source', 'cell_ID_sink')
        
        #add country ID
        cell.id.df$country_ID <- country.names$ID[
          country.names$ID == continent.countries.plants[country]] 
        
        downstream.flows.country.list[[cc]] <- cell.id.df
        
      }
        
      #explode country list
      downstream.flows.country <- do.call(rbind, downstream.flows.country.list)
      
      #attach country ids to continent list for bigger table
      downstream.flows.continent.list[[country]] <- downstream.flows.country
  
    
    
    }
    
    #explode continent list
    downstream.flows.continent <- do.call(rbind, downstream.flows.continent.list)
    
    outputDirLevel <- paste0(outputDirDownstream, continent, '/')
    dir.create(outputDirLevel, showWarnings = F, recursive = T)
    
    write.csv(downstream.flows.continent, paste0(outputDirLevel,
                                           'lev', lev, '.csv'), row.names = F)
    
  }
  
}

