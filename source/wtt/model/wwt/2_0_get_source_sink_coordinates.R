rm(list=ls())

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#### paths ####
inputDir <- '../../../../output/wtt/model/wwt/0_volumes/'

inputDirData <- paste0(inputDir, '0_raw/0_hybas_lev01/')

#flows assigned to plants in the same catchment
inputDirSameCatchment <- paste0(inputDir, '2_wastewaters_to_plants/0_0_flows_same_catchment/')

#flows assigned to a downstream catchment
inputDirDownstream <- paste0(inputDir, '2_wastewaters_to_plants/1_0_flows_downstream/')

#set output path
outputDir <- paste0(inputDir, '2_wastewaters_to_plants/2_0_source_sink_coordinates/')
dir.create(outputDir, showWarnings = F, recursive = T)

#set continent
hybas_continents <- c('af','ar','as','au','eu','na', 'sa','si')

#hybas level
hybas_levels <- c('03','04','05','06','07','08','09','10')

latlon.flows.global.list <- list()

for(hybas_continent in seq(1, length(hybas_continents))){
  
  continent <- hybas_continents[hybas_continent]
  
  outputDirContinent <- paste0(outputDir, continent, '/')
  dir.create(outputDirContinent, showWarnings = F, recursive = T)
  
  #continent lat lon
  table.continent <- vroom(paste0(inputDirData, 'wwt_', continent, '.csv'),
                           col_names = TRUE, show_col_types = FALSE) 
  latlon.continent <- table.continent %>% select(cell_ID, lon, lat)
  
  for(level in seq(1, length(hybas_levels))){
  
    replaceMessage(
      paste0('continent ', hybas_continent, '/', length(hybas_continents),
             ': ', continent, ' - level ', hybas_levels[level]))
    
    
    lev <- hybas_levels[level]
    
    #get source to sink wastewater flows table per continent
    flows.same.catchment <- read.csv(paste0(inputDirSameCatchment, continent, '/lev', lev, '.csv'))
    flows.same.catchment$downstream <- 0
    
    
    flows.downstream <- read.csv(paste0(inputDirDownstream, continent, '/lev', lev, '.csv'))
    flows.downstream$downstream <- 1
      
    #### processing ####
    
    #get assigned wastewaters
    flows.df <- rbind(flows.same.catchment, flows.downstream) %>% 
      filter(cell_ID_sink != 0)
    
    # #get wastewater pixels that were not assigned to a plant
    # not.assigned.wastewater.pixels <- rbind(flows.same.catchment, flows.downstream) %>% 
    #   filter(cell_ID_sink == 0) %>% 
    #   select(cell_ID_source) %>% 
    #   rename(cell_ID = cell_ID_source)
    
    #source lat, lon
    cell.id.source <- as.data.frame(flows.df$cell_ID_source)
    colnames(cell.id.source) <- 'cell_ID'
    latlon.source <- inner_join(cell.id.source, latlon.continent, by='cell_ID')
    colnames(latlon.source) <- c('cell_ID_source', 'lon_source', 'lat_source')
    
    #sink lat, lon
    cell.id.sink <- as.data.frame(flows.df$cell_ID_sink)
    colnames(cell.id.sink) <- 'cell_ID'
    latlon.sink <- inner_join(cell.id.sink, latlon.continent, by = 'cell_ID')
    colnames(latlon.sink) <- c('cell_ID_sink', 'lon_sink', 'lat_sink')
    
  
    #make final dataframe
    latlon.source.sink <- as.data.frame(
      cbind(latlon.source, latlon.sink))
    
    #column names
    colnames(latlon.source.sink) <- c('cell_ID_source', 'lon_source', 'lat_source',
                                      'cell_ID_sink', 'lon_sink', 'lat_sink')
    
    #add column to know if plant is in same catchment (0) or downstream (1)
    latlon.source.sink.info <- inner_join(latlon.source.sink, 
                                                flows.df %>% 
                                                  select(-cell_ID_sink,
                                                         -country_ID),
                                          by = 'cell_ID_source')
    write.csv(latlon.source.sink.info, 
              paste0(outputDirContinent, 'lev', lev, '.csv'), row.names = F)
   
  }
  
}


#### get global flows per level #### 
print('collecting global flows per hybas level...')
outputDirFlowsGlobal <- paste0(inputDir, '2_wastewaters_to_plants/2_0_source_sink_coordinates/global/')
dir.create(outputDirFlowsGlobal, showWarnings = F, recursive = T)

global.flows.level.list <- list()

for(level in seq(1, length(hybas_levels))){
  
  lev <- hybas_levels[level]
  
  for(hybas_continent in seq(1, length(hybas_continents))){
    
    continent <- hybas_continents[hybas_continent]
  
    inputDirContinent <- paste0(
      inputDir, '2_wastewaters_to_plants/2_0_source_sink_coordinates/', continent, '/')
    
    continent.level.flows <- read.csv(paste0(inputDirContinent, 'lev', lev, '.csv')) %>% 
      mutate(continent = continent)
    
    global.flows.level.list[[hybas_continent]] <- continent.level.flows
  
  }
  
  global.flows.level <- do.call(rbind, global.flows.level.list)
  vroom_write(global.flows.level, paste0(outputDirFlowsGlobal, 'lev', lev, '.csv'), ',')
  
}
  