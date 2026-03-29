rm(list=ls())

#### retain volumes assigned to closest plant 
#### 3 cases for plot: 1) only hybas level 10, 
#### 2) all hybas levels, in same catchment,
#### 3) all hybas levels, also with downstream assignment

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#### paths and files ####
directory <- '../../../../output/wtt/model/wwt/0_volumes/'
inputDir <- paste0(directory, '2_wastewaters_to_plants/2_1_source_sink_distances/')
outputDir <- paste0(directory, '3_flows_model_output/')
dir.create(outputDir, showWarnings = F, recursive = T)

#hybas level
hybas_levels <- c('03','04','05','06','07','08','09','10')

global.flows.all.levels <- vroom(
  paste0(inputDir, '/global_flows.csv'),
  col_names = TRUE, show_col_types = FALSE)

#### loop over source pixels and retain the one with the closest distance to plant ####
#### best-case scenario: all hybas levels and also downstream
selected.source.sink.list <- list()
source.pixels <- unique(global.flows.all.levels$cell_ID_source)

for(idx in seq(1, length(source.pixels))){
  
  replaceMessage(paste0('calculating nearest plant: cellID ', idx, '/' , length(source.pixels)))
  
  source <- source.pixels[idx]
  
  source.rows <- global.flows.all.levels %>% 
    filter(cell_ID_source == source)
  
  # the volume was assigned only at one level? 
  if(nrow(source.rows) == 1){
    
    source.min.distance <- source.rows
    
  }
  
  # the volume was assigned at multiple levels
  else{
    
    distance.data <- unique(source.rows$ss_distance)
    
    # the distance is always the same for all levels
    if(length(distance.data) == 1){
      
      # so just keep the one from the highest level (smallest basin size)
      source.min.distance <- source.rows[which.max(source.rows$level), ]
      
    }
    
    # the plant is not always the same at all levels where it was assigned
    else{
      
      min.distance <- min(distance.data) 
      
      pixels.min.distance <- source.rows[source.rows$ss_distance == min.distance, ]
      
      if(nrow(pixels.min.distance) == 1){
        
        source.min.distance <- pixels.min.distance
        
      }
      
      else{
        
        source.min.distance <- pixels.min.distance[which.max(pixels.min.distance$level), ]
        
      }
    }
    
    
  }
  
  selected.source.sink.list[[idx]] <- source.min.distance
  
}


selected.source.sink <- do.call(rbind, selected.source.sink.list)

write.csv(selected.source.sink, paste0(
  outputDir, 'nearest_ss_flows.csv'), 
          row.names = F)