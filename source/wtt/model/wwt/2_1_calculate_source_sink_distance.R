rm(list=ls())

library(dplyr)
library(vroom)
library(geosphere)


replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}


#### paths and raw files ####
inputDirRaw <- '../../../../input/wtt/wwt/'

inputDirWorld <- '../../../../input/global_data/'

inputDirVolumes <- '../../../../output/wtt/model/wwt/0_volumes/'

inputDirFlows <- paste0(
  inputDirVolumes, '2_wastewaters_to_plants/2_0_source_sink_coordinates/global/')

outputDir <- paste0(inputDirVolumes, '2_wastewaters_to_plants/2_1_source_sink_distances/')
dir.create(outputDir, showWarnings = F, recursive = T)

#file with ids and country names
country.id.names <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv'))

#read original hydrowaste
hydrowaste <- read.csv(paste0(inputDirRaw, 'HydroWASTE_updated_China.csv')) %>% 
  mutate(WASTE_DIS_KM3_Y = WASTE_DIS * 365 / 10^9)

#read file with pre-assignment volumes and plants
wwt.global.raw <- vroom(
  paste0(inputDirVolumes, '0_raw/0_hybas_lev01/wwt_global.csv'),
  col_names = TRUE, show_col_types = FALSE)

#hybas level
hybas_levels <- c('03','04','05','06','07','08','09','10')

#### processing ####

global.flows.list <- list()

bugged.flows.list <- list()

#joining with pixels' info and calculating source-sink distance
for(level in seq(1, length(hybas_levels))){

  lev <- hybas_levels[level]
  print(paste0('level: ', lev))

  #read file with global assigned flows to plants
  global.flows <- read.csv(
    paste0(inputDirFlows, '/lev', lev, '.csv'))

  #inner join flows with pixel info
  global.flows.info.df <- inner_join(global.flows, wwt.global.raw,
                                      by = c('cell_ID_source' = 'cell_ID')) %>%
    select(-lon, -lat) %>%
    inner_join(country.id.names, by = c('country_ID' = 'ID'))
  
  # calculate distance between sources and sinks
  # package geosphere outputs distance in meters
  distance.list <- list()

  for(coord in seq(1, nrow(global.flows.info.df))){

    replaceMessage(paste0(coord, ' / ', nrow(global.flows.info.df)))

    distance <- as.data.frame(
      distm(global.flows.info.df[coord ,c('lon_source','lat_source')],
            global.flows.info.df[coord ,c('lon_sink','lat_sink')],
            fun = distHaversine))

    distance.list[[coord]] <- distance

  }

  distance.m <- do.call(rbind, distance.list)
  distance.km <- distance.m / 1000
  colnames(distance.km) <- 'ss_distance'
  

  #attach distance to dataframe
  global.flows.with.distance <- cbind(global.flows.info.df, distance.km) 
  
  global.flows.add.info <- global.flows.with.distance %>% 
    mutate(level = as.numeric(lev)) 
  
  #give info on pixel (source,sink or both or nothing)
  global.flows.add.info$ss_info <- NA
  
  #pixel status
  global.flows.add.info$ss_info[global.flows.add.info$wwtv != 0 & global.flows.add.info$wwtp == 0] <- 'source'
  global.flows.add.info$ss_info[global.flows.add.info$wwtv == 0 & global.flows.add.info$wwtp != 0] <- 'sink'
  global.flows.add.info$ss_info[global.flows.add.info$wwtv != 0 & global.flows.add.info$wwtp != 0] <- 'source_and_sink'
  global.flows.add.info$ss_info[global.flows.add.info$wwtv == 0 & global.flows.add.info$wwtp == 0] <- 'empty'
  
  #reposition columns
  relocated.flows.table <- global.flows.add.info %>% 
    relocate(country_ID, .before = Country) %>% 
    relocate(ss_distance, .before = downstream) %>% 
    relocate(ss_info, .before = downstream) %>% 
    relocate(continent, .before = level) %>% 
    arrange(cell_ID_source)
  
  #round floats to 3 numbers after comma
  relocated.flows.table$ss_distance <- round2(relocated.flows.table$ss_distance, 3)
  relocated.flows.table$lon_source <- round2(relocated.flows.table$lon_source, 3)
  relocated.flows.table$lat_source <- round2(relocated.flows.table$lat_source, 3)
  relocated.flows.table$lon_sink <- round2(relocated.flows.table$lon_sink, 3)
  relocated.flows.table$lat_sink <- round2(relocated.flows.table$lat_sink, 3)

  ### save things ####
  vroom_write(relocated.flows.table, 
              paste0(outputDir, 'lev', lev, '.csv'), ',')
  
  global.flows.list[[level]] <- relocated.flows.table
  

}

global.flows.all.levels <- do.call(rbind, global.flows.list) %>% 
  arrange(cell_ID_source)

vroom_write(global.flows.all.levels, 
          paste0(outputDir, 'global_flows.csv'), ',')