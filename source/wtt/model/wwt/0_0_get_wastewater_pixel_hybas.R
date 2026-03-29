rm(list=ls())

#### extract continent-level pixels for wastewater production and 
#### treatment plants locations

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDirWorld <- '../../../../input/global_data/'

inputDirWaste <- '../../../../input/wtt/wwt/'

inputDirCatchments <- paste0(inputDirWorld, 'general/HydroBASINS_5arcmin/0_hybas_lev01/')

outputDir <- 
  '../../../../output/wtt/model/wwt/0_volumes/0_raw/0_hybas_lev01/'

dir.create(outputDir, showWarnings = F, recursive = T)

#### load files ####
countries.global <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'),
                          col_names = TRUE, show_col_types = FALSE)

population.pixel <- vroom(paste0(inputDirWorld, 'general/population_5arcmin_2015.csv'),
                          col_names = TRUE, show_col_types = FALSE)

ww.plants.global <- vroom(paste0(inputDirWaste, 'HydroWASTE_updated_China_5arcmin.csv'),
                          col_names = TRUE, show_col_types = FALSE)

ww.volumes.global <- vroom(paste0(inputDirWaste, 'WWT_Jones2021_5arcmin.csv'),
                          col_names = TRUE, show_col_types = FALSE)

#### load hybas continent locations and filter ####
continents <- c('af','ar','as','au','eu','na', 'sa','si')

wwt.global.list <- list()

#loop over continents
for(continent in seq(1:length(continents))){

  replaceMessage(paste0('continent ', continent, '/', length(continents),
                        ': ', continents[continent]))
  
  #continent cell IDs
  continent.cells <- 
    read.csv(paste0(inputDirCatchments, 'cellid_', continents[continent], '.csv'))
  
  #get lat lon
  continent.lon <- ww.plants.global$lon[continent.cells[,1]]
  continent.lat <- ww.plants.global$lat[continent.cells[,1]]
  
  #get country ID, wastewater plants, volumes and population
  countries.continent <- countries.global$country_ID[continent.cells[,1]]
  
  ww.plants.continent <- ww.plants.global$wwtp[continent.cells[,1]]
  ww.volumes.continent <- ww.volumes.global$wwtv[continent.cells[,1]]
  
  pop.continent <- population.pixel$pop_2015[continent.cells[,1]]
  
  #cbind
  continent.ww.df <- as.data.frame(
    cbind(continent.cells$cell_ID,
          continent.lon,
          continent.lat,
          countries.continent,
          pop.continent,
          ww.volumes.continent,
          ww.plants.continent
    ))
  
  
  colnames(continent.ww.df) <- c('cell_ID','lon', 'lat', 'country_ID', 'population',
                                 'wwtv', 'wwtp')
    
  #save
  write.csv(continent.ww.df, paste0(outputDir, 'wwt_', continents[continent], '.csv'), row.names = F)

  wwt.global.list[[continent]] <- continent.ww.df
  
}

wwt.global <- do.call(rbind, wwt.global.list)
vroom_write(wwt.global, paste0(outputDir, 'wwt_global.csv'), ",")
