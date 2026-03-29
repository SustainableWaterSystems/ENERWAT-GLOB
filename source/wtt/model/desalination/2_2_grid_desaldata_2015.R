rm(list=ls())

library(dplyr)
library(vroom)
library(FNN)
library(terra)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../output/wtt/model/desalination/2_energy/'
inputDirWorld <- '../../../../input/global_data/'
outputDir <- '../../../../output/wtt/model/desalination/2_energy/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### part 1 :::: attributes each plant to the nearest gridcell (centroid) ####
#load point data
data <- read.csv(paste0(inputDir, 'energy_plants_2015_desal.csv')) %>% 
  filter(!is.na(Latitude)) 

#load text file of gridcells with cell ID and coordinates
gridcells <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'),
                   show_col_types = F)

#get non-NA cell IDs
not.na.gridcells <- gridcells[!is.na(gridcells$Country),]

#### assign cell IDs to plants ####
#loop over countries to constrain assignment to same country
# data.countries <- unique(data$country.renamed)
data.countries <- intersect(unique(gridcells$Country), unique(data$Country))


#empty list for appending
data.with.cellids.list <- list()

for(country in 1:length(data.countries)){
  
  replaceMessage(paste0('Country ', country, '/', length(data.countries)))
  
  #get country plants
  country.plants <- data %>% 
    # filter(country.renamed==data.countries[country])
    filter(Country==data.countries[country]) 
  
  #get country pixels
  gridcells.country <- not.na.gridcells %>% 
    # filter(Country == data.countries[country])
    filter(Country == data.countries[country])
  
  #make dataframe of plant coordinates
  coord.plants.country <- as.data.frame(cbind(
    country.plants$Longitude, country.plants$Latitude))
  
  #make dataframe of gridcells coordinates
  coord.cells.country <- as.data.frame(cbind(
    gridcells.country$cell_lon, gridcells.country$cell_lat))
  
  #find nearest cell to plants
  matches <- knnx.index(coord.cells.country, coord.plants.country, k = 1)
  
  #find nearest neighbour cell ID
  nearest.cell.id <- as.data.frame(
    gridcells.country$cell_ID[matches[,1]])
  colnames(nearest.cell.id) <- 'pcrglobwb_cellID'
  
  #bind to country plants
  country.plants.cellid <- cbind(country.plants, nearest.cell.id)
  
  #append to global list 
  data.with.cellids.list[[country]] <- country.plants.cellid
  
}

#explode list
data.with.cellids <- do.call(rbind, data.with.cellids.list)

#save intermediate output
write.csv(data.with.cellids,
          paste0(outputDir, 'energy_plants_2015_desal.csv'), row.names = F)
