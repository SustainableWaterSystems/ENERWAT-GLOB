rm(list=ls())

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../../output/wtt/model/desalination/2_energy/'
inputDirWorld <- '../../../../../input/global_data/'
outputDir <- '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### part 1 :::: filter drinking desalination plants from desal outputs ####
#load point data
data <- read.csv(paste0(inputDir, 'energy_plants_2015_desal.csv')) 

data.filtered.drinking <- data %>% 
  filter(!is.na(Latitude)) %>% 
  filter(Customer.type == "Municipalities as drinking water (TDS 10ppm - <1000ppm)"  
         | Customer.type == "Tourist facilities as drinking water (TDS 10ppm - <1000ppm)")

#save intermediate output
write.csv(data.filtered.drinking, 
          paste0(outputDir, 'desaldata_drinking_2015_gridcells.csv'), row.names = F)

#### add desalination capacity to 5 arcmin country table ####

#load text file of gridcells with cell ID and coordinates
gridcells <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'),
                   show_col_types = F)

desal.drinking.5arc <- data.filtered.drinking %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(drinking.desalination.m3.y = sum(Capacity..m3.d.) * 365) %>% 
  rename(cell_ID = pcrglobwb_cellID)

desal.drinking.5arc.all <- merge(gridcells, desal.drinking.5arc, all = T)

desal.drinking.5arc.clean <- desal.drinking.5arc.all %>% 
  select(cell_ID, cell_lon, cell_lat, drinking.desalination.m3.y) %>% 
  rename(lon = cell_lon,
         lat = cell_lat) %>% 
  replace(is.na(.), 0)

vroom_write(desal.drinking.5arc.clean, 
            paste0(outputDir, 'desaldata_drinking_5arcmin_2015.csv'), ',')
