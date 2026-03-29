rm(list=ls())

library(vroom)
library(dplyr)
library(stringr)

inputDir <- '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/1_desalination_allocation/'
inputDirDWT <- '../../../../../input/wtt/dwt/'
inputDirWorld <- '../../../../../input/global_data/'
inputDirTreatment <- '../../../../../output/wtt/model/dwt/0_population/3_output_clean/'

outputDir <- '../../../../../output/wtt/model/dwt/1_volumes/1_allocation_desalination_SW_GW/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### files ####
countries.regions <- read.csv(
  paste0(inputDirWorld, 'general/countries_id_regions.csv')
)

#### water demands
countries.5arcmin <- vroom(
  paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'))

population.treatment.5arcmin.2015 <- vroom(
  paste0(inputDirTreatment, 'population_treatment_5arcmin_SAFE.csv'))

water.demands.2015 <- vroom(
  paste0(inputDirDWT, 'domestic_demands_2015_tot_m3.csv'))

#### desalination at 5 arcmin
desalination.5arcmin.2015 <- vroom(
  '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/desaldata_drinking_5arcmin_2015.csv') 


#### processing ####
desalination.country.files <- str_remove(list.files(inputDir), '.csv')

global.allocated.drinking.desal.list <- list()

for(country in 1:length(desalination.country.files)){
  
  country.file <- vroom(paste0(inputDir, desalination.country.files[country], '.csv'),
                        show_col_types = F)
  
  global.allocated.drinking.desal.list[[country]] <- country.file
  
}

global.allocated.drinking.desal <- do.call(rbind, global.allocated.drinking.desal.list) 

global.desal.demands.pop <- global.allocated.drinking.desal %>% 
  rename(cell_ID=assigned.demands.cell_ID) %>% 
  inner_join(water.demands.2015 %>% 
    select(cell_ID, demands.m3.2015)) %>% 
  inner_join(population.treatment.5arcmin.2015 %>% 
               select(cell_ID, pop_2015_total, pop_2015_urban, 
                      pop_2015_rural, treatment))
# sum(global.desal.dcountries.regions# sum(global.desal.demands.pop$demands.m3.2015)/10^9

vroom_write(global.desal.demands.pop, 
            paste0(outputDir, 
                   'demands_domestic_allocated_to_desalination.csv'), ',')
