rm(list=ls())

library(vroom)
library(dplyr)
library(parallel)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDirWorld <- '../../../../../input/global_data/'
inputDirDWT <- '../../../../../input/wtt/dwt/'
inputDirTreatment <- '../../../../../output/wtt/model/dwt/0_population/3_output_clean/'

inputDirDistances <- '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/0_desalination_distances/'

outputDir <- '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/1_desalination_allocation/'
dir.create(outputDir, recursive = T, showWarnings = F)

#### files ####
countries.5arcmin <- vroom(
  paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'))

population.treatment.5arcmin.2015 <- vroom(
  paste0(inputDirTreatment, 'population_treatment_5arcmin_SAFE.csv'))

#### water demands
water.demands.2015 <- vroom(
  paste0(inputDirDWT, 'domestic_demands_2015_tot_m3.csv'))

#### desalination at 5 arcmin
desalination.5arcmin.2015 <- vroom(
  '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/desaldata_drinking_5arcmin_2015.csv') 


#### processing ####

#### pre-filter countries for desal capacities and demands
treatment.countries <- unique(population.treatment.5arcmin.2015$Country)

treatment.demands <- water.demands.2015 %>%
  filter(demands.m3.2015 != 0) %>%
  inner_join(countries.5arcmin) %>%
  filter(Country %in% treatment.countries) %>% 
  inner_join(population.treatment.5arcmin.2015 %>% select(cell_ID, treatment)) %>% 
  filter(treatment == 1)

unique(treatment.demands$treatment)
sum(treatment.demands$demands.m3.2015)

desalination.gridcells <- desalination.5arcmin.2015 %>% 
  filter(drinking.desalination.m3.y != 0) %>% 
  inner_join(countries.5arcmin) %>% 
  filter(Country %in% treatment.countries)

desalination.countries <- sort(unique(desalination.gridcells$Country))



allocate_demands_to_desalination <- function(country){
  
  # country=1
  country.sel <- desalination.countries[country]
  
  country.demands <- treatment.demands %>% 
    filter(Country == country.sel)
  
  country.plants <- desalination.gridcells %>% 
    filter(Country == country.sel)
  
  inputDirCountryDesalDist <- paste0(inputDirDistances, country.sel, '/')
  
  replaceMessage(paste0('Assigning drinking desalinated water... ',
                        'Country ', country, '/', length(desalination.countries),
                        ' : ', country.sel))
  
  assigned.country.demands.ids.list <- list()
  for(plant in seq(1, nrow(country.plants))){
    
    plant.sel <- country.plants[plant,]
    
    demands.distance.from.plant <- read.csv(
      paste0(inputDirCountryDesalDist, 'distance_from_plant_',
             as.character(plant.sel$cell_ID), '.csv')) 
    
    
    #unzip assigned demands and remove from current plant 
    #so to avoid assignments of same demand pixel to more plants
    #AND remove demand pixels that have higher demand than capacity
    #so that each plant gets at least one pixel assigned
    
    demands.cells.to.remove <- do.call(rbind, assigned.country.demands.ids.list)
    
    demands.distance.from.plant.filtered <- demands.distance.from.plant %>% 
      filter(cell_ID %in% treatment.demands$cell_ID) %>% 
      filter(!cell_ID %in% demands.cells.to.remove$assigned.demands.cell_ID) %>% 
      filter(demands.m3.2015 <= plant.sel$drinking.desalination.m3.y) 
    
    
    #### 3. order demands by distance
    demands.ordered <- demands.distance.from.plant.filtered %>% 
      arrange(distance) 
    
    df.selection <- 1
    repeat{
      
      #get cap for assigned people 
      plant.capacity.cap <- plant.sel$drinking.desalination.m3.y
      
      #select number of rows until cap is exceeded
      pixels.assigned <- demands.ordered[1:df.selection, ]
      
      # if total population assigned exceeds the cap, break the loop
      if(sum(pixels.assigned$demands.m3.2015) > plant.capacity.cap |
         df.selection > nrow(demands.ordered)){break}
      
      else{df.selection <- df.selection + 1}
      
      
    }
    
    #get selection of demands assigned to selected plant
    assigned.demands <- demands.ordered[1:df.selection-1, ]
    
    #store assigned demands to remove from next plant
    assigned.country.demands.ids.list[[plant]] <- 
      data.frame(assigned.demands$cell_ID) %>%  
      mutate(desal.plant.cellid = plant.sel$cell_ID)
    
    
  }
  
  assigned.country.demands <- 
    do.call(rbind, assigned.country.demands.ids.list)
  
  vroom_write(assigned.country.demands, paste0(outputDir, country.sel, '.csv'), ',')
  
}


mclapply(1:length(desalination.countries), allocate_demands_to_desalination, mc.cores = 1)
