rm(list=ls())

library(dplyr)
library(vroom)

inputDir <- '../../../../../input/wtt/dwt/'
inputDirWorld <- '../../../../../input/global_data/'
outputDir <- 
  '../../../../../output/wtt/model/dwt/0_population/0_preprocessing/'
dir.create(outputDir, recursive = T, showWarnings = F)

#### read files 
# small tables
#access to safely managed water
wb.access.2015 <- read.csv(
  paste0(inputDir, 'water_access_world_bank_2015.csv'))

#access rates for urban and rural
aquastat.urban.rural.2015 <- 
  read.csv(paste0(inputDir, 'water_access_AQUASTAT_2015.csv'))


# 5arcmin population
pop.total <- vroom(paste0(
  inputDirWorld, 'general/population_5arcmin/population_5arcmin_2015_total.csv'))
pop.urban <- vroom(paste0(
  inputDirWorld, 'general/population_5arcmin/population_5arcmin_2015_urban.csv'))
pop.rural <- vroom(paste0(
  inputDirWorld, 'general/population_5arcmin/population_5arcmin_2015_rural.csv'))

# 5 arcmin countries
countries.5arcmin <- 
  vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'))

# 5 arcmin cell area (for density calculations)
# it's in m2
# cellarea.5arcmin <- vroom(paste0(inputDirWorld, 'cellsize_5arcmin.csv'))

#### processing ####
#### make single dataframe of population

pop.df <- cbind(pop.total, pop.urban$pop_2015_urban) %>% 
  cbind(., pop.rural$pop_2015_rural)
colnames(pop.df) <- c('cell_ID','lon','lat',
                      'pop_2015_total','pop_2015_urban','pop_2015_rural')

pop.df.clean <- pop.df %>% 
  filter(pop_2015_total != 0) %>% 
  inner_join(countries.5arcmin %>% 
               select(cell_ID, Country)) %>% 
  filter(!is.na(Country)) %>% 
  #give label urban (U), rural (R), urban > rural (UR), rural > urban (RU)
  mutate(pop_type_cell = case_when(
    pop_2015_urban != 0 & pop_2015_rural == 0 ~ "U", 
    pop_2015_urban == 0 & pop_2015_rural != 0 ~ "R", 
    pop_2015_urban > pop_2015_rural ~ "UR", 
    pop_2015_urban < pop_2015_rural ~ "RU"))

#check cell type 
sum(pop.df.clean$pop_type_cell == "U")
sum(pop.df.clean$pop_type_cell == "R")
sum(pop.df.clean$pop_type_cell == "UR")
sum(pop.df.clean$pop_type_cell == "RU")

countries.rural.urban <- pop.df.clean %>% 
  group_by(Country) %>% 
  summarise(pop_total = sum(pop_2015_total),
            pop_urban = sum(pop_2015_urban),
            pop_rural = sum(pop_2015_rural)
            )



#### country numbers access
source('fun_0_0_make_rural_urban_tidydf.R')


global.access.2015 <- inner_join(wb.access.2015, aquastat.rural.urban.df) %>%
  inner_join(., countries.rural.urban) %>% 
  select(Country, fill.2015.research, 
         access.aquastat.total, access.aquastat.urban, access.aquastat.rural,
         pop_total, pop_urban, pop_rural) %>%
  #calculate first the total (rural/urban separate does not compute) -> use urban/rural ratios
  mutate(access_pop_total_aquastat = pop_total * access.aquastat.total / 100,
         access_pop_total_wb = pop_total * fill.2015.research / 100) %>% 
  #rural/urban dummy to calculate ratio
  mutate(access_dummy_urban = pop_urban * access.aquastat.urban / 100,
         access_dummy_rural = pop_rural * access.aquastat.rural / 100) %>% 
  #calculate ratio of urban:rural access
  mutate(access_ratio_urban = 
            access_dummy_urban / (access_dummy_urban + access_dummy_rural),
          access_ratio_rural = 
            access_dummy_rural / (access_dummy_urban + access_dummy_rural)) %>% 
  #calculate acutal urban/rural numbers 
  mutate(access_pop_urban_aquastat = access_pop_total_aquastat * access_ratio_urban,
         access_pop_rural_aquastat = access_pop_total_aquastat * access_ratio_rural,
         access_pop_urban_wb = access_pop_total_wb * access_ratio_urban,
         access_pop_rural_wb = access_pop_total_wb * access_ratio_rural)

colnames(global.access.2015)[2] <- 'safe.access.wb.filled'

global.access.2015.clean <- global.access.2015 %>% 
  select(Country, pop_total, pop_urban, pop_rural,
         access_pop_total_aquastat, access_pop_urban_aquastat, access_pop_rural_aquastat,
         access_pop_total_wb, access_pop_urban_wb, access_pop_rural_wb)


#### save dataframes ####
write.csv(global.access.2015, paste0(outputDir, 'countries_safe_water_access_allinfo.csv'),
          row.names = F)
write.csv(global.access.2015.clean, paste0(outputDir, 'countries_safe_water_access_input.csv'),
          row.names = F)
vroom_write(pop.df.clean, paste0(outputDir, 'population_information_5arcmin.csv'),
            ',')