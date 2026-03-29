rm(list=ls())

#use preprocessing tables to make final gdp and population dataframes
#169 countries 

library(dplyr)
library(vroom)

inputDir <- '../../../../../output/wtt/model/dwt/0_population/0_preprocessing/'
inputDirFilters <- 
  '../../../../../output/wtt/model/dwt/0_population/0_preprocessing/0_temp/'
inputDirWorld <- '../../../../../input/global_data/'
outputDir <- '../../../../../output/wtt/model/dwt/0_population/1_input/'

#### read files ####
#### 5 arcmin data
#5 arcmin table of countries pixel with label
countries.raster.5arcmin <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'))

#5 arcmin table of population (2015)
population.data.5arcmin <- vroom(paste0(inputDir, 'population_information_5arcmin.csv')) %>% 
  filter(pop_2015_total != 0) %>% 
  inner_join(countries.raster.5arcmin %>% 
               select(cell_ID, Country)) %>% 
  filter(!is.na(Country))

# 5 arcmin GDP upscaled from 1km (Chen 2022) #-> try different upscaling procedures
gdp.data.5arcmin <- vroom(paste0(inputDirWorld, 'general/gdp_ppp_1km_chen2022_to_5arcmin_2015.csv')) %>% 
  filter(GDP_total_2015 != -9999) %>%  
  filter(GDP_total_2015 != 0) %>% 
  inner_join(countries.raster.5arcmin %>% 
               select(cell_ID, Country)) %>% 
  filter(!is.na(Country))

#### water access and filtered cell IDs 

# statistics and absolute numbers of people with water access per country (2015)
countries.water.access <- read.csv(paste0(inputDir, 'countries_safe_water_access_input.csv'))

uncertain.population.one <- read.csv(paste0(inputDirFilters, '02_unmatched_population.csv'))
uncertain.population.two <- read.csv(paste0(inputDirFilters, '03_population_removed.csv'))
removed.gdp.one <- read.csv(paste0(inputDirFilters, '01_removed_gdp_5arcmin.csv'))
removed.gdp.two <- read.csv(paste0(inputDirFilters, '03_gdp_removed.csv'))

countries.complete <- read.csv(paste0(inputDirFilters, '00_countries_matching_first_try.csv'))
countries.matched.extra.gdp <- read.csv(paste0(inputDirFilters, '01_countries_matching_extra_gdp.csv'))
countries.matched.extra.pop <- read.csv(paste0(inputDirFilters, '02_countries_matching_uncertain_nightlight.csv'))
countries.matched.closest <- read.csv(paste0(inputDirFilters, '03_countries_matching_with_removals.csv'))


#### checksums
sum(uncertain.population.one$pop_2015_total) #correct
sum(uncertain.population.two$pop_2015_total) #wrong
sum(removed.gdp.one$GDP_total_2015) #close to correct
sum(removed.gdp.two$GDP_total_2015) #wrong

sum(countries.complete$pop_2015_total) #correct
sum(countries.matched.extra.gdp$pop_2015_total)
sum(countries.matched.extra.pop$pop_2015_total) #correct
sum(countries.matched.closest$pop_2015_total)

#### processing ####
#### final dataframe, matching a population pixel with a gdp pixel
final.input.df <- rbind(countries.complete, 
                        countries.matched.extra.gdp,
                        countries.matched.extra.pop,
                        countries.matched.closest) 

final.input.df.vars <- final.input.df %>% 
  mutate(GDP_capita_2015 = GDP_total_2015 / pop_2015_total)  

sum(final.input.df$pop_2015_total)
sum(final.input.df$pop_2015_urban)
sum(final.input.df$pop_2015_rural)
sum(final.input.df$GDP_total_2015)

length(unique(final.input.df$Country))


#### removed population (uncertain nighttime data)
removed.pop <- rbind(uncertain.population.one, uncertain.population.two)
removed.gdp <- rbind(removed.gdp.one, 
                     removed.gdp.two %>% 
                       select(cell_ID_source, lon, lat, GDP_total_2015, Country) %>% 
                       rename(cell_ID = cell_ID_source))


#### save things ####
write.csv(countries.water.access,
          paste0(outputDir, 'countries_safe_water_access_input.csv'),
          row.names = F)

vroom_write(removed.pop,
            paste0(outputDir, 'unmatched_population_cells.csv'), ',')

vroom_write(removed.gdp,
            paste0(outputDir, 'unmatched_gdp.csv'), ',')

vroom_write(final.input.df.vars,
            paste0(outputDir, 'pop_gdp_input_df.csv'), ',')
