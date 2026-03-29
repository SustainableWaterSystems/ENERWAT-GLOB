rm(list=ls())

#### script to calculate pixel-level safe water access (AQUASTAT -> within 30 minutes walk) 
#### and connection to a water treatment plant (World Bank -> safely managed drinking water)


library(vroom)
library(dplyr)
library(FNN)
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

#### read files ####
inputDir <- 
  '../../../../../output/wtt/model/dwt/0_population/0_preprocessing/'
inputDirWorld <- '../../../../../input/global_data/'
outputDirTemp <- 
  '../../../../../output/wtt/model/dwt/0_population/0_preprocessing/0_temp/'
dir.create(outputDirTemp, recursive = T, showWarnings = F)

# statistics and absolute numbers of people with water access per country (2015)
countries.water.access <- read.csv(paste0(
  inputDir, 'countries_safe_water_access_input.csv'))

#5 arcmin table of countries pixel with label
countries.raster.5arcmin <- vroom(paste0(
  inputDirWorld, 'general/countries_raster_5arcmin.csv'))

#5 arcmin table of population (2015)
population.data.5arcmin <- vroom(paste0(
  inputDir, 'population_information_5arcmin.csv')) %>% 
  filter(pop_2015_total != 0) %>% 
  inner_join(countries.raster.5arcmin %>% 
               select(cell_ID, Country)) %>% 
  filter(!is.na(Country))

# 5 arcmin GDP upscaled from 1km (Chen 2022) #-> try different upscaling procedures
gdp.data.5arcmin <- vroom(paste0(
  inputDirWorld, 'general/gdp_ppp_1km_chen2022_to_5arcmin_2015.csv')) %>% 
  filter(GDP_total_2015 != -9999) %>%  
  filter(GDP_total_2015 != 0) %>% 
  inner_join(countries.raster.5arcmin %>% 
               select(cell_ID, Country)) %>% 
  filter(!is.na(Country))


sum(gdp.data.5arcmin$GDP_total_2015)
sum(countries.water.access$pop_total)
sum(population.data.5arcmin$pop_2015_total)

length(unique(countries.water.access$Country))
length(unique(population.data.5arcmin$Country))
length(unique(gdp.data.5arcmin$Country))

setdiff(unique(gdp.data.5arcmin$Country),
        unique(population.data.5arcmin$Country))
setdiff(unique(population.data.5arcmin$Country),
        unique(gdp.data.5arcmin$Country))

setdiff(unique(population.data.5arcmin$Country),
        unique(countries.water.access$Country))
setdiff(unique(population.data.5arcmin$Country),
        unique(countries.water.access$Country))




#### filtering : match all countries ####

#get countries that exist in water access data, population data and gdp data
common.countries.in.all.data <- intersect(
  unique(population.data.5arcmin$Country),
  unique(countries.water.access$Country)
) %>% 
  intersect(., unique(gdp.data.5arcmin$Country))

water.access.statistics.matching <- countries.water.access %>% 
  filter(Country %in% common.countries.in.all.data)
population.5arcmin.matching <- population.data.5arcmin %>% 
  filter(Country %in% common.countries.in.all.data)
gdp.5arcmin.matching <- gdp.data.5arcmin %>% 
  filter(Country %in% common.countries.in.all.data)

#### processing begins ####
# match gdp with population at 5arcmin
first.join.5arcmin <- inner_join(population.5arcmin.matching, 
                                 gdp.5arcmin.matching %>% 
                                   select(cell_ID, GDP_total_2015))

#### check which gdp pixels with a value do not have a correspondent population
unassigned.gdp <- gdp.5arcmin.matching[
  gdp.5arcmin.matching$cell_ID %in%
    setdiff(gdp.5arcmin.matching$cell_ID, first.join.5arcmin$cell_ID),]

sum(unassigned.gdp$GDP_total_2015) #7.4 trillion USD of unassigned GDP
sum(unassigned.gdp$GDP_total_2015) / sum(gdp.5arcmin.matching$GDP_total_2015) # 6% of total 2015 GDP data

#### check which population pixels were not matched (total GPD == 0)
unassigned.pop <- population.5arcmin.matching[
  population.5arcmin.matching$cell_ID %in%
    setdiff(population.5arcmin.matching$cell_ID, first.join.5arcmin$cell_ID),

]

sum(unassigned.pop$pop_2015_total) # ~104 million unassigned people
sum(unassigned.pop$pop_2015_urban) #43 million urban
sum(unassigned.pop$pop_2015_rural) #61 million rural

sum(unassigned.pop$pop_2015_total) / sum(population.5arcmin.matching$pop_2015_total) #1.5%
sum(unassigned.pop$pop_2015_urban) / sum(population.5arcmin.matching$pop_2015_urban) #1.2%
sum(unassigned.pop$pop_2015_rural) / sum(population.5arcmin.matching$pop_2015_rural) #1.8%



#### check which countries fall in which case of non-assignment 
# (or which ones have no more issues - full assignment)

countries.unassigned.gdp <- unique(unassigned.gdp$Country)
countries.unassigned.pop <- unique(unassigned.pop$Country)
countries.unassigned.both <- intersect(countries.unassigned.pop, countries.unassigned.gdp)

# 26 countries ok + 148 to clean 

#### begin by saving countries where all pixels matched at first try
countries.ok <- common.countries.in.all.data[
  !common.countries.in.all.data 
  %in% unique(c(countries.unassigned.gdp, countries.unassigned.pop))] 

countries.ok.first.try.df <- first.join.5arcmin %>% 
  filter(Country %in% countries.ok)

sum(countries.ok.first.try.df$pop_2015_total)
sum(countries.ok.first.try.df$pop_2015_total) / 
  sum(population.5arcmin.matching$pop_2015_total)

vroom_write(countries.ok.first.try.df, 
            paste0(outputDirTemp, '00_countries_matching_first_try.csv'), ',')




#### get remaining countries
countries.to.assign <- common.countries.in.all.data[
  common.countries.in.all.data 
  %in% unique(c(countries.unassigned.gdp, countries.unassigned.pop))] 

first.join.5arcmin.filtered <- first.join.5arcmin %>% 
  filter(Country %in% countries.to.assign)
  
  
#### do per case
### final validation : gdp / capita per country (world bank good data)

## Level 1. GDP to assign but no more population (*)
# # DEPRECATED assign gdp to closest population cell
# # DECISION these gpd pixels get simply removed (errors in downscaling / upscaling)
source('fun_0_1_0_remove_unassigned_gdp.R')

## Level 2. Population to assign but no more GDP (**)
# DEPRECATED move unassigned population to closest population pixel
# DECIDED remove these 5 countries -> high uncertainty on nighttime light data
# e.g. 95% are in North Korea
# total population of these 5 countries: 36,366,386
source('fun_0_1_1_remove_pop_only.R')
# 
# ## Level 3. Both contain unassigned values (***)
# # first assign gdp to closest population pixel (out of all in a country)
# # then pick threshold distance (same approach as wwt)
# # finally move remaining population (as in 2)
source('fun_0_1_2_assign_both_gdp_and_population.R')
