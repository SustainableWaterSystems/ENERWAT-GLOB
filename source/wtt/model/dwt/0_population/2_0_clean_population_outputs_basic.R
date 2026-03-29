rm(list=ls())

library(vroom)
library(dplyr)


inputDir <- '../../../../../output/wtt/model/dwt/0_population/'
outputDir <- '../../../../../output/wtt/model/dwt/0_population/3_output_clean/'
dir.create(outputDir, recursive = T, showWarnings = F)


#### read files ####

countries.5arcmin <- vroom('../../../../../input/global_data/general/countries_raster_5arcmin.csv')

treated.population.5arcmin <- vroom(paste0(inputDir, '2_output/treated_population_BASIC.csv'))
untreated.population.5arcmin <- vroom(paste0(inputDir, '2_output/untreated_population_BASIC.csv'))
unmatched.population.5arcmin <- vroom(paste0(inputDir, '1_input/unmatched_population_cells.csv'))

#### processing ####
country.pixel.count <- countries.5arcmin %>% 
  group_by(Country) %>% summarise(px.total=n())
unmatched.country.count <- unmatched.population.5arcmin %>% 
  group_by(Country) %>% summarise(px.unmatched=n())
unmatched.pixel.ratio <- inner_join(unmatched.country.count, country.pixel.count) %>% 
  mutate(px.unmatched.ratio = px.unmatched / px.total * 100)

unmatched.to.remove <-  unmatched.pixel.ratio[
  unmatched.pixel.ratio$px.unmatched.ratio >= 25,
]
unmatched.to.untreated <- unmatched.population.5arcmin[
  unmatched.population.5arcmin$Country %in% 
    unique(setdiff(unmatched.pixel.ratio$Country, unmatched.to.remove$Country)),]


#### final population dataframes ####
untreated.final.df <- untreated.population.5arcmin[
  !untreated.population.5arcmin$Country %in% unmatched.to.remove$Country,
] %>% 
  mutate(treatment = 0)

treated.final.df <- treated.population.5arcmin[
  !treated.population.5arcmin$Country %in% unmatched.to.remove$Country,
] %>% 
  mutate(treatment = 1)

treatment.status.df <- rbind(treated.final.df, untreated.final.df) %>% 
  arrange(cell_ID)

vroom_write(treatment.status.df, paste0(outputDir, 'population_treatment_5arcmin_BASIC.csv'),',')