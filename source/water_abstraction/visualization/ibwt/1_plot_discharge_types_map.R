rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggtext)
library(ggthemes)
library(patchwork)


palette.timeseries <- c("#000000", "#BEAED4", '#648FFF','#FE6100') 
palette.map <- c("#7FC97F", '#FFB000', '#785EF0', "#F0027F") 

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

wg <- map_data("world")


#### load ####
sections.coord <- read.csv(paste0(inputDirDischarge, '0_timeseries_raw/0_information_sections.csv'))

section.info.df <- read.csv(
  paste0(inputDirDischarge, '0_timeseries_raw/1_0_bias_correction_sections.csv'))
sections.reservoirs <- read.csv(
  paste0(inputDirElevation, '0_0_sections_reservoirs.csv'))

discharge.monthly.df <- read.csv(
  paste0(inputDirDischarge, 
         '1_bias_corrected/0_0_discharge_bias_corrected_monthly.csv')) %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(
    year.continuous = (year(datetime)),
    year.factor = factor(year(datetime)),     # use year to define separate curves
    date = update(datetime, year = 1))  # use a constant year for the x-axis

discharge.yearly.df <- read.csv(
  paste0(inputDirDischarge, 
         '1_bias_corrected/0_1_discharge_bias_corrected_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

discharge.types <- read.csv(
  paste0(inputDirEnergy, 
         '5_0_discharge_by_intake_type_timeseries.csv')) %>% 
  mutate(datetime = as.Date(datetime))

#### ####

source('fun_1_0_plot_discharge_timeseries_types.R')
source('fun_1_1_plot_discharge_map.R')

discharge.ts.map <- p.discharge.y.types / p.map.discharge

ggsave(paste0(outputDirViz,'ibwt_discharge_ts_map.png'), discharge.ts.map,
       height=11, width=8, units='in', dpi=300, bg='white')
file.show(paste0(outputDirViz,'ibwt_discharge_ts_map.png'))

