rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggpattern)
library(ggtext)
library(cowplot)
library(reshape2)
library(patchwork)

my.palette <- c('#1E81B0',"#BEAED4", '#648FFF','#FE6100')
my.palette2 <- c('#648FFF','#FE6100')
my.palette3 <- c("#54A29B","#c6c507")

####
inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

#### ####
year.discharge.types <- read.csv(
  paste0(inputDirEnergy,
         '5_1_discharge_by_intake_type_timeseries_country.csv')) %>%
  mutate(datetime = as.Date(datetime))

year.production <- read.csv(paste0(inputDirEnergy, '0_energy_production_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

year.pumping <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

energy.countries.ts <- read.csv(paste0(inputDirEnergy, '6_energy_countries_1980_2021.csv')) %>% 
  mutate(datetime = as.Date(datetime))

#### south_africa to south africa ####
year.pumping$country[which(year.pumping$country == 'South_Africa')] <- 'South Africa'
year.production$country[which(year.production$country == 'South_Africa')] <- 'South Africa'
energy.countries.ts$Country[which(energy.countries.ts$Country == 'South_Africa')] <- 'South Africa'
year.discharge.types$country[which(year.discharge.types$country == 'South_Africa')] <- 'South Africa'


#### functions ####
source('fun_4_0_country_discharge.R')
source('fun_4_1_country_energy_budget.R')
source('fun_4_2_country_energy_intensity.R')

#### patch and save ####
p.energy.combined <- plot_grid(p.10.discharge,
                               p.energy.values,
                               p.intensity,
                               nrow=3)



ggsave(paste0(outputDirViz, 'country_energy.png'), p.energy.combined,
       height=14, width=10, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz, 'country_energy.png'))




#### paper stats ####
mean(as.numeric(df.intensity.c$low), na.rm=T)
mean(as.numeric(df.intensity.c$mean), na.rm=T)
mean(as.numeric(df.intensity.c$high), na.rm=T)

mean(as.numeric(df.intensity.p$low), na.rm=T)
mean(as.numeric(df.intensity.p$mean), na.rm=T)
mean(as.numeric(df.intensity.p$high), na.rm=T)

sum(first.10.pumping$energy.twh.low)
sum(first.10.pumping$energy.twh.mean)
sum(first.10.pumping$energy.twh.high)

sum(last.10.pumping$energy.twh.low)
sum(last.10.pumping$energy.twh.mean)
sum(last.10.pumping$energy.twh.high)

sum(first.10.hydro$energy.twh.low)
sum(first.10.hydro$energy.twh.mean)
sum(first.10.hydro$energy.twh.high)

sum(last.10.hydro$energy.twh.low)
sum(last.10.hydro$energy.twh.mean)
sum(last.10.hydro$energy.twh.high)

sum(first.10.discharge$km3.total)
sum(last.10.discharge$km3.total)

mean(year.discharge.types$km3.production, na.rm=T)
mean(year.discharge.types$km3.consumption, na.rm=T)
mean(year.discharge.types$km3.discharge.only, na.rm=T)
