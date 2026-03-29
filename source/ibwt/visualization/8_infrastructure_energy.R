rm(list=ls())

library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(ggtext)

my.palette <- c('#88CCEE','#44AA99', '#E3A43E', '#E56529')
palette.energy <- c('#648FFF','#FE6100')

inputDir <- '../../../output/ibwt/model/'
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../output/ibwt/visualization/'

#### load ####
segments.info <- read.csv(paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))

wg <- map_data("world")

production.monthly <- read.csv(
  paste0(inputDirEnergy, '0_energy_production_monthly.csv'))
production.yearly <- read.csv(
  paste0(inputDirEnergy, '0_energy_production_yearly.csv'))

consumption.monthly <- read.csv(
  paste0(inputDirEnergy, '1_energy_consumption_monthly.csv'))
consumption.yearly <- read.csv(
  paste0(inputDirEnergy, '1_energy_consumption_yearly.csv'))

sections.coord <- read.csv(paste0(inputDirDischarge, '1_bias_corrected/0_information_sections.csv')) 
intakes.coord.hydropower <- read.csv(paste0(inputDirDischarge, '2_infrastructure/0_information_hydropower.csv'))
intakes.coord.pumping <- read.csv(paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))


#### prepare data ####
production.plot.data.y <- production.yearly %>%
  group_by(datetime) %>%
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>%
  mutate(datetime = as.Date(datetime),
         energy = 'production')

consumption.plot.data.y <- consumption.yearly %>% 
  group_by(datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  mutate(datetime = as.Date(datetime),
         energy = 'consumption')

#### calculate linear trends of production and consumption ####
production.yearly.idx <- production.plot.data.y %>% 
  mutate(idx = seq(nrow(consumption.plot.data.y)))

consumption.yearly.idx <- consumption.plot.data.y %>% 
  mutate(idx = seq(nrow(consumption.plot.data.y)))

lm.production.low <- lm(energy.twh.low~idx, data=production.yearly.idx)
lm.production.mean <- lm(energy.twh.mean~idx, data=production.yearly.idx)
lm.production.high <- lm(energy.twh.high~idx, data=production.yearly.idx)
lm.consumption.low <- lm(energy.twh.low~idx, data=consumption.yearly.idx)
lm.consumption.mean <- lm(energy.twh.mean~idx, data=consumption.yearly.idx)
lm.consumption.high <- lm(energy.twh.high~idx, data=consumption.yearly.idx)

summary(lm.production.low)
summary(lm.production.mean)
summary(lm.production.high)
summary(lm.consumption.low)
summary(lm.consumption.mean)
summary(lm.consumption.high)

#### continue processing for plot data ####
p.data <- rbind(production.plot.data.y,
                consumption.plot.data.y
                )

source('fun_8_0_plot_map_production.R')
source('fun_8_1_plot_map_consumption.R')


p.energy.mix <- plot_grid(p.map.hydropower, p.map.pumping, nrow=2)

#### save ####
ggsave(paste0(outputDirViz,'ibwt_energy_global_appendix.png'), p.energy.mix,
       height=14, width=12, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz,'ibwt_energy_global_appendix.png'))
