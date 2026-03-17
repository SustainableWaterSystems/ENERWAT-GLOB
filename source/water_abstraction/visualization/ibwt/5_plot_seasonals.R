rm(list=ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(patchwork)
library(ggtext)

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

wg <- map_data("world")

#### load ####
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

segments.info <- read.csv(paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))

year.pumping <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

month.pumping <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_monthly.csv'))%>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(
    year.continuous = (year(datetime)),
    year.factor = factor(year(datetime)),     # use year to define separate curves
    date = update(datetime, year = 1)  # use a constant year for the x-axis
  )

year.hydropower <- read.csv(paste0(inputDirEnergy, '0_energy_production_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

month.hydropower <- read.csv(paste0(inputDirEnergy, '0_energy_production_monthly.csv'))%>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(
    year.continuous = (year(datetime)),
    year.factor = factor(year(datetime)),     # use year to define separate curves
    date = update(datetime, year = 1)  # use a constant year for the x-axis
  )

source('fun_5_0_plot_seasonal_discharge.R')
source('fun_5_1_plot_seasonal_energy_production.R')
source('fun_5_2_plot_seasonal_energy_consumption.R')

p.production.legend <- ggplot(month.production.south, aes(x=date, y=energy.twh.mean, color = year.continuous)) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_line(alpha=0.1) +
  # ggtitle('F. Seasonal discharge (South)') +
  xlab('Month') +
  ylab('km<sup>3</sup> month<sup>-1</sup>') +
  # scale_y_continuous(position = "right") +
  scale_color_viridis_c(name='Year')+
  theme_bw()  +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust=0.5),
        legend.direction = 'horizontal',
        legend.title.position = 'top',
        legend.key.width = unit(1, 'cm')
  )

seasonal.legend <- get_legend(p.production.legend)

plots.ts <- plot_grid(p.seasonal.discharge,
              p.seasonal.production,
              p.seasonal.consumption,
              nrow=3)

p.seasonal.leg <- plot_grid(plots.ts,
                            seasonal.legend,
                            nrow = 2,
                            rel_heights = c(1,0.2))

#### save ####
ggsave(paste0(outputDirViz,'ibwt_seasonal.png'), p.seasonal.leg,
       height=12, width=9, units='in', dpi=300, bg='white')
file.show(paste0(outputDirViz,'ibwt_seasonal.png'))
