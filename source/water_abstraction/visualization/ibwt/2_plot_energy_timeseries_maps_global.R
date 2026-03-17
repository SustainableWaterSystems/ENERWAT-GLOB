rm(list=ls())

library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(ggtext)

my.palette <- c('#88CCEE','#44AA99', '#E3A43E', '#E56529')
palette.energy <- c('#648FFF','#FE6100')

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

#### load ####
segments.info <- read.csv(paste0(inputDirDischarge, '2_infrastructure/1_information_pumping.csv'))

# year.hydropower <- read.csv(paste0(inputDirEnergy, '0_energy_production_yearly.csv')) %>% 
#   mutate(datetime = as.Date(datetime))
# 
# year.pumping <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_yearly.csv')) %>% 
#   mutate(datetime = as.Date(datetime))

wg <- map_data("world")

production.monthly <- read.csv(
  paste0(inputDirEnergy, '0_energy_production_monthly.csv'))
production.yearly <- read.csv(
  paste0(inputDirEnergy, '0_energy_production_yearly.csv'))

consumption.monthly <- read.csv(
  paste0(inputDirEnergy, '1_energy_consumption_monthly.csv'))
consumption.yearly <- read.csv(
  paste0(inputDirEnergy, '1_energy_consumption_yearly.csv'))

sections.coord <- read.csv(paste0(inputDirDischarge, '0_timeseries_raw/0_information_sections.csv')) 
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

p.data <- rbind(production.plot.data.y,
                consumption.plot.data.y
                )

p.budget <- ggplot(p.data %>% 
                     mutate(
                       energy = factor(energy, levels = c('production','consumption'))), 
                   aes(x=datetime, y=energy.twh.mean, color=energy, fill=energy)) +
  geom_line(linewidth=1.2) +
  geom_ribbon(aes(x=datetime, 
                  ymin=energy.twh.low, 
                  ymax=energy.twh.high,
                  color=energy,
                  fill=energy),
              alpha=0.2, 
              inherit.aes=F, show.legend=F) +
  # scale_color_manual(values = c("#54A29B","#c6c507"),
  scale_color_manual(values = palette.energy,
                     labels = c('Energy production','Energy consumption')) +
  # "#BEAED4", "#F0027F"
  scale_fill_manual(values = palette.energy) +
  ggtitle('A. Modelled total energy production and consumption') +
  xlab('Year') +
  ylab('TWh y<sup>-1</sup>') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title = element_markdown(size=14),
        axis.text = element_text(size=12),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 13)
  ) +
  guides(color=guide_legend(override.aes = list(size = 3, shape = 22, linewidth = 5)))

source('fun_2_0_plot_map_production.R')
source('fun_2_1_plot_map_consumption.R')


p.energy.mix <- (p.budget / production.w.leg / plot_spacer() / consumption.w.leg ) +
  plot_layout(heights = c(1, 1.4, 0.05, 1.4))

#### save ####
ggsave(paste0(outputDirViz,'ibwt_energy_global.png'), p.energy.mix,
       height=15, width=11, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz,'ibwt_energy_global.png'))
