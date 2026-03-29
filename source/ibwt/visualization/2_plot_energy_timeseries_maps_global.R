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
inputDirInfo <-
  '../../../output/ibwt/model/1_discharge_timeseries/1_bias_corrected/'
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

ibwt.info <- read.csv(paste0(inputDirInfo, '1_0_bias_correction_sections.csv'))

#### prepare data ####
#### timeseries 
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

#### maps 
source('fun_2_make_ibwt_energy_plot_data.R')

#### timeseries ####
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
  theme(plot.title = element_text(hjust = 0.5, size=18),
        axis.title = element_markdown(size=16),
        axis.text = element_text(size=15),
        legend.position = 'bottom',
        legend.box="vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.margin = margin(0,0,0.5,0, "cm")
  ) +
  guides(color=guide_legend(override.aes = list(size = 3, shape = 22, linewidth = 5), nrow=2))

###Inter-basin transfer locations - Energy production
p.map.production <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = ibwt.plot.df.production, 
             aes(
               x = lon.center, y = lat.center, #shape = purpose.main,
               fill = energy.m3.kwh.mean, size = energy.twh.mean),
             alpha = 0.6, stroke = 1, shape=21)+
  ylim(-55,70) +
  ggtitle('B. Per-transfer energy production (1980-2021)') +
  scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
                        breaks=c(1,5,10,20,30),
                        range=c(3,10)) +
  scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (m<sup>3</sup> kwh<sup>-1</sup>)',
                      limits = c(0,15)) + 
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(1.0, 'cm'),
        legend.box="vertical",
        legend.title = element_markdown(hjust=0.5, size=16),
        legend.title.position = 'top',
        legend.text = element_text(size=15),
        legend.text.position = 'bottom',
        legend.key.width = unit(0.2, 'in'),
        plot.title = element_text(hjust = 0.5, size=18, face='plain')) +
  guides(size=guide_legend(override.aes=list(fill='black'), order=1))


p.map.consumption <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = ibwt.plot.df.consumption, 
             aes(
               x = lon.center, y = lat.center, #shape = purpose.main,
               fill = energy.kwh.m3.mean, size = energy.twh.mean),
             alpha = 0.6, stroke = 1, shape=21)+
  ylim(-55,70) +
  ggtitle('C. Per-transfer energy consumption (1980-2021)') +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,4,8,12,17),
                        range=c(3,10)) +
  scale_fill_gradient(low="#e1af93", high= "#fbdba0", name = 'Consumption intensity (kwh m<sup>3</sup>)',
                      limits = c(0,3)) + 
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(1.0, 'cm'),
        legend.box="vertical",
        legend.title = element_markdown(hjust=0.5, size=16),
        legend.title.position = 'top',
        legend.text = element_text(size=15),
        legend.text.position = 'bottom',
        legend.key.width = unit(0.2, 'in'),
        plot.title = element_text(hjust = 0.5, size=18, face='plain')) +
  guides(size=guide_legend(override.aes=list(fill='black'), order=1))


p.energy <- (p.budget / (p.map.production + p.map.consumption)) +
  plot_layout(heights = c(1.2,1))

#### save ####
ggsave(paste0(outputDirViz,'ibwt_energy_global.png'), p.energy,
       height=14, width=12, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz,'ibwt_energy_global.png'))
