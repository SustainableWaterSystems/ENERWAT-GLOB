rm(list=ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(cowplot)


inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirInfrastructure <- paste0(inputDir, '1_discharge_timeseries/2_infrastructure/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

#### ####

hydropower <- read.csv(paste0(inputDirInfrastructure, '0_information_hydropower.csv'))
pumping <- read.csv(paste0(inputDirInfrastructure, '1_information_pumping.csv'))
bypass <- read.csv(paste0(inputDirInfrastructure, '2_information_bypass.csv'))

wg <- map_data("world")

#### plot ####
p.map.hydropower <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = hydropower,
             aes(
               x = reservoir.lon, y = reservoir.lat,
               fill=head.m, color=head.m
             ),
             alpha = 0.9, stroke = 0.3)+
  ylim(-55,70) +
  ggtitle('A. Power stations - Hydropower head') +
  scale_color_gradient(low="#54A29B", high="#c6c507") +
  scale_fill_gradient(low="#54A29B", high="#c6c507") +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.key.width = unit(1, 'cm')) +
  guides(color = guide_colorbar(title = "Hydropower head (m)"),
         fill = guide_colorbar(title = "Hydropower head (m)"))

p.map.pumping <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = pumping,
             aes(
               x = intake.lon, y = intake.lat,
               color=elevation.lift,
               fill=elevation.lift
             ),
             alpha = 0.9, stroke = 0.3)+
  ylim(-55,70) +
  ggtitle('B. Pumping stations - Pumping lift')  +
  scale_color_gradient(low="#54A29B", high="#c6c507") +
  scale_fill_gradient(low="#54A29B", high="#c6c507") +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.key.width = unit(1, 'cm')) +
  guides(color = guide_colorbar(title = "Pumping lift (m)"),
         fill = guide_colorbar(title = "Pumping lift (m)"))


p.map.bypass <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = bypass
             ,
             aes(
               x = intake.lon, y = intake.lat,
               color=net.bypass,
               fill=net.bypass, shape=bypass.type
             ),
             alpha = 0.9, stroke = 0.3)+
  ylim(-55,70) +
  ggtitle('C. Tunnels and aqueducts - Pumping lift bypass') +
  scale_color_gradient(low="#54A29B", high="#c6c507") +
  scale_fill_gradient(low="#54A29B", high="#c6c507") +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.key.width = unit(1, 'cm')) +
  guides(color = guide_colorbar(title = "Pumping lift bypass (m)"),
         fill = guide_colorbar(title = "Pumping lift bypass (m)"),
         shape = guide_legend(title = "Bypass type"))

#### patch ####
p.patch <- p.map.hydropower / p.map.pumping / p.map.bypass

ggsave(paste0(outputDirViz,'infrastructure_heights.png'), p.patch,
       height=10, width=7, units='in', dpi=300, bg='white')
file.show(paste0(outputDirViz,'infrastructure_heights.png'))
