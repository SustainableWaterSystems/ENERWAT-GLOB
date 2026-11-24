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

inputDir <- '../../../output/ibwt/model/'
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../output/ibwt/visualization/'

#### load ####
validation.driscoll <- read.csv(
  paste0(inputDirEnergy, '7_validation_driscoll_yearly.csv'))

p.driscoll <- ggplot(validation.driscoll)+
  geom_point(aes(x=energy.twh.reported, y=energy.twh.mean, color=transfer.name), size=2) +
  geom_errorbar(aes(x=energy.twh.reported, ymin=energy.twh.low, ymax=energy.twh.high, color=transfer.name)) +
  geom_abline(slope=1) + 
  xlab('Reported energy consumption (TWh y<sup>-1</sup>)')+
  ylab('Modelled energy consumption (TWh y<sup>-1</sup>)')+
  xlim(c(0,10))+
  ylim(c(0,10))+
  theme_bw() +
  theme(axis.title = element_markdown(),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5)) +
  guides(shape=guide_legend(title="Inter-basin water transfer", override.aes = list(size = 3)))


ggsave(paste0(outputDirViz,'validation_energy_driscoll.png'), p.driscoll,
       height=5, width=8, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz,'validation_energy_driscoll.png'))

