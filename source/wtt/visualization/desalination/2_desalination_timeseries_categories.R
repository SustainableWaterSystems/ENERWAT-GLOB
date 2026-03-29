rm(list=ls())

library(dplyr)
library(vroom)
library(tidyr)
library(ggplot2)
library(ggtext)
library(patchwork)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir  <- '../../../../output/wtt/model/desalination/0_tech_categories/'
outputDir <- '../../../../output/wtt/visualization/desalination/'

source('fun_2_0_capacity_energy_per_tech_evolving_RO.R')
source('fun_2_1_capacity_energy_per_subregion_evolving_RO.R')
source('fun_2_2_capacity_energy_per_feed_evolving_RO.R')

#### 4 x 3 grid #### 
#cumulative capacity plots
m3.day.plot.tech <- m3.day.plot.tech  +
  ggtitle('A. Technologies\n') +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        axis.text = element_text(size=14), 
        axis.title.y = element_markdown(lineheight = 1.5, size=16))

m3.day.plot.subr <- m3.day.plot.subr +
  ggtitle('B. Subregions\n') +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        axis.text.y=element_blank(),
        axis.title.y=element_blank())

m3.day.plot.feed <- m3.day.plot.feed  +
  ggtitle('C. Feedwater types\n') +
  theme(plot.title = element_text(hjust = 0.5, size=18),
        axis.text.y=element_blank(),
        axis.title.y=element_blank())


#cumulative energy plots
twh.day.plot.tech <- twh.day.plot.tech +
  theme(axis.text = element_text(size=14),
        axis.title.y = element_markdown(lineheight = 1.5, size=16),
        plot.title = element_blank(),
        axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank())

twh.day.plot.subr <- twh.day.plot.subr +
  theme(plot.title = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y.right = element_blank())

twh.day.plot.feed <- twh.day.plot.feed +
  theme(plot.title = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y.right = element_blank())

#capacity ratio plots
ratio.capacity.plot.tech <- ratio.capacity.plot.tech +
  theme(axis.text.y = element_text(size=14),
        axis.title.y = element_markdown(lineheight = 1.5, size=16),
        plot.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

ratio.capacity.plot.subr <- ratio.capacity.plot.subr +
  theme(plot.title = element_blank(),axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

ratio.capacity.plot.feed <- ratio.capacity.plot.feed +
  theme(plot.title = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

#energy ratio plots
ratio.energy.plot.tech <- ratio.energy.plot.tech +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(lineheight = 1.5, size=16),
        plot.title = element_blank())

ratio.energy.plot.subr <- ratio.energy.plot.subr +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=16),
        plot.title = element_blank())

ratio.energy.plot.feed <- ratio.energy.plot.feed +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank())

#final plot
final.plot.vertical <- (m3.day.plot.tech + m3.day.plot.subr + m3.day.plot.feed) /
  (twh.day.plot.tech + twh.day.plot.subr + twh.day.plot.feed) /
  (ratio.capacity.plot.tech + ratio.capacity.plot.subr + ratio.capacity.plot.feed) / 
  (ratio.energy.plot.tech + ratio.energy.plot.subr + ratio.energy.plot.feed) +
  plot_layout(guides='collect') &
  theme(legend.justification = 'center',
        legend.title = element_text(hjust=0.5, size=16),
        legend.text = element_text(size=14),
        legend.position="bottom",
        legend.direction = 'vertical',
        legend.spacing= unit(3.0, 'cm'))
# #save
ggsave(paste0(outputDir,'desal_grid_plot.png'), final.plot.vertical,
       height=10, width=12, units='in', dpi=300)

file.show(paste0(outputDir,'desal_grid_plot.png'))
