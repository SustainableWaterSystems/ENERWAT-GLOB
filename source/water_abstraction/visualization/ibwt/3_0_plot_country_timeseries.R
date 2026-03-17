rm(list=ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(patchwork)
library(ggtext)


my.palette <- c('#332288', '#117733', '#88CCEE', '#E56529', '#DDCC77',
                '#E3A43E',  '#AA4499', '#882255', '#44AA99', '#441B1B')

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')
outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

#### load ####
energy.countries.ts <- read.csv(paste0(inputDirEnergy, '6_energy_countries_1980_2021.csv')) %>% 
  mutate(datetime = as.Date(datetime))
energy.countries.ts$Country[which(energy.countries.ts$Country == 'South_Africa')] <- 'South Africa'


#### actual values
p.p <- ggplot(energy.countries.ts 
              # %>% filter(twh.mean.p!=0)
) +
  geom_line(aes(x=datetime, y=twh.mean.p, color=Country), linewidth=0.75) +
  geom_ribbon(aes(x=datetime, 
                  ymin=twh.low.p, 
                  ymax=twh.high.p, 
                  color=Country), alpha=0.05, linetype ='dashed') +
  ggtitle('A. National energy production (%)') +
  scale_color_manual(values = my.palette) +
  xlab('Year')+
  ylab('TWh y<sup>-1</sup>') +
  ylim(c(0,50)) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5),
        plot.title = element_text(hjust=0.5, size=18),
        axis.title.y = element_markdown(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) 

p.c <- ggplot(energy.countries.ts 
              # %>% filter(twh.mean.c!=0)
) +
  geom_line(aes(x=datetime, y=twh.mean.c, color=Country), linewidth=0.75) +
  geom_ribbon(aes(x=datetime, 
                  ymin=twh.low.c, 
                  ymax=twh.high.c, 
                  color=Country), alpha=0.05, linetype ='dashed') +
  ggtitle('B. National energy consumption (%)') +
  scale_color_manual(values = my.palette) +
  xlab('Year')+
  ylab('TWh y<sup>-1</sup>') +
  scale_y_continuous(position = "right", limits = c(0,50)) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5),
        plot.title = element_text(hjust=0.5, size=18),
        axis.title.y = element_markdown(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) 


#### production ratio
#electricity
p.ratio.p <- ggplot(energy.countries.ts 
                    # %>% filter(twh.mean.p!=0)
                    ) +
  geom_line(aes(x=datetime, y=ratio.electricity.mean.p, color=Country), linewidth=0.75) +
  geom_ribbon(aes(x=datetime, 
                  ymin=ratio.electricity.low.p, 
                  ymax=ratio.electricity.high.p, 
                  color=Country), alpha=0.05, linetype ='dashed')  +
  ggtitle('\nC. National energetic contribution (%)') +
  scale_color_manual(values = my.palette) +
  xlab('Year')+
  ylab('Ratio (%)') +
  ylim(c(0,45)) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5),
        plot.title = element_text(hjust=0.5, size=18)) 

#### consumption ratio
#electricity
p.ratio.c <- ggplot(energy.countries.ts 
                    # %>% filter(twh.mean.c!=0)
) +
  geom_line(aes(x=datetime, y=ratio.electricity.mean.c, color=Country), linewidth=0.75) +
  geom_ribbon(aes(x=datetime, 
                  ymin=ratio.electricity.low.c, 
                  ymax=ratio.electricity.high.c, 
                  color=Country), alpha=0.05, linetype ='dashed') +
  ggtitle('\nD. National energetic toll (%)') +
  scale_color_manual(values = my.palette) +
  xlab('Year')+
  ylab('Ratio (%)') +
  scale_y_continuous(position = "right", limits = c(0,45)) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5),
        plot.title = element_text(hjust=0.5, size=18)) 


#### patchwork ####
patch.validation <- (p.p + p.c) / (p.ratio.p + p.ratio.c) + 
  # plot_annotation(title = "Relationships of IBWT megaprojects with countries' energy supplies\n",
  #                 theme = theme(plot.title = element_text(hjust = 0.5, size = 22))) +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom',
        legend.text = element_markdown(size=16),
        legend.title = element_text(size=18),
        legend.spacing = unit(2, 'cm'),
        axis.title = element_markdown(size=18),
        axis.text = element_markdown(size=16),
        plot.title = element_text(size=20),
        legend.key.spacing.x = unit(1, 'cm'),
        legend.key.spacing.y = unit(0.5, 'cm'),
        legend.spacing.y = unit(1, 'cm')
        )


ggsave(paste0(outputDirViz,'energy_countries_timeseries.png'), patch.validation,
       height=12, width=12, units='in', dpi=300, bg='white')
file.show(paste0(outputDirViz,'energy_countries_timeseries.png'))