rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

palette.purposes <- c("#7FC97F", "#AFF80A","#BEAED4", "#FDC086",
                       "#386CB0", "#F0027F", '#332288', '#117733', 
                       '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')

palette.purposes <- c('#88CCEE', "#FDC086", '#44AA99','#CC6677')

#### load ####
inputDir <-
  '../../../output/ibwt/model/1_discharge_timeseries/1_bias_corrected/'

outputDir <- '../../../output/ibwt/visualization/'
dir.create(outputDir, recursive = T, showWarnings = F)

#### ####
ibwt.info <- read.csv(paste0(inputDir, '1_0_bias_correction_sections.csv'))
ibwt.segments <- read.csv(paste0(inputDir, '1_1_bias_correction_segments.csv'))

wg <- map_data("world")

ibwt.names <- unique(ibwt.info$transfer.name)

####
source('fun_0_make_ibwt_locations_plot_data.R')

#### plot ####
###Inter-basin transfer locations
p.map <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = ibwt.plot.df, 
             aes(
               # x = ibwt.from.lon, y = ibwt.from.lat, shape = purpose.main,
               x = lon.center, y = lat.center, shape = purpose.main,
               fill = transfer.length, size = n.reservoirs),
             alpha = 0.7, stroke = 1)+
  ylim(-55,70) +
  ggtitle('A. Inter-basin water transfer megaprojects') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.0, 'cm'),
        legend.title = element_text(hjust=0.5, size=14),
        legend.title.position = 'top',
        legend.text = element_text(size=13),
        plot.title = element_text(hjust = 0.5, size=14, face='plain')) +
  scale_shape_manual(values =c(21,22,23,24),
                     name = 'Main purpose') +
  scale_fill_gradient(low = 'darkolivegreen4', high = 'gold1',
                      limits = c(0,1500)) +
  scale_size(name="Number of reservoirs",
             breaks=c(1,2,3,5,6),
             range=c(2,6)) +
  guides(color=guide_legend(title="Main purpose"),
         fill=guide_colorbar(title="Project length (km)", direction = 'horizontal',
                            label.position = 'bottom',
                            theme = theme(
                              legend.key.width  = unit(8, "lines")), order = 1),
         size=guide_legend(title='Number of reservoirs', direction = 'horizontal',
                           label.position = 'bottom', order = 2)
         ) 
#### cumulative length over time

p.length <- ggplot() + 
  geom_line(ibwt.cumulative.stats, mapping = aes(x=initial.year, y= cumulative.length)) +
  geom_col(ibwt.cumulative.stats, mapping = aes(x=initial.year, y = length.year)) +
  ggtitle('B.') +
  xlab('Year') +
  ylab('km') +
  theme_light() 

p.reservoirs <- ggplot() + 
  geom_line(ibwt.cumulative.stats, mapping = aes(x=initial.year, y= cumulative.reservoirs)) +
  geom_col(ibwt.cumulative.stats, mapping = aes(x=initial.year, y = reservoirs.year)) +
  ggtitle('C.') +
  xlab('Year') +
  ylab('(-)') +
  theme_light() 


p.purposes <- ggplot() + 
  geom_point(pdf, mapping = aes(x=initial.year, y= cs, shape = group), size=2) +
  geom_line(pdf, mapping = aes(x=initial.year, y= cs, shape= group)) +
  scale_color_manual(values=palette.purposes) +
  scale_shape_manual(values =c(21,22,23,24)) +
  ggtitle('D.') +
  xlab('Year') +
  ylab('(-)') +
  theme_light() +
  theme(legend.position = 'none') 

p.patch <- p.map / (p.length + p.reservoirs + p.purposes) + 
  plot_layout(heights = c(3, 1))


ggsave(paste0(outputDir,'ibwt_locations_purposes.png'), p.patch,
       height=7, width=7, units='in', dpi=300, bg='white')

file.show(paste0(outputDir,'ibwt_locations_purposes.png'))
