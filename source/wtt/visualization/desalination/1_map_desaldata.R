rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggthemes)

palette.feed.only <- c("#7FC97F", "#AFF80A","#BEAED4", 
                       "#FDC086","#386CB0", "#F0027F")

#### inputs ####
inputDir <- '../../../../output/wtt/model/desalination/0_tech_categories/'
outputDir <- '../../../../output/wtt/visualization/desalination/'

desaldata <- read.csv(
  paste0(inputDir, 'DesalDataEnergy_online_2019.csv'))

wg <- map_data("world")

#### prepare for plot ####
desaldata.selected <- desaldata %>% 
  filter(Capacity..m3.d. > 1000) 

desaldata.selected$feed <- NA
desaldata.selected$feed[
  desaldata.selected$Feedwater == 
    'Pure water or tap water (TDS <500ppm)'] <- 
  'Pure water'

desaldata.selected$feed[
  desaldata.selected$Feedwater == 
    'River water or low concentrated saline water (TDS 500ppm - <3000ppm)'] <- 
  'River water'

desaldata.selected$feed[
  desaldata.selected$Feedwater ==
    'Wastewater'] <-
  'Wastewater'

desaldata.selected$feed[
  desaldata.selected$Feedwater == 
    'Brackish water or inland water (TDS 3000ppm - <20000ppm)'] <- 
  'Brackish water'

desaldata.selected$feed[
  desaldata.selected$Feedwater == 
    'Seawater (TDS 20000ppm - 50000ppm)'] <- 
  'Seawater'

desaldata.selected$feed[
  desaldata.selected$Feedwater == 
    'Brine or concentrated seawater (TDS >50000ppm)'] <- 
  'Brine'


#### plot ####
desaldata.map <- 
  
  ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = desaldata.selected %>% 
               mutate(feed = factor(feed, levels=c('Pure water',  'River water',
                                                   'Wastewater',
                                                   'Brackish water',
                                                   'Seawater', 'Brine'))), 
             aes(x = Longitude, y = Latitude, 
                                   fill=feed, shape = tech,
                                   size = Capacity..m3.d.), 
             alpha = 0.9, stroke = 0.3) +
  scale_fill_manual(values = palette.feed.only, name = 'Feedwater') +
  scale_shape_manual(values =c("MSF"=21,"MED"=22, "RO"=24, "ED"=23),
                     name = 'Technology') +
  theme_map() +
  ylim(-55,70) +  
  scale_size(name=expression(paste("Capacity", " (m"^"3 ", "d"^"-1", ")")),
             breaks=c(1000,10000,50000,100000,250000, 500000,880000),
             labels=c('1 000 - 10 000','1 000 - 10 000',
                      '10 000 - 50 000','50 000 - 100 000',
                      '100 000 - 250 000', '250 000 - 500 000',
                      '500 000 - 880 000'),
             range=c(2,9)) +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.0, 'cm'),
        legend.title = element_text(hjust=0.5, size=14),
        legend.text = element_text(size=13)) +
  guides(size = guide_legend(ncol=2), 
         fill = guide_legend(ncol =2, override.aes = list(size = 3, shape=21)),
         shape = guide_legend(override.aes = list(size = 3)))


ggsave(paste0(outputDir,'desaldata_map.png'), desaldata.map,
       height=7, width=11, units='in', dpi=300, bg='white')

file.show(paste0(outputDir,'desaldata_map.png'))
