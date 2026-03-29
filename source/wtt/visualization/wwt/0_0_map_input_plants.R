library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)


#paths
inputDirWorld <- '../../../../input/global_data/'
inputDir <- '../../../../input/wtt/wwt/'
outputDir <- '../../../../output/wtt/visualization/wwt/'
dir.create(outputDir, showWarnings = F, recursive = T)

wg <- map_data("world")

hydrowaste <- read.csv(paste0(inputDir, 'HydroWASTE_updated_China.csv'))

ww.volumes.df <- vroom(paste0(inputDir,'WWT_Jones2021_5arcmin.csv')) 
ww.volumes.df.for.plot <- ww.volumes.df %>% 
  filter(wwtv != 0)

population.2015 <- vroom(paste0(inputDirWorld, 'general/population_5arcmin_2015.csv'))

population.2015.for.plot <- population.2015[
  
  population.2015$cell_ID %in% ww.volumes.df.for.plot$cell_ID,
  
]


#### plot ####
#assign discrete strings
hydrowaste$POP_PLOT_STRING <- NA

hydrowaste$POP_PLOT_STRING[hydrowaste$POP_SERVED <= 50000] <- '< 50,000'
hydrowaste$POP_PLOT_STRING[
  hydrowaste$POP_SERVED > 50000 & hydrowaste$POP_SERVED <= 100000] <- '50,000 - 100,000'
hydrowaste$POP_PLOT_STRING[
  hydrowaste$POP_SERVED > 100000 & hydrowaste$POP_SERVED <= 500000] <- '100,000 - 500,000'
hydrowaste$POP_PLOT_STRING[hydrowaste$POP_SERVED >500000] <- '> 500,000'


#### prepare plot data ####
# small plants
hydrowaste.small.data <- hydrowaste  %>%
  filter(POP_PLOT_STRING == '< 50,000') 

hydrowaste.small.data$POP_PLOT_STRING <- NA

hydrowaste.small.data$POP_PLOT_STRING[hydrowaste.small.data$POP_SERVED <= 1000] <- '< 1,000'
hydrowaste.small.data$POP_PLOT_STRING[
  hydrowaste.small.data$POP_SERVED > 1000 & hydrowaste.small.data$POP_SERVED <= 10000] <- '1,000 - 10,000'
hydrowaste.small.data$POP_PLOT_STRING[
  hydrowaste.small.data$POP_SERVED > 10000 & hydrowaste.small.data$POP_SERVED <= 25000] <- '10,000 - 25,000'
hydrowaste.small.data$POP_PLOT_STRING[
  hydrowaste.small.data$POP_SERVED > 25000 & hydrowaste.small.data$POP_SERVED <= 50000] <- '25,000 - 50,000'

#large plants
hydrowaste.large.data <- hydrowaste  %>%
  filter(POP_PLOT_STRING != '< 50,000') 

hydrowaste.large.data$POP_PLOT_STRING <- NA
hydrowaste.large.data$POP_PLOT_STRING[
  hydrowaste.large.data$POP_SERVED > 50000 & hydrowaste.large.data$POP_SERVED <= 100000] <- 
  '50,000 - 100,000'
hydrowaste.large.data$POP_PLOT_STRING[
  hydrowaste.large.data$POP_SERVED > 100000 & hydrowaste.large.data$POP_SERVED <= 250000] <- 
  '100,000 - 250,000'
hydrowaste.large.data$POP_PLOT_STRING[
  hydrowaste.large.data$POP_SERVED > 250000 & hydrowaste.large.data$POP_SERVED <= 500000] <- 
  '250,000 - 500,000'
hydrowaste.large.data$POP_PLOT_STRING[hydrowaste.large.data$POP_SERVED > 500000] <- '> 500,000'


#palette for hydrowaste
colors <- c("#7FC97F", "#AFF80A","#BEAED4", "#FDC086", 
            "#386CB0", "#F0027F", '#FE6100', '#FFB000', '#648FFF', '#785EF0', '#332288', '#117733', '#44AA99',
            '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')

hw.palette.small <- c("#386CB0", "#F0027F", '#FE6100', '#FFB000', #small plants
                      '#88CCEE', '#DDCC77', '#CC6677', '#AA4499') #large plants (for legend)
hw.palette.large <- c('#88CCEE', '#DDCC77', '#CC6677', '#AA4499')
hydrowaste.small.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = hydrowaste.small.data %>% 
               mutate(POP_PLOT_STRING = factor(POP_PLOT_STRING, 
                                               levels=c( '< 1,000',
                                                         '1,000 - 10,000',
                                                         '10,000 - 25,000',
                                                         '25,000 - 50,000',
                                                         '50,000 - 100,000',
                                                         '100,000 - 250,000',
                                                         '250,000 - 500,000',
                                                         '> 500,000'
                                               ))) %>% 
               mutate(LEVEL = factor(LEVEL, levels = c('Primary',
                                                       'Secondary',
                                                       'Advanced'))),
             aes(x = LON_WWTP, y = LAT_WWTP,
                 fill = POP_PLOT_STRING, size = WASTE_DIS, shape = LEVEL),
             alpha = 0.6, 
             # shape = 21, 
             stroke = 0.2) +
  theme_map() +
  ggtitle('A. Small wastewater treatment plants') +
  ylim(-55,80) +
  scale_fill_manual(values= hw.palette.small, drop= F, name = 'Population served') +
  scale_shape_manual(values =c("Primary"=21,"Secondary"=22, "Advanced"=24),
                     name = 'Treatment level') +
  scale_size(name="Wastewater discharge",
             breaks=c(1000,10000,100000,500000, Inf),
             range=c(1,3),
             guide = 'none') +
  guides(fill = guide_legend(ncol = 2, override.aes = list(size=5, 
                                                           alpha = 1,
                                                           shape = 21)),
         shape = guide_legend(override.aes = list(size = 4))) +
  theme(legend.title = element_text(hjust=0.5, size=18))

hydrowaste.large.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = hydrowaste.large.data %>% 
               mutate(POP_PLOT_STRING = factor(POP_PLOT_STRING, 
                                         levels=c( '50,000 - 100,000',
                                                   '100,000 - 250,000',
                                                   '250,000 - 500,000',
                                                   '> 500,000'
                                                   )))  %>% 
               mutate(LEVEL = factor(LEVEL, levels = c('Primary',
                                                       'Secondary',
                                                       'Advanced'))),
             aes(x = LON_WWTP, y = LAT_WWTP,
                 fill = POP_PLOT_STRING, size = WASTE_DIS, shape = LEVEL),
             alpha = 0.6, 
             # shape = 21, 
             stroke = 0.3) +
  
  theme_map() +
  ggtitle('B. Large wastewater treatment plants') +
  ylim(-55,80) +  
  scale_fill_manual(values= hw.palette.large, guide = 'none') +
  scale_shape_manual(values =c("Primary"=21,"Secondary"=22, "Advanced"=24),
                     name = 'Treatment level', guide ='none') +
  scale_size(name="Wastewater discharge (m<sup>3</sup> d<sup>-1</sup>)",
             breaks=c(1000,10000,100000,500000, Inf),
             labels=c('0 - 1,000','1,000 - 10,000','10,000 - 100,000',
                      '100,000 - 500,000', '> 500,000'),
             range=c(1,8)) +
  theme(legend.title = element_markdown(hjust=0.5, size=18))

#### patchwork ####
pp.with.volumes <- (hydrowaste.small.plot / hydrowaste.large.plot) + #/ pop.plot) +
  plot_layout(guides = 'collect') &
  theme(legend.position="bottom",
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1, 'cm'),
        legend.text = element_text(size=14),
        plot.title = element_text(size = 20, hjust = 0.5))
  
# # 
ggsave(paste0(outputDir,'wwt_plants.png'), pp.with.volumes,
       height=13, width=12, units='in', dpi=300, bg='white')
#
file.show(paste0(outputDir,'wwt_plants.png'))

