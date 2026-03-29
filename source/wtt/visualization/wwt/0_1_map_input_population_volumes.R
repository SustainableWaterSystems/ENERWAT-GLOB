library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(ggtext)
library(viridis)

#paths
inputDirWorld <- '../../../../input/global_data/'
inputDir <- '../../../../input/wtt/wwt/'
outputDir <- '../../../../output/wtt/visualization/wwt/'


#inputs for maps
wg <- map_data("world")

ww.volumes.df <- vroom(paste0(inputDir,'WWT_Jones2021_5arcmin.csv')) 
ww.volumes.df.for.plot <- ww.volumes.df %>% 
  filter(wwtv != 0) %>% 
  mutate(wwtv = wwtv / 10^3) #convert to km3

population.2015 <- vroom(paste0(inputDirWorld, 'general/population_5arcmin_2015.csv'))

population.2015.all <- population.2015 %>% 
  filter(pop_2015 != 0)

population.2015.cut <- population.2015[
  
  population.2015$cell_ID %in% ww.volumes.df.for.plot$cell_ID,
  
]


#plot jones2021 - scale B or G viridis
volumes.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = ww.volumes.df.for.plot, aes(x = lon, y = lat, 
                                                color = wwtv, fill=wwtv),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(option = 'G', na.value = 'transparent',
                     name = 'km<sup>3</sup> y<sup>-1</sup>', 
                     trans = 'log10') +
  scale_color_viridis(option = 'G', na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('A. Treated wastewater volumes') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(size = 20, hjust = 0.5))

# plot population 2015
pop.plot.all <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = population.2015.all, aes(x = lon, y = lat, 
                                                color = pop_2015, 
                                                fill=pop_2015),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(option = 'B', na.value = 'transparent',
                     name = 'Population', trans = 'log10') +
  scale_color_viridis(option = 'B', na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ylim(-55,80) 


pop.plot.cut <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = population.2015.cut, aes(x = lon, y = lat, 
                                                color = pop_2015, 
                                                fill=pop_2015),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(option = 'B', na.value = 'transparent',
                     name = 'People', trans = 'log10') +
  scale_color_viridis(option = 'B', na.value = 'transparent', 
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('\nB. Population served') +
  ylim(-55,80) +  
  theme(legend.box="horizontal") +
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_text(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(size = 20, hjust = 0.5))

#### patchwork ####
pp.with.volumes <- (volumes.plot / pop.plot.cut) 
  
  
# # 
ggsave(paste0(outputDir,'pop_vol.png'), pp.with.volumes,
       height=13, width=12, units='in', dpi=300, bg='white')
#
file.show(paste0(outputDir,'pop_vol.png'))

