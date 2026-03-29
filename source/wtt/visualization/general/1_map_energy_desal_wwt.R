library(dplyr)
library(ggplot2)
library(vroom)
library(ggthemes)
library(patchwork)
library(ggtext)
library(grid)
library(gridExtra)
library(cowplot)


plot.title.size = 26
plot.title.legend.size = 22

#### inputs ####
inputDirWorld <- '../../../../input/global_data/'
inputDirData <- '../../../../output/wtt/model/'
outputDir <- '../../../../output/wtt/visualization/general/'

wg <- map_data("world")

countries.raster <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv')) %>% 
  select(cell_ID, cell_lon, cell_lat) 

energy.plants.desal <- read.csv(
  paste0(inputDirData, 'desalination/2_energy/energy_plants_2015_desal.csv'))

energy.plants.wwt <- read.csv(
  paste0(inputDirData,'wwt/1_energy/energy_plants_2015_wwt.csv')) 

my.palette <- c('#1E81B0','#117733', '#E3A43E', '#E56529', '#AC3228',  '#441B1B')


#### aggregate to 5 arcmin ####

energy.5arcmin.desal <- energy.plants.desal  %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(gwh.y.mean = sum(kwh.y.mean) / 10^6) %>% 
  inner_join(countries.raster %>% 
               select(cell_ID, cell_lon, cell_lat) %>% 
               rename(pcrglobwb_cellID = cell_ID)) %>% 
  arrange(gwh.y.mean)  %>% 
  mutate(energy.cat = case_when(
    gwh.y.mean <  1 ~ "< 1", 
    gwh.y.mean >=  1 & gwh.y.mean < 10 ~ "1 - 10", 
    gwh.y.mean >=  10 & gwh.y.mean < 100 ~ "10 - 100", 
    gwh.y.mean >=  100 & gwh.y.mean < 1000 ~ "100 - 1 000",  
    gwh.y.mean >=  1000 & gwh.y.mean < 10000 ~ "1 000 - 10 000", 
    gwh.y.mean >=  10000 ~ "> 10 000")) %>%
  mutate(energy.cat = factor(
    energy.cat, levels = c("< 1","1 - 10", "10 - 100", "100 - 1 000",
                           "1 000 - 10 000", "> 10 000")))

energy.5arcmin.wwt <- energy.plants.wwt %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(gwh.y.mean = sum(kwh.y.mean) / 10^6) %>% 
  inner_join(countries.raster %>% 
               select(cell_ID, cell_lon, cell_lat) %>% 
               rename(pcrglobwb_cellID = cell_ID)) %>% 
  arrange(gwh.y.mean) %>% 
  mutate(energy.cat = case_when(
    gwh.y.mean <  1 ~ "< 1", 
    gwh.y.mean >=  1 & gwh.y.mean < 10 ~ "1 - 10", 
    gwh.y.mean >=  10 & gwh.y.mean < 100 ~ "10 - 100", 
    gwh.y.mean >=  100 & gwh.y.mean < 1000 ~ "100 - 1 000",  
    gwh.y.mean >=  1000 & gwh.y.mean < 10000 ~ "1 000 - 10 000", 
    gwh.y.mean >=  10000 ~ "> 10 000")) %>%
  mutate(energy.cat = factor(
    energy.cat, levels = c("< 1","1 - 10", "10 - 100", "100 - 1 000",
                           "1 000 - 10 000", "> 10 000")))


#### plot ####
bbox.1 <- c(20.113281,14.812718,60.644531,37.020098) #middle east north africa
bbox.2 <- c(100.554688,18.812718,145.371094,41.244772) #china/japan 2
bbox.3 <- c(-11.425781,35,35.507813,59.085739) #europe/na
bbox.4 <- c(-124.980469,24.846565,-80.269531,48.574790) #conus

#### big maps
p.desal <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.desal, aes(x = cell_lon, y = cell_lat, 
                                      fill= energy.cat),
             shape = 22,
             size = 1, stroke=0
            ) +
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[1], y=bbox.1[2], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.1[3], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[2]))+
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[4], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[1], y=bbox.4[2], yend=bbox.4[4]))+
  geom_segment(aes(x=bbox.4[3], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[4]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[2]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[4], yend=bbox.4[4]))+
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ggtitle('Desalination')  +
  ylim(-55,70)+
  xlim(-155,180)+
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5, size = plot.title.size))

p.wwt <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.wwt, aes(x = cell_lon, y = cell_lat, 
                                              fill= energy.cat),
             shape = 22,
             size = 0.5, stroke=0
  ) +
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[1], y=bbox.2[2], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.2[3], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[2]))+
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[4], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[1], y=bbox.3[2], yend=bbox.3[4]))+
  geom_segment(aes(x=bbox.3[3], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[4]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[2]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[4], yend=bbox.3[4]))+
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ggtitle('Wastewater treatment') +
  ylim(-55,70)+
  xlim(-155,180)+
  theme(legend.position="none",
        plot.title = element_markdown(hjust = 0.5, size = plot.title.size))


#### zooms ####

#### MIDDLE EAST
energy.plot.desal.z1 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.desal, aes(x = cell_lon, y = cell_lat,
                                              fill = energy.cat),
             shape = 22,
             size = 1.5, stroke=0
  ) +
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ylim(bbox.1[2], bbox.1[4])+
  xlim(bbox.1[1], bbox.1[3])+
  theme(legend.position="none")

##USA
energy.plot.desal.z2 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.desal, aes(x = cell_lon, y = cell_lat,
                                              fill= energy.cat),
             shape = 22,
             size = 1.5, stroke=0
  ) +
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ylim(bbox.4[2], bbox.4[4])+
  xlim(bbox.4[1], bbox.4[3])+
  theme(legend.position="none")
  
#### wastewater treatment
energy.plot.wwt.z1 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.wwt, aes(x = cell_lon, y = cell_lat, 
                                              fill= energy.cat),
             shape = 22,
             size = 1, stroke=0
  ) +
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ylim(bbox.2[2], bbox.2[4])+
  xlim(bbox.2[1], bbox.2[3])+
  theme(legend.position="none")

energy.plot.wwt.z2 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = energy.5arcmin.wwt, aes(x = cell_lon, y = cell_lat, 
                                              fill= energy.cat),
             shape = 22,
             size = 0.7, stroke=0
  ) +
  scale_fill_manual(values = my.palette)+
  theme_map() +
  ylim(bbox.3[2], bbox.3[4])+
  xlim(bbox.3[1], bbox.3[3])+
  theme(legend.position="none")



######### ARRANGEMENT 2 #################
p.zooms.4.alt <- grid.arrange(
  grobs = list(
    energy.plot.desal.z2, 
    energy.plot.desal.z1,
    energy.plot.wwt.z2, 
    energy.plot.wwt.z1), 
  ncol = 4
  )

p.desal.wwt.alt <- plot_grid(p.desal, p.wwt, nrow = 1)
p.desal.wwt.zooms.alt <- plot_grid(p.desal.wwt.alt, p.zooms.4.alt, nrow = 2, 
                                   rel_heights=c(1,0.5))


plot.legend.alt <- ggplot()+
  geom_point(data = energy.5arcmin.desal, aes(x = cell_lon, y = cell_lat, 
                                              color= gwh.y.mean),
  ) +
  binned_scale(aesthetics = "color",
               name = "<br/> Energy consumption (GWh y<sup>-1</sup>)", 
               palette = function(x) my.palette,
               breaks = c(0, 1, 10, 100, 1000, 10000, 20000),
               limits = c(0, 20000),
               guide = "colorsteps",
               labels = c("0 - 1", "1 - 10", "10 - 100", "100 - 1 000",
                          "1 000 - 10 000", "10 000 - 20 000", "")
  ) +
  theme_map() +
  theme(
    legend.title = element_markdown(hjust=0.5, size=plot.title.legend.size),
    legend.title.position = 'top',
    legend.direction = 'horizontal',
    legend.justification = 'center',
    legend.text = element_text(size=16, hjust=-0.4),
    legend.text.position = 'bottom',
    legend.key.width = unit(7.3, "cm"),
    legend.key.height = unit(0.7, "cm")
  )

legend.alt <- get_legend(plot.legend.alt)

p.desal.wwt.4.alt <- plot_grid(p.desal.wwt.zooms.alt, legend.alt,
                           nrow = 2, rel_heights = c(1,0.3))


ggsave(paste0(outputDir, 'energy_spatial_closer_4_alt_leg_2.png'), p.desal.wwt.4.alt,
       height=10, width=15, units='in', dpi=300, bg='white')

file.show(paste0(outputDir, 'energy_spatial_closer_4_alt_leg_2.png'))

