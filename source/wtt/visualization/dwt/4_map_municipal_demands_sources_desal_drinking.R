library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(patchwork)
library(ggtext)
library(cowplot)
library(RColorBrewer)

inputDirRaw <- '../../../../input/wtt/dwt/'
inputDirDesal <- '../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/'

outputDir <- '../../../../output/wtt/visualization/dwt/'

wg <- map_data("world")

demands.2015 <- vroom(paste0(inputDirRaw, 'domestic_demands_2015_tot_m3.csv'))

sources.pcrglob <- vroom(paste0(inputDirRaw,
                                'aqueduct_withdrawal_sources_5arcmin_2015.csv')) %>% 
  inner_join(demands.2015 %>% 
               dplyr::select(cell_ID, lon,lat))

desalination.drinking <- vroom(paste0(inputDirDesal,
                                      'desaldata_drinking_5arcmin_2015.csv'))

#### processing ####
palette.rgw <- brewer.pal(8, "YlGn")
palette.fgw <- brewer.pal(8, "BuPu")

#### plots ####
municipal.demands.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = demands.2015 %>% 
               filter(demands.m3.2015 != 0), aes(x = lon, y = lat, 
                                                 color = demands.m3.2015, fill=demands.m3.2015),
             shape = 22, 
             size = 1, stroke=0) +
  scale_fill_viridis(option = 'G', na.value = 'transparent',
                     name = 'Demands (m<sup>3</sup> y<sup>-1</sup>)'
                     ,
                     trans = 'log10'
  ) +
  scale_color_viridis(option = 'G', na.value = 'transparent',
                      guide = 'none'
                      , trans = 'log10'
  ) +
  theme_map() +
  ggtitle('A. Municipal water demands (5arcmin, 2015)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

desal.drinking.plot <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = desalination.drinking %>% 
               filter(drinking.desalination.m3.y != 0), aes(x = lon, y = lat, 
                                          color = drinking.desalination.m3.y, fill=drinking.desalination.m3.y),
             shape = 22, 
             size = 0.8, stroke=0) +
  scale_fill_viridis_c(option = 'E', na.value = 'transparent',
                     name = 'Capacity (m<sup>3</sup> y<sup>-1</sup>)', 
                     trans = 'log10', direction = -1, limits = c(100, 1000000000)) +
  scale_color_viridis_c(option = 'E', na.value = 'transparent',
                      guide = 'none', trans = 'log10', direction = -1) +
  theme_map() +
  ggtitle('B. Desalination (drinking)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

sw.plot <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = sources.pcrglob %>% 
               filter(SW.ww.m3 != 0), aes(x = lon, y = lat, 
                                                 color = SW.ww.m3, fill=SW.ww.m3),
             shape = 22, 
             size = 0.3, stroke=0) +
  scale_fill_viridis(option = 'B', na.value = 'transparent',
                     name = 'SW abstraction (m<sup>3</sup> y<sup>-1</sup>)'
                     ,
                     trans = 'log10'
  ) +
  scale_color_viridis(option = 'B', na.value = 'transparent',
                      guide = 'none'
                      , trans = 'log10' 
  ) +
  theme_map() +
  ggtitle('C. Surface water') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

rgw.plot <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = sources.pcrglob %>% 
               filter(nonFossilGW.ww.m3 != 0), aes(x = lon, y = lat, 
                                          color = nonFossilGW.ww.m3, fill=nonFossilGW.ww.m3),
             shape = 22, 
             size = 0.3, stroke=0) +
  scale_fill_gradient(low = palette.rgw[1], high = palette.rgw[8], na.value = 'transparent',
                      name = 'RGW abstraction (m<sup>3</sup> y<sup>-1</sup>)'
                      ,
                      trans = 'log10'
  ) +
  scale_color_gradient(low = palette.rgw[1], high = palette.rgw[8], na.value = 'transparent',
                       guide = 'none'
                       , trans = 'log10'
  ) +
  theme_map() +
  ggtitle('D. Renewable groundwater') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))


fgw.plot <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = sources.pcrglob %>% 
               filter(fossilGW.ww.m3 != 0), aes(x = lon, y = lat, 
                                          color = fossilGW.ww.m3, fill=fossilGW.ww.m3),
             shape = 22, 
             size = 0.3, stroke=0) +
  scale_fill_gradient(low = palette.fgw[1], high = palette.fgw[8], na.value = 'transparent',
                     name = 'FGW abstraction (m<sup>3</sup> y<sup>-1</sup>)'
                     ,
                     trans = 'log10'
  ) +
  scale_color_gradient(low = palette.fgw[1], high = palette.fgw[8], na.value = 'transparent',
                      guide = 'none'
                      , trans = 'log10'
  ) +
  theme_map() +
  ggtitle('E. Fossil groundwater') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))


sources.grid <- ((desal.drinking.plot + sw.plot) / (rgw.plot + fgw.plot)) + 
  plot_annotation(title = 'Water sources (5arcmin, 2015)\n',
                  theme = theme(plot.title = element_markdown(hjust = 0.5 , size = 24))) &
  theme(legend.position = 'bottom',
        legend.spacing= unit(4, 'cm'))


final_p <- plot_grid(municipal.demands.plot,
                     NULL,
                     sources.grid,
                     nrow = 3,
                     rel_heights = c(1, 0.03, 1),
                     align = "v",
                     axis = "t"
)

ggsave(paste0(outputDir, 'drinking_demands_sources.png'), final_p,
       height=16, width=12, units='in', dpi=300, bg='white')

file.show(paste0(outputDir, 'drinking_demands_sources.png'))