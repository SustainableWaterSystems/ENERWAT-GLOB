library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(patchwork)
library(ggtext)


outputDir <- '../../../../output/wtt/visualization/dwt/'

wg <- map_data("world")

gdp.5arcmin <- vroom(
  '../../../../output/wtt/model/dwt/0_population/1_input/pop_gdp_input_df.csv')

pop.rural.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = gdp.5arcmin %>% 
               filter(pop_type_cell == 'R' | pop_type_cell == 'RU' ), 
             aes(x = lon, y = lat, color = pop_2015_total, fill=pop_2015_total),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(na.value = 'transparent',
                     name = 'Population (Rural)', 
                     trans = 'log10') +
  scale_color_viridis(na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('A. 5 arcmin rural population (2015)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=24),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.text = element_text(size=20),
        legend.spacing= unit(4, 'cm'),
        
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))

pop.urban.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = gdp.5arcmin %>% 
               filter(pop_type_cell == 'U' | pop_type_cell == 'UR' ), 
             aes(x = lon, y = lat, color = pop_2015_total, fill=pop_2015_total),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(na.value = 'transparent',
                     name = 'Population (Urban)', 
                     trans = 'log10') +
  scale_color_viridis(na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('B. 5 arcmin urban population (2015)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=24),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.text = element_text(size=20),
        legend.spacing= unit(4, 'cm'),
        
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24))


gdp.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = gdp.5arcmin, aes(x = lon, y = lat, 
                                      color = GDP_total_2015, fill=GDP_total_2015),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(option = 'F', na.value = 'transparent',
                     name = 'GDP total (million USD)', 
                     trans = 'log10') +
  scale_color_viridis(option = 'F', na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('<br />C. 5 arcmin GDP-PPP (2015)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=24),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
         legend.text = element_text(size=20),
        legend.spacing= unit(4, 'cm'),
        
       legend.key.width = unit(1.7, "cm"),
        plot.title = element_markdown(hjust = 0.5, size = 24))


gdp.capita.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = gdp.5arcmin, aes(x = lon, y = lat, 
                                     color = GDP_capita_2015, fill=GDP_capita_2015),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_viridis(option = 'F', na.value = 'transparent',
                     name = 'GDP per capita (million USD)', 
                     trans = 'log10') +
  scale_color_viridis(option = 'F', na.value = 'transparent',
                      guide = 'none', trans = 'log10') +
  theme_map() +
  ggtitle('<br />D. 5 arcmin GDP-PPP per capita (2015)') +
  ylim(-55,80)+
  guides(fill = guide_colourbar(title.position="top")) +
  theme(legend.title = element_markdown(hjust=0.5, size=24),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        
        legend.text = element_text(size=20),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_markdown(hjust = 0.5, size = 24))


#### patchwork ####
pp.patch <- ((pop.rural.plot + pop.urban.plot) / (gdp.plot + gdp.capita.plot))

ggsave(paste0(outputDir, 'gdp_pop_maps_drinking.png'), pp.patch,
       height=15, width=20, units='in', dpi=300, bg='white')

file.show(paste0(outputDir, 'gdp_pop_maps_drinking.png'))

