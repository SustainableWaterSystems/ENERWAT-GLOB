library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

outputDir <- '../../../../output/wtt/visualization/dwt/'

water.access <- read.csv(
  '../../../../output/wtt/model/dwt/0_population/0_preprocessing/countries_safe_water_access_allinfo.csv')

wg <- map_data("world")

#### rename countries in map dataframe
wg.renamed <- wg %>% 
  mutate(Country = recode(
    region, "Antigua" = "Antigua and Barbuda", 
    "Cape Verde" = "Cabo Verde",
    "Democratic Republic of the Congo" = "Congo DRC",                          
    "Republic of Congo" = "Congo Republic",                   
    "Ivory Coast" = "Cote d Ivoire" ,                 
    "Swaziland" = "Eswatini",                         
    "Saint Kitts" = "Saint Kitts and Nevis",           
    "Nevis" = "Saint Kitts and Nevis",      
    "Saint Vincent" = "Saint Vincent and the Grenadines",
    "Grenadines" = "Saint Vincent and the Grenadines", 
    "Trinidad" = "Trinidad and Tobago",  
    "Tobago" = "Trinidad and Tobago",              
    "UK" = "United Kingdom",                  
    "USA" = "United States"))
  
  
#### inner join with statistics to map

#dummy variable to keep original order (otherwise deforms countries)
wg.renamed$id.order  <- 1:nrow(wg.renamed)

wg.statistics <- merge(wg.renamed, water.access, all = T) %>% 
  arrange(id.order)

#### plot ####

map.safe.access <- ggplot()+
  geom_map(
    data = wg.statistics, map = wg.statistics,
    aes(long, lat, map_id = region, fill= safe.access.wb.filled),
    color = "white", alpha=0.6, stroke = 0.5
  ) +
  scale_fill_gradient(low = "red", high = "blue", limits = c(0,100),
                      guide = 'none') +
  theme_map() +
  ylim(-55,80) +
  ggtitle('A. Safe access') +
  theme(plot.title = element_text(hjust = 0.5, size = 22))

map.basic.access <- ggplot()+
  geom_map(
    data = wg.statistics, map = wg.statistics,
    aes(long, lat, map_id = region, fill= access.aquastat.total),
    color = "white", alpha=0.6, stroke = 0.5
  ) +
  scale_fill_gradient(low = "red", high = "blue", limits = c(0,100),
                      guide = 'none') +
  theme_map() +
  ylim(-55,80) +
  ggtitle('B. Basic access') +
  theme(plot.title = element_text(hjust = 0.5, size = 22))

map.rural.access <- ggplot()+
  geom_map(
    data = wg.statistics, map = wg.statistics,
    aes(long, lat, map_id = region, fill = access.aquastat.rural),
    color = "white", alpha=0.6, stroke = 0.5
  ) +
  scale_fill_gradient(low = "red", high = "blue", limits = c(0,100),
                      guide = 'none') +
  theme_map() +
  ylim(-55,80) +
  ggtitle('C. Rural access') +
  theme(plot.title = element_text(hjust = 0.5, size = 22))

map.urban.access <- ggplot()+
  geom_map(
    data = wg.statistics, map = wg.statistics,
    aes(long, lat, map_id = region, fill = access.aquastat.urban),
    color = "white", alpha=0.6  ,stroke = 0.5
  ) +
  scale_fill_gradient(low = "red", high = "blue", limits = c(0,100),
                      name = 'Total population with access (%)') +
  theme_map() +
  ylim(-55,80) +
  ggtitle('D. Urban access') +
  guides(fill = guide_colourbar(title.position="top")) +
  theme(plot.title = element_text(hjust = 0.5, size = 22)) 



#### patch 4 panel ####
pp.patch <- (map.safe.access + map.basic.access) /
  (map.rural.access + map.urban.access) +
  plot_annotation(title = 'Country water access statistics (2015)',
                  theme = theme(
                    plot.title = element_text(hjust = 0.5 , size = 24))) +
  plot_layout(guides = 'collect') &
  theme(legend.title = element_text(hjust=0.5, size=18),
        legend.position = 'bottom',
        legend.text = element_text(size=16),
        legend.key.width = unit(1.7, "cm"))


ggsave(paste0(outputDir, 'country_water_access_maps.png'), pp.patch,
       height=10, width=13, units='in', dpi=300, bg='white')
#
file.show(paste0(outputDir, 'country_water_access_maps.png'))
