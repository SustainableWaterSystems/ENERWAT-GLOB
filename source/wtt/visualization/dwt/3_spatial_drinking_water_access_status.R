library(vroom)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(patchwork)

palette.access <- c('#648FFF', '#785EF0', '#FFB000','#FE6100')

#### paths ####
inputDirWorld <- '../../../../input/global_data/'
inputDirPop <- '../../../../output/water_treatment/model/dwt/0_population/'
inputDirVol <- '../../../../output/water_treatment/model/dwt/1_volumes/1_allocation_desalination_SW_GW/'

outputDir <- '../../../../output/water_treatment/visualization/dwt/'

#### processing ####
wg <- map_data("world")

countries.5arcmin <- vroom(
  paste0(inputDirWorld, 'countries_raster_5arcmin.csv'), show_col_types = F)

AQUASTAT.map <- vroom(paste0(
  inputDirPop, '3_output_clean/population_treatment_5arcmin_BASIC.csv'
))

population.type.5arcmin <- vroom(paste0(inputDirPop, '1_input/pop_gdp_input_df.csv'))
cellid.urban <- population.type.5arcmin$cell_ID[
  population.type.5arcmin$pop_type_cell == 'U' | population.type.5arcmin$pop_type_cell == 'UR' ]
cellid.rural <- population.type.5arcmin$cell_ID[
  population.type.5arcmin$pop_type_cell == 'R' | population.type.5arcmin$pop_type_cell == 'RU' ]

desalination <- vroom(
  paste0(inputDirVol, 'demands_domestic_allocated_to_desalination.csv')
) 

desalination.rural <- desalination %>% 
  filter(cell_ID %in% cellid.rural)

desalination.urban <- desalination %>% 
  filter(cell_ID %in% cellid.urban)

treated.rural <- vroom(
  paste0(inputDirVol, 'demands_domestic_allocated_to_SW_and_GW_treated_rural.csv')
)

treated.urban <- vroom(
  paste0(inputDirVol, 'demands_domestic_allocated_to_SW_and_GW_treated_urban.csv')
)

untreated.rural <- vroom(
  paste0(inputDirVol, 'demands_domestic_allocated_to_SW_and_GW_untreated_rural.csv')
)

untreated.urban <- vroom(
  paste0(inputDirVol, 'demands_domestic_allocated_to_SW_and_GW_untreated_urban.csv')
)

#### process ####
AQUASTAT.basic.ID <- AQUASTAT.map %>% 
  filter(treatment == 1) %>% 
  select(cell_ID)

basic.rural <- untreated.rural %>% 
  filter(cell_ID %in% AQUASTAT.basic.ID$cell_ID)
  
basic.urban <- untreated.urban %>% 
  filter(cell_ID %in% AQUASTAT.basic.ID$cell_ID)

actually.untreated.rural <- untreated.rural %>% 
  filter(!cell_ID %in% AQUASTAT.basic.ID$cell_ID)

actually.untreated.urban <- untreated.urban %>% 
  filter(!cell_ID %in% AQUASTAT.basic.ID$cell_ID)


#### assign integer and merge all ####
map.cwdt <- rbind(treated.rural, treated.urban) %>% 
  mutate(boolean.id = 1) %>% 
  select(cell_ID, boolean.id)

map.desal <- desalination %>% 
  mutate(boolean.id = 2) %>% 
  select(cell_ID, boolean.id)

map.basic <- rbind(basic.rural, basic.urban) %>% 
  mutate(boolean.id = 3) %>% 
  select(cell_ID, boolean.id)

map.untreated <- rbind(actually.untreated.rural, actually.untreated.urban) %>% 
  mutate(boolean.id = 4) %>% 
  select(cell_ID, boolean.id)

#### merge all
map.all.treatments <- rbind(
  map.cwdt, map.desal, map.basic, map.untreated
)

map.all.treatment.rural <- map.all.treatments %>% 
  filter(cell_ID %in% cellid.rural) %>% 
  inner_join(countries.5arcmin %>% 
               select(cell_ID, cell_lon, cell_lat))

map.all.treatment.urban <- map.all.treatments %>% 
  filter(cell_ID %in% cellid.urban)  %>% 
  inner_join(countries.5arcmin %>% 
               select(cell_ID, cell_lon, cell_lat))

access.rural.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = map.all.treatment.rural, aes(x = cell_lon, y = cell_lat, 
                                     color = factor(boolean.id), fill=factor(boolean.id)),
             shape = 22, 
             size = 0.2, stroke=0) +
  scale_fill_manual(labels = c("Conventional drinking water treatment", 
                               "Desalinated drinking water",
                               "Basic water access",
                               "Untreated drinking water"),
                    values = palette.access ,
                    na.value = 'transparent',
                    name = 'Status') +
  scale_color_manual(labels = c("Conventional drinking water treatment", 
                                "Desalinated drinking water",
                                "Basic water access",
                                "Untreated drinking water"),
                     values = palette.access,
                     na.value = 'transparent',
                     name = 'Status',
                     guide = 'none') + 
  theme_map() +
  guides(color = guide_legend(override.aes = list(size = 4)),
         fill = guide_legend(override.aes = list(size = 4))) +
  ggtitle('Rural') +
  ylim(-55,80)+
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.text = element_text(size=16),
        legend.spacing= unit(4, 'cm'),
        
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 22))

access.urban.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = map.all.treatment.urban, aes(x = cell_lon, y = cell_lat, 
                                                 color = factor(boolean.id), fill=factor(boolean.id)),
             shape = 22, 
             size = 0.5, stroke=0) +
  scale_fill_manual(labels = c("Conventional drinking water treatment", 
                               "Desalinated drinking water",
                               "Basic water access",
                               "Untreated drinking water"),
                    values = palette.access ,
                    na.value = 'transparent',
                    name = 'Status') +
  scale_color_manual(labels = c("Conventional drinking water treatment", 
                                "Desalinated drinking water",
                                "Basic water access",
                                "Untreated drinking water"),
                     values = palette.access,
                     na.value = 'transparent',
                     name = 'Status',
                     guide = 'none') + 
  theme_map() +
  guides(color = guide_legend(override.aes = list(size = 4)),
         fill = guide_legend(override.aes = list(size = 4))) +
  ggtitle('Urban') +
  ylim(-55,80)+
  theme(legend.title = element_markdown(hjust=0.5, size=18),
        legend.position="bottom",
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.text = element_text(size=16),
        legend.spacing= unit(4, 'cm'),
        
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 22))

p.patch <- (access.rural.plot / access.urban.plot) + 
  plot_layout(guides = 'collect') +
  plot_annotation(title = '5 arcmin modelled drinking water access status (2015)\n',
                  theme = theme(plot.title = element_text(hjust = 0.5 , size = 24))) &
  theme(legend.position = 'bottom')



ggsave(paste0(outputDir, 'water_access_maps.png'), p.patch,
       height=13, width=12, units='in', dpi=300, bg='white')

file.show(paste0(outputDir, 'water_access_maps.png'))

