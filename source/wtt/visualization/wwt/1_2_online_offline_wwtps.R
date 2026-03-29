library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(RColorBrewer)

#paths
inputDirRaw <- '../../../../input/wtt/wwt/'
inputDirFlows <- '../../../../output/wtt/model/wwt/0_volumes/3_flows_model_output/'
outputDir <- '../../../../output/wtt/visualization/wwt/'

wg <- map_data("world")

wwtp.off <- read.csv(paste0(inputDirFlows, 'wwtp_modelled_offline.csv')) %>% 
  mutate(STATUS.MODEL = 'Offline') %>% 
  select(WASTE_ID, LAT_WWTP, LON_WWTP, STATUS.MODEL)
wwtp.on <- read.csv(paste0(inputDirFlows, 'wwtp_modelled_flows.csv')) %>% 
  mutate(STATUS.MODEL = 'Operational') %>% 
  select(WASTE_ID, LAT_WWTP, LON_WWTP, STATUS.MODEL)
hydrowaste.g <- read.csv(paste0(inputDirRaw, 'HydroWASTE_2022.csv'))
hydrowaste.w.china <- read.csv(paste0(inputDirRaw, 'HydroWASTE_updated_China.csv'))

#### processing ####
hydrowaste.status <- inner_join(hydrowaste.w.china %>% 
                                  select(WASTE_ID, LAT_WWTP, LON_WWTP), hydrowaste.g %>% 
                                  select(WASTE_ID, STATUS)) %>% 
  rename(STATUS.DB = STATUS)

hydrowaste.china <- hydrowaste.w.china %>% 
  filter(!WASTE_ID %in% hydrowaste.status$WASTE_ID) %>% 
  mutate(STATUS.DB = 'Operational') %>% 
  select(WASTE_ID, LAT_WWTP, LON_WWTP, STATUS.DB)


og.data.status <- rbind(hydrowaste.status, hydrowaste.china)
model.data.status <- rbind(wwtp.off, wwtp.on)

data.status.plot <- inner_join(og.data.status, model.data.status) %>% 
  mutate(plot.status = case_when(STATUS.DB == 'Not Reported' & 
                                   STATUS.MODEL == 'Offline' ~ 'Not reported / Modelled offline', 
                                 STATUS.DB == 'Not Reported' & 
                                   STATUS.MODEL == 'Operational' ~ 'Not reported / Modelled operational', 
                                 STATUS.DB == 'Operational' & 
                                   STATUS.MODEL == 'Offline' ~ 'Reported operational / Modelled offline', 
                                 STATUS.DB == 'Operational' & 
                                   STATUS.MODEL == 'Operational' ~ 'Reported operational / Modelled operational'))

nrow(data.status.plot %>% filter(STATUS.MODEL == 'Operational'))
nrow(data.status.plot %>% filter(STATUS.MODEL == 'Offline'))

pal.sample <- brewer.pal(8, 'Accent')
pal.plot <- c(pal.sample[1:3], pal.sample[5])


wwtp.status.plot <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = data.status.plot, aes(x = LON_WWTP, y = LAT_WWTP, 
                                     fill = plot.status),
             size = 0.7, shape = 21, stroke = 0) +
  scale_fill_manual(values = pal.plot) +
  theme_map() +
  ggtitle('Modelled status of wastewater treatment plants') +
  ylim(-55,80)+
  guides() +
  theme(legend.title = element_markdown(hjust=0.5, size=24),
        legend.position="bottom",
        legend.direction = 'horizontal',
        legend.justification = 'center',
        legend.spacing= unit(4, 'cm'),
        
        legend.text = element_text(size=20),
        legend.key.width = unit(1.7, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  guides(fill=guide_legend(title="Status", ncol=2, override.aes = list(size = 3), title.position="top"))

ggsave(paste0(outputDir, 'wwtp_status.png'), wwtp.status.plot,
       height=9, width=12, units='in', dpi=300, bg='white')

file.show(paste0(outputDir, 'wwtp_status.png'))

