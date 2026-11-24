#boxes consumption
bbox.1 <- c(-126.703123,25.357505,-105.904295,45.030696) #california
bbox.2 <- c(-6.755859,34.281762,3.933594,44.069106) #spain
bbox.3 <- c(65.341176,21.859173,81.724983,38.421791) #india
bbox.4 <- c(105.792969,25.958393,125.292969,48.574790) #china

#### process ####
intensity.summarise.segments.c <- consumption.yearly %>% 
  group_by(transfer.name, segment.id, reservoir.id, pumping.lift.m) %>% 
  summarise(energy.twh.mean = mean(energy.twh.mean),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm = T)) %>% 
  inner_join(intakes.coord.pumping %>% 
               select(reservoir.id, segment.full, intake.lon, intake.lat) %>% 
               rename(segment.id = segment.full))

p.data.sections <- intensity.summarise.segments.c %>% 
  group_by(reservoir.id) %>% 
  summarise(pumping.lift.m = sum(pumping.lift.m),
            energy.twh.mean = sum(energy.twh.mean),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean))  %>% 
  inner_join(sections.coord %>% 
               group_by(reservoir.id) %>% 
               summarise(lon=mean(lon),
                         lat=mean(lat)) %>% 
               select(reservoir.id, lon, lat)) %>% 
  filter(!is.na(energy.kwh.m3.mean))

p.map.pumping <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = intensity.summarise.segments.c,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.kwh.m3.mean,
               size=energy.twh.mean
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  ylim(-55,70) +
  ggtitle('B. Energy consumption (Pumping stations)') +
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        # breaks = c(0,1,2,3,4,5),
                        range=c(2,9)) +
  scale_fill_gradient(low="#e1af93", high= "#fbdba0", 
                      name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'bottom',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=16),
        legend.title.position = 'top',
        legend.text = element_text(size=15),
        legend.text.position = 'bottom',
        legend.key.width = unit(0.7, 'cm'),
        plot.title = element_text(hjust = 0.5, size=20, face='plain')) +
  guides(size=guide_legend(order=1, override.aes = list(fill='black'), nrow=1))