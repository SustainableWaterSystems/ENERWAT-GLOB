#zoom boxes production
bbox.1 <- c(-126.703123,25.357505,-105.904295,45.030696) #california
bbox.2 <- c(-97.901824,44.081369,-63.840934,61.940941) #canada
bbox.3 <- c(-6.755859,34.281762,3.933594,44.069106) #spain #spain
bbox.4 <- c(143.708344,-40.767267,153.201810,-31.204820) #australia


#### process ####
intensity.summarise.segments.p <- production.yearly %>% 
  group_by(transfer.name, segment.id, reservoir.id, head.m) %>% 
  summarise(energy.twh.mean = mean(energy.twh.mean),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean)) %>% 
  inner_join(intakes.coord.hydropower %>%
               select(reservoir.id, segment.full, intake.lon, intake.lat) %>%
               rename(segment.id = segment.full)) %>% 
  mutate(intake.lat = as.numeric(intake.lat)) %>% 
  distinct(transfer.name, segment.id, .keep_all = TRUE) 
 
p.data.sections <- intensity.summarise.segments.p %>% 
  group_by(reservoir.id) %>% 
  summarise(head.m = sum(head.m),
            energy.twh.mean = sum(energy.twh.mean),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean))  %>% 
  inner_join(sections.coord %>% 
               group_by(reservoir.id) %>% 
               summarise(lon=mean(lon),
                         lat=mean(lat)) %>% 
               select(reservoir.id, lon, lat)) %>% 
  filter(!is.na(energy.m3.kwh.mean))

p.map.hydropower <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = intensity.summarise.segments.p,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.m3.kwh.mean,
               size=energy.twh.mean,
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  ylim(-55,70) +
  ggtitle('A. Energy production (Hydropower plants)') +
  theme_map() +
  scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
                        # breaks = c(1,2,3,4),
                        range=c(2,9)) +
  scale_fill_gradient(low="#9167c8", high= "#df88fa", 
                      name = 'Production intensity (m<sup>3</sup> kwh<sup>-1</sup>)')+
  theme(legend.position = 'bottom',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=16),
        legend.title.position = 'top',
        legend.text = element_text(size=15),
        legend.text.position = 'bottom',
        legend.key.width = unit(0.7, 'cm'),
        plot.title = element_text(hjust = 0.5, size=20, face='plain')) +
  guides(size=guide_legend(order=1, override.aes = list(fill='black')))