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

p.map.consumption.segments <- ggplot() +
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
  ggtitle('C. Pumping stations') +
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[1], y=bbox.1[2], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.1[3], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[2]))+
  geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[4], yend=bbox.1[4]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[1], y=bbox.4[2], yend=bbox.4[4]))+
  geom_segment(aes(x=bbox.4[3], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[4]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[2]))+
  geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[4], yend=bbox.4[4]))+
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[1], y=bbox.2[2], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.2[3], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[2]))+
  geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[4], yend=bbox.2[4]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[1], y=bbox.3[2], yend=bbox.3[4]))+
  geom_segment(aes(x=bbox.3[3], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[4]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[2]))+
  geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[4], yend=bbox.3[4]))+
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=14),
        legend.title.position = 'top',
        legend.text = element_text(size=13),
        legend.text.position = 'bottom',
        plot.title = element_text(hjust = 0.5, size=16, face='plain')) 

#### ####
p.legend.consumption <- ggplot() +
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
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'bottom',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=14),
        legend.title.position = 'top',
        legend.text = element_text(size=13),
        legend.text.position = 'bottom',
        legend.key.width = unit(0.7, 'cm'),
        plot.title = element_text(hjust = 0.5, size=16),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.margin = margin(b=0.5, unit='cm'))+
  guides(size=guide_legend(order=1))

legend.consumption <- get_plot_component(p.legend.consumption, 'guide-box-bottom', return_all = TRUE)

#### zooms ####
#### california
consumption.z1 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_jitter(data = intensity.summarise.segments.c,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.kwh.m3.mean,
               size=energy.twh.mean
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  theme_map() +
  ylim(bbox.1[2]-2, bbox.1[4]+2)+
  xlim(bbox.1[1]+2, bbox.1[3]-2)+
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16)) +
  guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
         shape = guide_legend(override.aes = list(size = 3)),
         size = guide_legend(nrow=1))

#### spain
consumption.z2 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_jitter(data = intensity.summarise.segments.c,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.kwh.m3.mean,
               size=energy.twh.mean
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  theme_map() +
  ylim(bbox.2[2]-2, bbox.2[4]+2)+
  xlim(bbox.2[1]+2, bbox.2[3]-2)+
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16)) +
  guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
         shape = guide_legend(override.aes = list(size = 3)),
         size = guide_legend(nrow=1))

#### india
consumption.z3 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_jitter(data = intensity.summarise.segments.c,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.kwh.m3.mean,
               size=energy.twh.mean
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  theme_map() +
  ylim(bbox.3[2]-2, bbox.3[4]+2)+
  xlim(bbox.3[1]+2, bbox.3[3]-2)+
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16)) +
  guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
         shape = guide_legend(override.aes = list(size = 3)),
         size = guide_legend(nrow=1))

#### china
consumption.z4 <- ggplot()+
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_jitter(data = intensity.summarise.segments.c,
             aes(
               x = intake.lon, y = intake.lat,
               fill=energy.kwh.m3.mean,
               size=energy.twh.mean,
             ),
             alpha = 0.9, stroke = 0.3, shape = 21)+
  theme_map() +
  ylim(bbox.4[2]-2, bbox.4[4]+2)+
  xlim(bbox.4[1]+2, bbox.4[3]-2)+
  theme_map() +
  scale_size_continuous(name = 'Mean energy consumption (TWh y<sup>-1</sup>)',
                        breaks=c(1,3,5,7,10),
                        range=c(2,6)) +
  scale_fill_gradient(low='#44AA99', high='#E3A43E', name = 'Consumption intensity (kwh m<sup>-3</sup>)') +
  theme(legend.position = 'none',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=12),
        legend.title.position = 'top',
        legend.text = element_text(size=11),
        plot.title = element_text(hjust = 0.5, size=16)) +
  guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
         shape = guide_legend(override.aes = list(size = 3)),
         size = guide_legend(nrow=1))


consumption.zooms.left <- plot_grid(consumption.z1, consumption.z2, nrow = 2)
consumption.zooms.right <- plot_grid(consumption.z3, consumption.z4, nrow = 2)

consumption.combined <- plot_grid(consumption.zooms.left,
                                 p.map.consumption.segments, 
                                 consumption.zooms.right, 
                                 ncol=3,
                                 rel_widths = c(0.3,1,0.3))


consumption.w.leg <- ( consumption.combined / plot_spacer() / legend.consumption ) +
  plot_layout(heights = c(1, 0.05, 0.1))
# consumption.w.leg
