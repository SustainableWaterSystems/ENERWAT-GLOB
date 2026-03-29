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
                        breaks = c(1,3,6,9,12),
                        range=c(2,9)) +
  scale_fill_gradient(low="#9167c8", high= "#df88fa", 
                      name = 'Production intensity (m<sup>3</sup> kwh<sup>-1</sup>)',
                      limits=c(0,30))+
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



# p.map.production.segments <- ggplot() +
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_point(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean,
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   ylim(-55,70) +
#   ggtitle('A. Hydropower plants') +
#   geom_segment(aes(x=bbox.1[1], xend=bbox.1[1], y=bbox.1[2], yend=bbox.1[4]))+
#   geom_segment(aes(x=bbox.1[3], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[4]))+
#   geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[2], yend=bbox.1[2]))+
#   geom_segment(aes(x=bbox.1[1], xend=bbox.1[3], y=bbox.1[4], yend=bbox.1[4]))+
#   geom_segment(aes(x=bbox.4[1], xend=bbox.4[1], y=bbox.4[2], yend=bbox.4[4]))+
#   geom_segment(aes(x=bbox.4[3], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[4]))+
#   geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[2], yend=bbox.4[2]))+
#   geom_segment(aes(x=bbox.4[1], xend=bbox.4[3], y=bbox.4[4], yend=bbox.4[4]))+
#   geom_segment(aes(x=bbox.2[1], xend=bbox.2[1], y=bbox.2[2], yend=bbox.2[4]))+
#   geom_segment(aes(x=bbox.2[3], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[4]))+
#   geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[2], yend=bbox.2[2]))+
#   geom_segment(aes(x=bbox.2[1], xend=bbox.2[3], y=bbox.2[4], yend=bbox.2[4]))+
#   geom_segment(aes(x=bbox.3[1], xend=bbox.3[1], y=bbox.3[2], yend=bbox.3[4]))+
#   geom_segment(aes(x=bbox.3[3], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[4]))+
#   geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[2], yend=bbox.3[2]))+
#   geom_segment(aes(x=bbox.3[1], xend=bbox.3[3], y=bbox.3[4], yend=bbox.3[4]))+
#   theme_map() +
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (m<sup>3</sup> kwh<sup>-1</sup>)') +
#   theme(legend.position = 'none',
#         legend.justification = 'center',
#         legend.spacing= unit(1.5, 'cm'),
#         legend.title = element_markdown(hjust=0.5, size=12),
#         legend.title.position = 'top',
#         legend.text = element_text(size=11),
#         legend.text.position = 'bottom',
#         plot.title = element_text(hjust = 0.5, size=16, face='plain')) 
# 
# 
# 
# #### ####
# p.legend.production <- ggplot() +
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_point(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   # ylim(-55,70) +
#   ggtitle('As. Hydropower plants') +
#   theme_map() +
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa",
#                       name = 'Production intensity (m<sup>3</sup> kwh<sup>-1</sup>)',
#                       limits = c(0,30)
#                         ) +
#   theme(legend.position = 'bottom',
#         legend.justification = 'center',
#         legend.spacing= unit(1.5, 'cm'),
#         legend.title = element_markdown(hjust=0.5, size=14),
#         legend.title.position = 'top',
#         legend.text = element_text(size=13),
#         legend.text.position = 'bottom',
#         legend.key.width = unit(0.7, 'cm'),
#         plot.title = element_text(hjust = 0.5, size=16, face='plain')) +
#   guides(size=guide_legend(order=1, override.aes = list(fill='black')))
# 
# 
# legend.production <- get_plot_component(p.legend.production, 'guide-box-bottom', return_all = TRUE)
# 
# #### zooms 
# production.z1 <- ggplot()+
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_jitter(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   theme_map() +
#   ylim(bbox.1[2]-2, bbox.1[4]+2)+
#   xlim(bbox.1[1]+2, bbox.1[3]-2)+
#   theme_map()+
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (kwh m<sup>-3</sup>)') +
#   theme(legend.position = 'none',
#         plot.title = element_text(hjust = 0.5, size=16)) +
#   guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
#          shape = guide_legend(override.aes = list(size = 3)),
#          size = guide_legend(nrow=1))
# 
# production.z2 <- ggplot()+
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_jitter(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   theme_map() +
#   ylim(bbox.2[2]-2, bbox.2[4]+2)+
#   xlim(bbox.2[1]+2, bbox.2[3]-2)+
#   theme_map() +
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (kwh m<sup>-3</sup>)') +
#   theme(legend.position = 'none',
#         plot.title = element_text(hjust = 0.5, size=16)) +
#   guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
#          shape = guide_legend(override.aes = list(size = 3)),
#          size = guide_legend(nrow=1))
# 
# production.z3 <- ggplot()+
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_jitter(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   theme_map() +
#   ylim(bbox.3[2]-2, bbox.3[4]+2)+
#   xlim(bbox.3[1]+2, bbox.3[3]-2)+
#   theme_map()+
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (kwh m<sup>-3</sup>)') +
#   theme(legend.position = 'none',
#         plot.title = element_text(hjust = 0.5, size=16)) +
#   guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
#          shape = guide_legend(override.aes = list(size = 3)),
#          size = guide_legend(nrow=1))
# 
# production.z4 <- ggplot()+
#   geom_map(
#     data = wg, map = wg,
#     aes(long, lat, map_id = region),
#     color = "white", fill= "grey", alpha=0.6
#   ) +
#   geom_jitter(data = intensity.summarise.segments.p,
#              aes(
#                x = intake.lon, y = intake.lat,
#                fill=energy.m3.kwh.mean,
#                size=energy.twh.mean
#              ),
#              alpha = 0.9, stroke = 0.3, shape = 21)+
#   theme_map() +
#   ylim(bbox.4[2]-2, bbox.4[4]+2)+
#   xlim(bbox.4[1]+2, bbox.4[3]-2)+
#   theme_map() +
#   scale_size_continuous(name = 'Mean energy production (TWh y<sup>-1</sup>)',
#                         range=c(2,9)) +
#   scale_fill_gradient(low="#9167c8", high= "#df88fa", name = 'Production intensity (kwh m<sup>-3</sup>)') +
#   theme(legend.position = 'none',
#         legend.direction = 'vertical',
#         legend.justification = 'center',
#         legend.spacing= unit(1.5, 'cm'),
#         legend.title = element_markdown(hjust=0.5, size=12),
#         legend.title.position = 'top',
#         legend.text = element_text(size=11),
#         plot.title = element_text(hjust = 0.5, size=16)) +
#   guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
#          shape = guide_legend(override.aes = list(size = 3)),
#          size = guide_legend(nrow=1))
# 
# production.zooms.left <- plot_grid(production.z1, production.z2, nrow = 2)
# production.zooms.right <- plot_grid(production.z3, production.z4, nrow = 2)
# 
# production.combined <- plot_grid(production.zooms.left,
#                                  p.map.production.segments, 
#                                  production.zooms.right, 
#                                  ncol=3,
#                                  rel_widths = c(0.3,1,0.3))
# 
# production.w.leg <- ( production.combined / plot_spacer() / legend.production ) +
#   plot_layout(heights = c(1, 0.05, 0.1))