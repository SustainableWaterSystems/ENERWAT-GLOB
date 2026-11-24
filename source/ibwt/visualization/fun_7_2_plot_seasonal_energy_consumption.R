#### summarise ####
consumption.plot.data.y <- year.pumping %>% 
  group_by(datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  mutate(datetime = as.Date(datetime))

month.consumption.north <- month.pumping %>% 
  filter(hemisphere == 'North')

month.consumption.south <- month.pumping %>% 
  filter(hemisphere == 'South')

energy.first.10 <- consumption.plot.data.y %>% 
  filter(year(datetime) < 1989)

energy.last.10 <- consumption.plot.data.y %>% 
  filter(year(datetime) > 2013)

p.consumption.north <- ggplot(month.consumption.north, aes(date, energy.twh.mean, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(method = 'loess', se = FALSE) +
  # scale_y_continuous(position = "right") +
  theme_bw() +
  ggtitle('E. Seasonal energy consumption (North)')  +
  xlab('Month')+
  ylab('TWh month<sup>-1</sup>') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_markdown()
  ) +
  scale_color_viridis_d(alpha=0.3) 

p.consumption.south <- ggplot(month.consumption.south, aes(date, energy.twh.mean, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(se = FALSE) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  ggtitle('F. Seasonal energy consumption (South)') +
  xlab('Month')+
  ylab('TWh month<sup>-1</sup>') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_markdown()
  ) +
  scale_color_viridis_d(alpha=0.3) 

p.seasonal.consumption <- plot_grid(p.consumption.north,
                             p.consumption.south,
                             nrow=1)