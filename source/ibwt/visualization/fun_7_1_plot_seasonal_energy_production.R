#### summarise ####
production.plot.data.y <- year.hydropower %>% 
  group_by(datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  mutate(datetime = as.Date(datetime))

month.production.north <- month.hydropower %>% 
  filter(hemisphere == 'North')

month.production.south <- month.hydropower %>% 
  filter(hemisphere == 'South')

energy.first.10 <- production.plot.data.y %>% 
  filter(year(datetime) < 1989)

energy.last.10 <- production.plot.data.y %>% 
  filter(year(datetime) > 2013)

summary(energy.first.10$energy.twh.low)
summary(energy.first.10$energy.twh.mean)
summary(energy.first.10$energy.twh.high)

summary(energy.last.10$energy.twh.low)
summary(energy.last.10$energy.twh.mean)
summary(energy.last.10$energy.twh.high)

#### plot ####
p.production.north <- ggplot(month.production.north, aes(date, energy.twh.mean, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(method = 'loess', se = FALSE) +
  # scale_y_continuous(position = "right") +
  theme_bw() +
  ggtitle('C. Seasonal energy production (North)')  +
  xlab('Month')+
  ylab('TWh month<sup>-1</sup>') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_markdown()
  ) +
  scale_color_viridis_d(alpha=0.3) 

p.production.south <- ggplot(month.production.south, aes(date, energy.twh.mean, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(se = FALSE) +
  scale_y_continuous(position = "right") +
  theme_bw() +
  ggtitle('D. Seasonal energy production (South)') +
  xlab('Month')+
  ylab('TWh month<sup>-1</sup>') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_markdown()
  ) +
  scale_color_viridis_d(alpha=0.3) 


p.seasonal.production <- plot_grid(p.production.north,
                                   p.production.south,
                                   nrow=1)
