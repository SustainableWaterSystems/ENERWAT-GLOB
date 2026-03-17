####function to plot discharge timeseries ####
seasonal.data.north <- discharge.monthly.df %>% 
  filter(hemisphere == "North")

seasonal.data.south <- discharge.monthly.df %>% 
  filter(hemisphere == "South")

transfer.discharges.mean <- discharge.yearly.df %>% 
  group_by(transfer.name) %>% 
  summarise(discharge.km3.mean = mean(discharge.km3.y.corrected)
  )

discharge.plot.data.y <- discharge.yearly.df %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.y.corrected = sum(discharge.km3.y.corrected)) %>%
  mutate(idx = seq(42))

discharge.first.10 <- discharge.plot.data.y %>% 
  filter(year(datetime) < 1989)

discharge.last.10 <- discharge.plot.data.y %>% 
  filter(year(datetime) > 2013)

discharge.plot.data.m.n <- discharge.monthly.df %>% 
  filter(hemisphere == 'North') %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.month = sum(discharge.km3.month))

discharge.plot.data.m.s <- discharge.monthly.df %>% 
  filter(hemisphere == 'South') %>% 
  group_by(datetime) %>% 
  summarise(discharge.km3.month = sum(discharge.km3.month))

p.seasonal.north <- ggplot(seasonal.data.north, aes(date, discharge.km3.month, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(se = FALSE) +
  theme_bw() +
  ggtitle('A. Seasonal discharge (North)') +
  xlab('Month') +
  ylab('km<sup>3</sup> month<sup>-1</sup>') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_markdown()
  ) +
  scale_color_viridis_d(alpha=0.3)

p.seasonal.south <- ggplot(seasonal.data.south, aes(date, discharge.km3.month, color = year.factor)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_smooth(alpha=0.1, se = FALSE) +
  ggtitle('B. Seasonal discharge (South)') +
  xlab('Month') +
  ylab('km<sup>3</sup> month<sup>-1</sup>') +
  scale_y_continuous(position = "right")+
  theme_bw()  +
  theme(legend.position = 'none',
        axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5)
        ) +
  scale_color_viridis_d(alpha=0.3) 

#### patchwork #### 
p.seasonal.discharge <- plot_grid(p.seasonal.north,
                             p.seasonal.south,
                             nrow=1)