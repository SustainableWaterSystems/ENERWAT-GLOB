#### 
p.data.ts <- pivot_longer(discharge.types %>% select(datetime:km3.discharge.only),
                       cols = !c(datetime), 
                       names_to= 'type', 
                       values_to= 'km3') %>% 
  mutate(type = factor(type, levels=c('km3.total',
                                      'km3.discharge.only',
                                      'km3.production',
                                      'km3.consumption'
                                      )))

#### plot
p.discharge.y.types <- ggplot() +
  geom_line(p.data.ts, mapping = aes(x=datetime, y=km3, color=type), size=1) + 
  ggtitle('A. Total yearly discharge per intake type') +
  xlab('Year') +
  ylab('km<sup>3</sup> y<sup>-1</sup>') +
  scale_color_manual(values = palette.timeseries,
                     labels = c('Total',
                                'Discharge only',
                                'Hydropower (Energy production)',
                                'Pumping (Energy consumption)')) + 
  theme_bw() +
  theme(axis.title = element_markdown(size=14),
        axis.text = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size=16),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_blank(),
        legend.text = element_markdown(size=13)
  )+
  guides(color=guide_legend(nrow=2, override.aes = list(shape = 22, linewidth = 5)))

#### calculate discharge trend
discharge.idx <- p.data.ts %>% 
  filter(type == 'km3.total') %>% 
  mutate(idx = seq(42))

discharge.year.lm <- lm(km3~idx, data=discharge.idx)

lm.tidy <- broom::tidy(discharge.year.lm)
summary(discharge.year.lm)

discharge.first <- p.data.ts %>% 
  filter(type == 'km3.total') %>% 
  filter(year(datetime) <= 1989)

discharge.last <- p.data.ts %>% 
  filter(type == 'km3.total') %>% 
  filter(year(datetime) >= 2012)

discharge.summary <- p.data.ts %>% 
  group_by(type) %>% 
  summarise(km3 = mean(km3))
