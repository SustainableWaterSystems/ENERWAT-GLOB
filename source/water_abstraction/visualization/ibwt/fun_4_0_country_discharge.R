#### discharge ####
#### process
first.10.discharge <- year.discharge.types %>% 
  filter(year(datetime) <= 1989) %>% 
  group_by(country) %>% 
  summarise(km3.total = mean(km3.total, na.rm=T),
            km3.consumption = mean(km3.consumption, na.rm=T),
            km3.production = mean(km3.production, na.rm=T),
            km3.discharge.only = mean(km3.discharge.only, na.rm=T)) %>% 
  mutate(decade = '1980-1989')

last.10.discharge <- year.discharge.types %>% 
  filter(year(datetime) >= 2012) %>% 
  group_by(country) %>% 
  summarise(km3.total = mean(km3.total, na.rm=T),
            km3.consumption = mean(km3.consumption, na.rm=T),
            km3.production = mean(km3.production, na.rm=T),
            km3.discharge.only = mean(km3.discharge.only, na.rm=T)) %>% 
  mutate(decade = '2012-2021')

limits.discharge <- rbind(first.10.discharge,
                          last.10.discharge)

factor.discharge <- last.10.discharge %>% 
  arrange(desc(km3.total)) %>% 
  mutate(country.factor = factor(country, levels=country))

plot.data.discharge <- pivot_longer(limits.discharge,
                                    cols = km3.total:km3.discharge.only,
                                    names_to = "var.names",
                                    values_to = "discharge.km3") %>% 
  mutate(var.names = factor(var.names, levels=c('km3.total',
                                                'km3.discharge.only',
                                                'km3.production',
                                                'km3.consumption'
  )))

#### plot 
p.10.discharge <- ggplot() +
  geom_col_pattern(plot.data.discharge %>% 
                     mutate(country=factor(country, factor.discharge$country.factor)) %>% 
                     filter(!is.na(discharge.km3)), 
                   mapping=aes(x=country, 
                               y=discharge.km3, 
                               fill=var.names,
                               pattern=decade
                   ),
                   color='black',
                   pattern_fill='black',
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   position = "dodge") +
  ggtitle('A. Mean discharge by type')+
  xlab('Country') +
  ylab('km<sup>3</sup> y<sup>-1</sup><br />') +
  scale_fill_manual(values=my.palette,
                    labels = c('Total','Discharge only','Hydropower','Pumping')) +
  theme_bw() +
  theme(axis.title.y = element_markdown(size=14),
        plot.title = element_text(hjust = 0.5,size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size=12)
  ) + 
  guides(pattern=guide_legend(override.aes=list(fill=NA)),
         fill = guide_legend(
           override.aes = list(pattern = c("none", "none", "none","none"))))
