#### energy production ####
#### process
first.10.hydro <- year.production %>% 
  filter(year(datetime) <= 1989) %>% 
  group_by(country, datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
            ) %>% 
  group_by(country) %>%
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high)
  ) %>% 
  mutate(decade = '1980-1989') %>% 
  mutate(energy = 'production')

last.10.hydro <- year.production %>% 
  filter(year(datetime) >= 2012)  %>% 
  group_by(country, datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  group_by(country) %>%
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high)
  ) %>%
  mutate(decade = '2012-2021') %>% 
  mutate(energy = 'production')

limits.10.production <- rbind(first.10.hydro,last.10.hydro)

factor.production <- last.10.hydro %>% 
  arrange(desc(energy.twh.mean)) %>% 
  mutate(country.factor = factor(country, levels=country))

#### energy consumption ####
#### process
first.10.pumping <- year.pumping %>% 
  filter(year(datetime) <= 1989)  %>% 
  group_by(country, datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  group_by(country) %>%
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high)
  ) %>%
  mutate(decade = '1980-1989') %>% 
  mutate(energy = 'consumption')

last.10.pumping <- year.pumping %>% 
  filter(year(datetime) >= 2012) %>% 
  group_by(country)  %>% 
  group_by(country, datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)
  ) %>% 
  group_by(country) %>%
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high)
  ) %>%
  mutate(decade = '2012-2021') %>% 
  mutate(energy = 'consumption')

limits.10.consumption <- rbind(first.10.pumping,last.10.pumping)

factor.consumption <- last.10.pumping %>%
  arrange(desc(energy.twh.mean)) %>%
  mutate(country.factor = factor(country, levels=country))


#### rbind ####
limits.10.energy <- rbind(limits.10.production,
                          limits.10.consumption)


#### factor by net energy 
country.production.mean <- year.production %>% 
  group_by(country) %>% 
  summarise(twh.production = sum(energy.twh.mean))

country.consumption.mean <- year.pumping %>% 
  group_by(country) %>% 
  summarise(twh.consumption = sum(energy.twh.mean))

country.means <- merge(country.production.mean, country.consumption.mean, all=T) %>% 
  replace_na(list(twh.production = 0, twh.consumption = 0)) %>% 
  mutate(twh.net = twh.production - twh.consumption) %>% 
  arrange(desc(twh.net))

factor.energy <- country.means %>%
  mutate(country.factor = factor(country, levels=country))

limits.10.energy <- limits.10.energy %>%
  mutate(country=factor(country, factor.energy$country.factor)) %>% 
  mutate(energy = factor(energy, 
                         levels=c('production', 'consumption')))


#### plot ####
p.energy.values <- ggplot(limits.10.energy ,
                          mapping=aes(x=country, y=energy.twh.mean, fill=energy, pattern=decade)) +
  geom_bar_pattern(
           color='black',
           pattern_fill='black',
           pattern_density = 0.1,
           pattern_spacing = 0.025,
           stat='identity', position = "dodge") +
  geom_errorbar(limits.10.energy,
                mapping= aes(x=country, ymin=energy.twh.low, ymax=energy.twh.high), 
                width=.2,  position=position_dodge(.9)) +
    scale_fill_manual(values=my.palette2,
                      labels = c('Energy production','Energy consumption')) +
    ggtitle('B. Mean energy production and consumption') +
    xlab('Country') +
    ylab('TWh y<sup>-1</sup><br />') +
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
           override.aes = list(pattern = c("none", "none"))))