palette.feed <- c("#000000", "#7FC97F", "#AFF80A","#BEAED4", "#FDC086", 
                  "#386CB0", "#F0027F")
palette.feed.only <- c("#7FC97F", "#AFF80A","#BEAED4", "#FDC086", 
                       "#386CB0", "#F0027F")

#### load ####
desalData.energy <- read.csv(paste0(inputDir, 'DesalDataEnergy_online_2019.csv')) %>%
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) 

desalData.energy.RO <- vroom(
  paste0(inputDir,'DesalDataEnergy_RO_time_evolving.csv'), show_col_types = F) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  mutate(year.exponential=as.Date(paste(year.exponential, 1, 1, sep = "-"))) %>% 
  filter(!grepl('Construction', Plant.status)) %>%  
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status)) 

Pure <- desalData.energy[grepl("Pure", desalData.energy$Feedwater), ]

River <-  desalData.energy[grepl("River", desalData.energy$Feedwater), ]

Brackish <- desalData.energy[grepl("Brackish", desalData.energy$Feedwater), ]

Wastewater <-  desalData.energy[grepl("Wastewater", desalData.energy$Feedwater), ]

Seawater <-  desalData.energy[grepl("Seawater", desalData.energy$Feedwater), ]

Brine <-  desalData.energy[grepl("Brine", desalData.energy$Feedwater), ]


#### calculate cumulative capacities (m3.d) ####

#calculate cumulative capacity
cum.capacity.all.tech.frame <- desalData.energy %>% group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Total')

#create vector with the whole timeframe of DesalData
years_range_desalData <- as.data.frame(
  as.Date(
    seq.Date(
      cum.capacity.all.tech.frame$Online.date[1],
      cum.capacity.all.tech.frame$Online.date[nrow(cum.capacity.all.tech.frame)],
      by='year'))) %>% dplyr::rename(Online.date=1)

cum.capacity.all.tech.frame <- cum.capacity.all.tech.frame %>%
  merge(years_range_desalData, ., all=T)
cum.capacity.all.tech.frame$m3.d[2] <- 0
cum.capacity.all.tech.frame$cum.m3.d[2] <- cum.capacity.all.tech.frame$cum.m3.d[1]
cum.capacity.all.tech.frame$cum.m3.y[2] <- cum.capacity.all.tech.frame$cum.m3.y[1]
cum.capacity.all.tech.frame$tech[2] <- cum.capacity.all.tech.frame$tech[1]

#calculate capacities of indibidual feeds
Pure.C <- Pure %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Pure')

River.C <- River %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='River')

Brackish.C <- Brackish %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Brackish')

Wastewater.C <- Wastewater %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Wastewater')

Seawater.C <- Seawater %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Seawater')

Brine.C <- Brine %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Brine')

#checksum
cum.capacity.all.feeds <- Pure.C$cum.m3.d + River.C$cum.m3.d +
  Brackish.C$cum.m3.d + Wastewater.C$cum.m3.d + 
  Seawater.C$cum.m3.d +
  Brine.C$cum.m3.d

#### calculate cumulative for RO ####
#calculate cumulative per subregion
RO.feed.og <- desalData.energy.RO %>%
  group_by(year.exponential, Feedwater) %>%
  summarise(kwh.d.low = sum(kwh.d.low),
            kwh.d.mean = sum(kwh.d.mean),
            kwh.d.high = sum(kwh.d.high)) 

RO.feed.og$feed <- NA
RO.feed.og$feed[grep("Pure", RO.feed.og$Feedwater)] <- 'Pure'
RO.feed.og$feed[grep("River", RO.feed.og$Feedwater)] <- 'River'
RO.feed.og$feed[grep("Wastewater", RO.feed.og$Feedwater)] <- 'Wastewater'
RO.feed.og$feed[grep("Brackish", RO.feed.og$Feedwater)] <- 'Brackish'
RO.feed.og$feed[grep("Seawater", RO.feed.og$Feedwater)] <- 'Seawater'
RO.feed.og$feed[grep("Brine", RO.feed.og$Feedwater)] <- 'Brine'

feeds <- unique(RO.feed.og$feed)

feeds.cum.list <- list()

for(idx.feed in seq(1, length(feeds))){
  
  replaceMessage(paste0('Subregion: ', idx.feed, '/', length(feeds)))
  
  # idx.subr = 1
  feed.sel <- feeds[idx.feed]
  
  RO.feed <- RO.feed.og %>% 
    filter(feed == feed.sel)
  
  RO.feed.years <- RO.feed %>%
    merge(years_range_desalData %>%  
            rename(year.exponential = Online.date), ., all=T) %>% 
    filter(year.exponential <= max(desalData.energy.RO$Online.date)) %>% 
    fill(Feedwater, .direction = 'up') %>% 
    replace(is.na(.),0) %>%
    mutate(cum.kwh.d.low=kwh.d.low) %>%
    mutate(cum.kwh.d.mean=kwh.d.mean) %>%
    mutate(cum.kwh.d.high=kwh.d.high) %>%
    mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
    mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
    mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>% 
    mutate(feed = feed.sel) %>% 
    rename(Online.date = year.exponential)
  
  feeds.cum.list[[idx.feed]] <- RO.feed.years
  
}

feeds.cum <- do.call(rbind, feeds.cum.list)


Pure.E.RO <- feeds.cum %>% 
  filter(feed == 'Pure')
River.E.RO <- feeds.cum %>% 
  filter(feed == 'River')
Wastewater.E.RO <- feeds.cum %>% 
  filter(feed == 'Wastewater')
Brackish.E.RO <- feeds.cum %>% 
  filter(feed == 'Brackish')
Seawater.E.RO <- feeds.cum %>% 
  filter(feed == 'Seawater')
Brine.E.RO <- feeds.cum %>% 
  filter(feed == 'Brine')


#### calculate cumulative energy consumption (kwh.y) ####
#assume wastewater salinity is between brackish and seawater
Pure.E <- Pure %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Pure.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Pure.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Pure.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Pure')

River.E <- River %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + River.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + River.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + River.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='River')

Brackish.E <- Brackish %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Brackish.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Brackish.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Brackish.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Brackish')

Wastewater.E <- Wastewater %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Wastewater.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Wastewater.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Wastewater.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Wastewater')

Seawater.E <- Seawater %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Seawater.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Seawater.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Seawater.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Seawater')

Brine.E <- Brine %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + Brine.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + Brine.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + Brine.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='Brine')


#years
cum.energy.years <- years_range_desalData

#kwh.d
cum.all.tech.kwh.d.low <- Pure.E$kwh.d.low + River.E$kwh.d.low + Brackish.E$kwh.d.low +
  Wastewater.E$kwh.d.low +
  Seawater.E$kwh.d.low + Brine.E$kwh.d.low 
cum.all.tech.kwh.d.high <- Pure.E$kwh.d.high + River.E$kwh.d.high + Brackish.E$kwh.d.high +
  Wastewater.E$kwh.d.high +
  Seawater.E$kwh.d.high + Brine.E$kwh.d.high 
cum.all.tech.kwh.d.mean <- Pure.E$kwh.d.mean + River.E$kwh.d.mean + Brackish.E$kwh.d.mean +
  Wastewater.E$kwh.d.mean +
  Seawater.E$kwh.d.mean + Brine.E$kwh.d.mean 

#cum kwh.d
cum.all.tech.cum.kwh.d.low <- Pure.E$cum.kwh.d.low + River.E$cum.kwh.d.low + Brackish.E$cum.kwh.d.low +
  Wastewater.E$cum.kwh.d.low +
  Seawater.E$cum.kwh.d.low + Brine.E$cum.kwh.d.low 
cum.all.tech.cum.kwh.d.high <- Pure.E$cum.kwh.d.high + River.E$cum.kwh.d.high + Brackish.E$cum.kwh.d.high +
  Wastewater.E$cum.kwh.d.high +
  Seawater.E$cum.kwh.d.high + Brine.E$cum.kwh.d.high 
cum.all.tech.cum.kwh.d.mean <- Pure.E$cum.kwh.d.mean + River.E$cum.kwh.d.mean + Brackish.E$cum.kwh.d.mean +
  Wastewater.E$cum.kwh.d.mean +
  Seawater.E$cum.kwh.d.mean + Brine.E$cum.kwh.d.mean  

#total energy
cum.energy.all.tech.low <- Pure.E$cum.kwh.y.low + River.E$cum.kwh.y.low + Brackish.E$cum.kwh.y.low +
  Wastewater.E$cum.kwh.y.low +
  Seawater.E$cum.kwh.y.low + Brine.E$cum.kwh.y.low  
cum.energy.all.tech.high <- Pure.E$cum.kwh.y.high + River.E$cum.kwh.y.high + Brackish.E$cum.kwh.y.high +
  Wastewater.E$cum.kwh.y.high +
  Seawater.E$cum.kwh.y.high + Brine.E$cum.kwh.y.high  
cum.energy.all.tech.mean <- Pure.E$cum.kwh.y.mean + River.E$cum.kwh.y.mean + Brackish.E$cum.kwh.y.mean +
  Wastewater.E$cum.kwh.y.mean +
  Seawater.E$cum.kwh.y.mean + Brine.E$cum.kwh.y.mean  

cum.energy.all.tech.frame <- data.frame(
  cum.energy.years,
  cum.all.tech.kwh.d.low, cum.all.tech.kwh.d.mean, cum.all.tech.kwh.d.high,
  cum.all.tech.cum.kwh.d.low, cum.all.tech.cum.kwh.d.mean, cum.all.tech.cum.kwh.d.high,
  cum.energy.all.tech.low, cum.energy.all.tech.mean, cum.energy.all.tech.high) %>% 
  mutate(tech='Total')

colnames(cum.energy.all.tech.frame) <- colnames(Pure.E)

#### make dataframes for plotting ####
#### capacity
capacity.plot.data <- rbind(cum.capacity.all.tech.frame %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Pure.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            River.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Brackish.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Wastewater.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Seawater.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds),
                            Brine.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.feeds)) %>%
  mutate(cum.million.m3.d = cum.m3.d / 10^6) %>%
  mutate(tech = factor(tech, levels=c('Total',  'Pure', 'River', 'Wastewater',
                                      'Brackish', 'Seawater', 'Brine')))

#### energy
energy.plot.data <- rbind(cum.energy.all.tech.frame %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Pure.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          River.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          Brackish.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Wastewater.E %>%
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          Seawater.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean), 
                          Brine.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean)
) %>%
  mutate(cum.twh.y.low = cum.kwh.y.low / 10^9) %>% 
  mutate(cum.twh.y.mean = cum.kwh.y.mean / 10^9) %>% 
  mutate(cum.twh.y.high = cum.kwh.y.high / 10^9) %>% 
  mutate(tech = factor(tech, levels=c('Total',  'Pure', 'River', 'Wastewater',
                                      'Brackish', 'Seawater', 'Brine')))

#### plot ####
#capacity
m3.day.plot.feed <- ggplot(capacity.plot.data, aes(x=Online.date, y=cum.m3.y / 10^9,
                                                   color=tech)) +
  geom_line(linewidth=1.2) +
  ggtitle('Cumulative capacity') +
  scale_color_manual(values=palette.feed) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=14),
        axis.title.y = element_text(size=12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  guides(color=guide_legend(title="Feedwater type", ncol=2))

#energy
twh.day.plot.feed <- ggplot(energy.plot.data, aes(x=Online.date, y=cum.twh.y.mean, 
                                                  color=tech)) +
  geom_line(linewidth=1.2) +
  geom_ribbon(aes(x=Online.date, ymin=cum.twh.y.low, ymax=cum.twh.y.high, fill=tech),
              alpha=0.4, inherit.aes=F, show.legend=F) +
  scale_color_manual(values=palette.feed) +
  scale_fill_manual(values=palette.feed)+
  ylab('\nTWh/year\n')+
  scale_y_continuous(sec.axis = sec_axis(~.*0.0036, name='EJ/year\n')) +
  ggtitle('Cumulative energy use') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=14),
        axis.title.y = element_text(size=12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position = 'bottom') +
  guides(color=guide_legend(title="Feedwater type", ncol=2))


#### ratios 
#capacity
ratio.capacity.plot.feed <- ggplot(capacity.plot.data %>%
                                     filter(!grepl('Total',tech)),
                                   aes(x=Online.date, y=ratio.capacity,
                                       color=tech)) +
  geom_line(linewidth=1.2) +
  xlab('\nYear') +
  ylab('Ratio\n')+
  ggtitle('\nCapacity share') +
  scale_color_manual(values=palette.feed.only, 
                     guide="none") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=14),
        axis.title.y = element_text(size=12))

#energy
ratio.energy.plot.feed <- ggplot(energy.plot.data %>%
                                   filter(!grepl('Total',tech)),
                                 aes(x=Online.date, y=ratio.energy.mean,
                                     color=tech) ) +
  geom_line(linewidth=1.2) +
  xlab('\nYear') +
  ylab('Ratio\n')+
  ggtitle('\nEnergy use share') +
  scale_color_manual(values=palette.feed.only, 
                     guide="none") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=14),
        axis.text.y=element_blank(),
        axis.title.y=element_blank()) 
#patch together
combined.cumulative.feed <- m3.day.plot.feed + twh.day.plot.feed

# combined_cumulative
combined.ratios.feed <- (ratio.capacity.plot.feed 
                         + plot_spacer() + ratio.energy.plot.feed) +
  plot_layout(widths = c(4, 0.38 ,4 ))

# combined_ratios
combined.all.feed <- (combined.cumulative.feed / combined.ratios.feed) +
  plot_annotation('Feedwater types\n',
                  theme=theme(plot.title=element_text(hjust=0.5,
                                                      size=16))) +
  plot_layout(guides = 'collect') &
  theme(plot.title = element_text(hjust= 0.5, face='bold'),
        legend.position="bottom")


