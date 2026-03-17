#### energy intensity ####
# factor.intensity.p <- last.10.hydro %>% 
#   arrange(desc(energy.m3.kwh.mean)) %>% 
#   mutate(country.factor = factor(country, levels=country))

factor.intensity.p <- year.production %>% 
  # filter(transfer.name != 'All_American_Canal') %>%
  group_by(country, transfer.name, segment.id) %>% 
  summarise(energy.m3.kwh.low = median(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = median(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = median(energy.m3.kwh.high, na.rm=T),
            energy.m3.kwh.km.low = median(energy.m3.kwh.km.low, na.rm=T),
            energy.m3.kwh.km.mean = median(energy.m3.kwh.km.mean, na.rm=T),
            energy.m3.kwh.km.high = median(energy.m3.kwh.km.high, na.rm=T)) %>% 
  group_by(country) %>% 
  summarise(energy.m3.kwh.low = median(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = median(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = median(energy.m3.kwh.high, na.rm=T),
            energy.m3.kwh.km.low = median(energy.m3.kwh.km.low, na.rm=T),
            energy.m3.kwh.km.mean = median(energy.m3.kwh.km.mean, na.rm=T),
            energy.m3.kwh.km.high = median(energy.m3.kwh.km.high, na.rm=T)) %>% 
  # summarise(energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
  #           energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
  #           energy.m3.kwh.high = mean(energy.m3.kwh.high, na.rm=T),
  #           energy.m3.kwh.km.low = mean(energy.m3.kwh.km.low, na.rm=T),
  #           energy.m3.kwh.km.mean = mean(energy.m3.kwh.km.mean, na.rm=T),
  #           energy.m3.kwh.km.high = mean(energy.m3.kwh.km.high, na.rm=T)) %>% 
    arrange(desc(energy.m3.kwh.mean)) %>%
    mutate(country.factor = factor(country, levels=country))

scaling.factor.plot.p <- 10

#### error bars development
#### low values
var1 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.low, 
                   variable = 'm3.kwh') 
colnames(var1)[1:2] <- c('Country','low')

var2 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.km.low * scaling.factor.plot.p,
                   variable = 'm3.kwh.km') 
colnames(var2)[1:2] <- c('Country','low')

df.low <- rbind(var1, var2)

#### mean values
var3 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.mean, 
                   variable = 'm3.kwh') 
colnames(var3)[1:2] <- c('Country','mean')

var4 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.km.mean * scaling.factor.plot.p,
                   variable = 'm3.kwh.km') 
colnames(var4)[1:2] <- c('Country','mean')

df.mean <- rbind(var3, var4)


#### high values
var5 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.high, 
                   variable = 'm3.kwh') 
colnames(var5)[1:2] <- c('Country','high')

var6 <- data.frame(factor.intensity.p$country, 
                   factor.intensity.p$energy.m3.kwh.km.high * scaling.factor.plot.p, 
                   variable = 'm3.kwh.km') 
colnames(var6)[1:2] <- c('Country','high')

df.high <- rbind(var5, var6)


####
df.intensity.p <- data.frame(cbind(df.low$Country,
                                   df.low$low,
                                   df.mean$mean,
                                   df.high$high,
                                   df.high$variable)
)
colnames(df.intensity.p) <- c('Country','low','mean','high','variable')

intensity.plot.df.p <- df.intensity.p %>% 
  mutate(low = as.numeric(low)) %>% 
  mutate(mean = as.numeric(mean)) %>% 
  mutate(high = as.numeric(high)) 

#### energy intensity
# factor.intensity.c <- last.10.pumping %>% 
#   arrange(desc(energy.kwh.m3.mean)) %>% 
#   mutate(country.factor = factor(country, levels=country))

factor.intensity.c <- year.pumping %>%  
  group_by(country, transfer.name, segment.id) %>%
  summarise(energy.kwh.m3.low = median(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = median(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = median(energy.kwh.m3.high, na.rm=T),
            energy.kwh.m3.km.low = median(energy.kwh.m3.km.low, na.rm=T),
            energy.kwh.m3.km.mean = median(energy.kwh.m3.km.mean, na.rm=T),
            energy.kwh.m3.km.high = median(energy.kwh.m3.km.high, na.rm=T)
  ) %>%  
  group_by(country) %>%
  summarise(energy.kwh.m3.low = median(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = median(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = median(energy.kwh.m3.high, na.rm=T),
            energy.kwh.m3.km.low = median(energy.kwh.m3.km.low, na.rm=T),
            energy.kwh.m3.km.mean = median(energy.kwh.m3.km.mean, na.rm=T),
            energy.kwh.m3.km.high = median(energy.kwh.m3.km.high, na.rm=T)
  ) %>%  
  # summarise(energy.kwh.m3.low = mean(energy.kwh.m3.low, na.rm=T),
  #           energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm=T),
  #           energy.kwh.m3.high = mean(energy.kwh.m3.high, na.rm=T),
  #           energy.kwh.m3.km.low = mean(energy.kwh.m3.km.low, na.rm=T),
  #           energy.kwh.m3.km.mean = mean(energy.kwh.m3.km.mean, na.rm=T),
  #           energy.kwh.m3.km.high = mean(energy.kwh.m3.km.high, na.rm=T)
  # ) %>%  
  arrange(desc(energy.kwh.m3.mean)) %>% 
  mutate(country.factor = factor(country, levels=country))

scaling.factor.plot.c <- 10

#### error bars development
#### low values
var1 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.low, 
                   variable = 'kwh.m3') 
colnames(var1)[1:2] <- c('Country','low')

var2 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.km.low * scaling.factor.plot.c,
                   variable = 'kwh.m3.km') 
colnames(var2)[1:2] <- c('Country','low')

df.low <- rbind(var1, var2)

#### mean values
var3 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.mean, 
                   variable = 'kwh.m3') 
colnames(var3)[1:2] <- c('Country','mean')

var4 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.km.mean * scaling.factor.plot.c,
                   variable = 'kwh.m3.km') 
colnames(var4)[1:2] <- c('Country','mean')

df.mean <- rbind(var3, var4)


#### high values
var5 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.high, 
                   variable = 'kwh.m3') 
colnames(var5)[1:2] <- c('Country','high')

var6 <- data.frame(factor.intensity.c$country, 
                   factor.intensity.c$energy.kwh.m3.km.high * scaling.factor.plot.c, 
                   variable = 'kwh.m3.km') 
colnames(var6)[1:2] <- c('Country','high')

df.high <- rbind(var5, var6)


####
df.intensity.c <- data.frame(cbind(df.low$Country,
                                   df.low$low,
                                   df.mean$mean,
                                   df.high$high,
                                   df.high$variable)
)
colnames(df.intensity.c) <- c('Country','low','mean','high','variable')

intensity.plot.df.c <- df.intensity.c %>% 
  mutate(low = as.numeric(low)) %>% 
  mutate(mean = as.numeric(mean)) %>% 
  mutate(high = as.numeric(high)) 

#### plot ####
p.intensity.p <- ggplot(intensity.plot.df.p %>% 
                          mutate(Country = factor(Country, factor.intensity.p$country.factor)), 
                        mapping=aes(x=Country,y=mean,fill=variable))  +
  geom_bar(stat='identity', position='dodge', color='black') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  ggtitle('C. Energy production intensity') +
  xlab('Country') +
  ylab('m<sup>3</sup> kWh<sup>-1</sup><br />') +
  scale_fill_manual(values=my.palette3,
                    labels = c('m<sup>3</sup> kWh<sup>-1</sup>',
                               'm<sup>3</sup> kWh<sup>-1</sup> km<sup>-1</sup>')) +
  scale_y_continuous(
    sec.axis = sec_axis(~./scaling.factor.plot.p, name = 'm<sup>3</sup> kWh<sup>-1</sup> km<sup>-1</sup><br />'),
    limits=c(0,4)) +
  theme_bw() +
  theme(axis.title.y = element_markdown(size=14),
        plot.title = element_text(hjust = 0.5, size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_markdown(size=12)
  )


p.intensity.c <- ggplot(intensity.plot.df.c %>% 
                          mutate(Country = factor(Country, factor.intensity.c$country.factor)), 
                        mapping=aes(x=Country,y=mean,fill=variable))  +
  geom_bar(stat='identity', position='dodge', color='black') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  ggtitle('D. Energy consumption intensity') +
  xlab('Country') +
  ylab('kWh m<sup>-3</sup> <br />') +
  scale_fill_manual(values=my.palette3,
                    labels = c('kWh m<sup>-3</sup>',
                               'kWh m<sup>-3</sup> km<sup>-1</sup>')) +
  scale_y_continuous(
    sec.axis = sec_axis(~./scaling.factor.plot.c, name = 'kWh m<sup>-3</sup> km<sup>-1</sup><br />'),
    limits=c(0,4)) +
  theme_bw() +
  theme(axis.title.y = element_markdown(size=14),
        plot.title = element_text(hjust = 0.5,size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_markdown(size=12)
  )

# p.intensity <- plot_grid(p.intensity.p, p.intensity.c)

p.intensity <- (p.intensity.p + p.intensity.c)
