#### load ####
# discharge.monthly <- read.csv(
#   paste0(inputDirDischarge, '1_bias_corrected/0_0_discharge_bias_corrected_monthly.csv'))
# discharge.yearly <- read.csv(
#   paste0(inputDirDischarge, '1_bias_corrected/0_1_discharge_bias_corrected_yearly.csv'))
# 


#### process ####

#### monthly stuff - seasonal peaks ?
discharge.seasons <- discharge.monthly.df %>% 
  mutate(year = year(datetime)) %>% 
  mutate(season = case_when(month == 1 | month == 2 | month == 3 ~ "winter",
                            month == 4 | month == 5 | month == 6 ~ "spring",
                            month == 7 | month == 8 | month == 9 ~ "summer",
                            month == 10 | month == 11 | month == 12 ~ "autumn")
         ) %>% 
  group_by(reservoir.id, year, season) %>% 
  summarise(discharge.km3.season = sum(discharge.km3.month))

seasons.list <- list()

reservoirs.unique <- unique(discharge.seasons$reservoir.id)

for(i in seq(length(reservoirs.unique))){
  
  discharge.select <- discharge.seasons %>% 
    filter(reservoir.id == reservoirs.unique[i])
  
  if(unique(discharge.select$reservoir.id) == 14995 |
     unique(discharge.select$reservoir.id) == 15433 |
     unique(discharge.select$reservoir.id) == 16032 |
     unique(discharge.select$reservoir.id) == 20444 |
     unique(discharge.select$reservoir.id) == 22359){next}
  
  years.select <- unique(discharge.select$year)
  
  peak.list <- list()
  
  for(j in seq(length(years.select))){
    
    discharge.select.season <- discharge.select %>% 
      filter(year == years.select[j])
    
    peak.season <- discharge.select.season$season[
      discharge.select.season$discharge.km3.season == max(discharge.select.season$discharge.km3.season)]
    
    peak.list[[j]] <- peak.season
  
  }
  
  peak.season.df <- data.frame(do.call(rbind, peak.list)) %>% 
    mutate(count=1)
  colnames(peak.season.df) <- c('season', 'cunt')
  
  count.n <- peak.season.df %>% 
    group_by(season) %>% 
    summarise(cunt=sum(cunt))
  
  peeek.season <- count.n$season[count.n$cunt == max(count.n$cunt)][1]
  
  reservoir.peak.df <- data.frame(unique(discharge.select$reservoir.id), peeek.season)
  colnames(reservoir.peak.df) <- c('reservoir.id', 'peak.season')
  
  seasons.list[[i]] <- reservoir.peak.df
  
}

peak.season.all.df <- do.call(rbind, seasons.list)


#### yearly stuff
discharge.summarise <- discharge.yearly.df %>%
  group_by(reservoir.id) %>%
  summarise(discharge.mean = mean(discharge.km3.y.corrected)) %>% 
  inner_join(sections.coord %>% select(reservoir.id, lon, lat)) %>% 
  inner_join(peak.season.all.df)

#### calculate discharge trend for each reservoir 
trends.list <- list()

sections.unique <- unique(discharge.yearly.df$reservoir.id)

for(i in seq(length(sections.unique))){
  
  discharge.select <- discharge.yearly.df %>% 
    filter(reservoir.id == sections.unique[i]) 
  
  discharge.select.idx <- discharge.select %>% 
    mutate(idx = seq(nrow(discharge.select)))
  
  discharge.year.lm <- lm(discharge.km3.y.corrected~idx, data=discharge.select.idx)
  
  lm.tidy <- broom::tidy(discharge.year.lm)
  
  Slope <- lm.tidy$estimate[2]
  p.value <- lm.tidy$p.value[2]
  
  trend.df.sel <- data.frame(unique(discharge.select$reservoir.id), Slope, p.value)
  
  trends.list[[i]] <- trend.df.sel
  
}

trends.df <- do.call(rbind, trends.list) 
row.names(trends.df) <- NULL
colnames(trends.df) <- c('reservoir.id', 'trend', 'p.value')

p.data.map <- discharge.summarise %>% 
  inner_join(trends.df) %>% 
  mutate(trend.sign = case_when(trend > 0 ~ "positive",
                                trend < 0 ~ "negative"))


#### plot #### 
# global
p.map.discharge <- ggplot() +
  geom_map(
    data = wg, map = wg,
    aes(long, lat, map_id = region),
    color = "white", fill= "grey", alpha=0.6
  ) +
  geom_point(data = p.data.map %>% 
               mutate(peak.season = factor(peak.season, levels = c('winter','spring','summer','autumn')),
                      trend.sign = factor(trend.sign, levels = c('positive','negative'))),
             aes(
               x = lon, y = lat,
               fill=peak.season,
               size=discharge.mean,
               shape = trend.sign
             ),
             alpha = 0.9, stroke = 0.3)+
  ylim(-55,70) +
  ggtitle('\nB. Reservoir discharge (1979-2023)') +
  theme_map() +
  scale_shape_manual(values = c("positive" = 24, "negative" = 25),
                     name = 'Trend',
                     labels = c('Positive','Negative')) +
  scale_size_continuous(name = 'Mean discharge (km<sup>3</sup> y<sup>-1</sup>)') +
  scale_fill_manual(values = palette.map, name = 'Peak season',
                    labels = c('JFM','AMJ','JAS','OND')
                    ) +
  theme(legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.spacing= unit(1.5, 'cm'),
        legend.title = element_markdown(hjust=0.5, size=14),
        legend.title.position = 'top',
        legend.text = element_text(size=13),
        plot.title = element_text(hjust = 0.5, size=16)) +
  guides(fill=guide_legend(ncol = 2, override.aes = list(size = 3, shape = 22)),
    shape = guide_legend(override.aes = list(size = 3)),
    size = guide_legend(nrow=1))

