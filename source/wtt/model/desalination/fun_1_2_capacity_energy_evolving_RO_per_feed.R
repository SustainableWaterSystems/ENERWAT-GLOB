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
