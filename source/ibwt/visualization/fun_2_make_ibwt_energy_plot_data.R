#energy production and consumption per transfer
ibwt.names.production <- unique(production.yearly$transfer.name)
ibwt.names.consumption <- unique(consumption.yearly$transfer.name)

ibwts.list.production <- list()
ibwts.list.consumption <- list()

#### production
for(i in seq(length(ibwt.names.production))){
  
  ibwt.sel <- ibwt.names.production[i]
  
  ibwt.info.sel <- ibwt.info %>% 
    filter(transfer.name == ibwt.sel)
  
  ibwt.from.lon <- ibwt.info.sel$from.lon[1]
  ibwt.from.lat <- ibwt.info.sel$from.lat[1]
  ibwt.to.lon <- ibwt.info.sel$to.lon[nrow(ibwt.info.sel)]
  ibwt.to.lat <- ibwt.info.sel$to.lat[nrow(ibwt.info.sel)]
  
  country <- unique(ibwt.info.sel$Country)
  
  ibwt.coords <- data.frame(country, 
                            ibwt.sel,
                            ibwt.from.lon,
                            ibwt.from.lat,
                            ibwt.to.lon,
                            ibwt.to.lat)
  
  ibwts.list.production[[i]] <- ibwt.coords
  
}

ibwt.coords.plot.production <- do.call(rbind, ibwts.list.production) 

#### consumption
for(i in seq(length(ibwt.names.consumption))){
  
  ibwt.sel <- ibwt.names.consumption[i]
  
  ibwt.info.sel <- ibwt.info %>% 
    filter(transfer.name == ibwt.sel)
  
  ibwt.from.lon <- ibwt.info.sel$from.lon[1]
  ibwt.from.lat <- ibwt.info.sel$from.lat[1]
  ibwt.to.lon <- ibwt.info.sel$to.lon[nrow(ibwt.info.sel)]
  ibwt.to.lat <- ibwt.info.sel$to.lat[nrow(ibwt.info.sel)]
  
  country <- unique(ibwt.info.sel$Country)
  
  ibwt.coords <- data.frame(country, 
                            ibwt.sel,
                            ibwt.from.lon,
                            ibwt.from.lat,
                            ibwt.to.lon,
                            ibwt.to.lat)
  
  ibwts.list.consumption[[i]] <- ibwt.coords
  
}

ibwt.coords.plot.consumption <- do.call(rbind, ibwts.list.consumption) 

#make dataframe for plotting
#### production
ibwt.segments.summarise.production <- production.yearly %>%
  group_by(transfer.name, datetime) %>%
  summarise(energy.twh.low = sum(energy.twh.low, na.rm=T),
            energy.twh.mean = sum(energy.twh.mean, na.rm=T),
            energy.twh.high = sum(energy.twh.high, na.rm=T),
            energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = mean(energy.m3.kwh.high, na.rm=T),
            energy.m3.kwh.km.low = mean(energy.m3.kwh.km.low, na.rm=T),
            energy.m3.kwh.km.mean = mean(energy.m3.kwh.km.mean, na.rm=T),
            energy.m3.kwh.km.high = mean(energy.m3.kwh.km.high, na.rm=T)
  ) %>% 
  group_by(transfer.name) %>% 
  summarise(energy.twh.low = mean(energy.twh.low, na.rm=T),
            energy.twh.mean = mean(energy.twh.mean, na.rm=T),
            energy.twh.high = mean(energy.twh.high, na.rm=T),
            energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = mean(energy.m3.kwh.high, na.rm=T),
            energy.m3.kwh.km.low = mean(energy.m3.kwh.km.low, na.rm=T),
            energy.m3.kwh.km.mean = mean(energy.m3.kwh.km.mean, na.rm=T),
            energy.m3.kwh.km.high = mean(energy.m3.kwh.km.high, na.rm=T))

ibwt.plot.df.production <- ibwt.coords.plot.production %>% 
  mutate(lon.center = (ibwt.from.lon + ibwt.to.lon)/2,
         lat.center = (ibwt.from.lat + ibwt.to.lat)/2) %>% 
  rename(transfer.name = ibwt.sel) %>%
  inner_join(ibwt.segments.summarise.production)


#### consumption
ibwt.segments.summarise.consumption <- consumption.yearly %>%
  group_by(transfer.name, datetime) %>%
  summarise(energy.twh.low = sum(energy.twh.low, na.rm=T),
            energy.twh.mean = sum(energy.twh.mean, na.rm=T),
            energy.twh.high = sum(energy.twh.high, na.rm=T),
            energy.kwh.m3.low = mean(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = mean(energy.kwh.m3.high, na.rm=T),
            energy.kwh.m3.km.low = mean(energy.kwh.m3.km.low, na.rm=T),
            energy.kwh.m3.km.mean = mean(energy.kwh.m3.km.mean, na.rm=T),
            energy.kwh.m3.km.high = mean(energy.kwh.m3.km.high, na.rm=T)
  ) %>% 
  group_by(transfer.name) %>% 
  summarise(energy.twh.low = mean(energy.twh.low, na.rm=T),
            energy.twh.mean = mean(energy.twh.mean, na.rm=T),
            energy.twh.high = mean(energy.twh.high, na.rm=T),
            energy.kwh.m3.low = mean(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = mean(energy.kwh.m3.high, na.rm=T),
            energy.kwh.m3.km.low = mean(energy.kwh.m3.km.low, na.rm=T),
            energy.kwh.m3.km.mean = mean(energy.kwh.m3.km.mean, na.rm=T),
            energy.kwh.m3.km.high = mean(energy.kwh.m3.km.high, na.rm=T))

ibwt.plot.df.consumption <- ibwt.coords.plot.consumption %>% 
  mutate(lon.center = (ibwt.from.lon + ibwt.to.lon)/2,
         lat.center = (ibwt.from.lat + ibwt.to.lat)/2) %>% 
  rename(transfer.name = ibwt.sel) %>%
  inner_join(ibwt.segments.summarise.consumption)