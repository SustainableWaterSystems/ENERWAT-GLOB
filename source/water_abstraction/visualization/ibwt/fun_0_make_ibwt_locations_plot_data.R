####
ibwt.segments.summarise <- ibwt.segments %>% 
  group_by(transfer.name) %>% 
  summarise(transfer.length = sum(segment.length.km)) 

#### gather transfer information ####
ibwts.list <- list()

for(i in seq(length(ibwt.names))){
  
  ibwt.sel <- ibwt.names[i]
  
  ibwt.info.sel <- ibwt.info %>% 
    filter(transfer.name == ibwt.sel)
  
  ibwt.from.lon <- ibwt.info.sel$from.lon[1]
  ibwt.from.lat <- ibwt.info.sel$from.lat[1]
  ibwt.to.lon <- ibwt.info.sel$to.lon[nrow(ibwt.info.sel)]
  ibwt.to.lat <- ibwt.info.sel$to.lat[nrow(ibwt.info.sel)]
  
  country <- unique(ibwt.info.sel$Country)
  
  year.start <- unique(ibwt.info.sel$initial.year.transfer)
  
  n.sections <- ibwt.info.sel$Section[nrow(ibwt.info.sel)]
  
  n.reservoirs <- length(unique(ibwt.info.sel$reservoir.id))
  
  purpose.main <- unique(ibwt.info.sel$Purpose.main)
  purpose.mixed <- unique(ibwt.info.sel$Purpose.mixed)
  
  ibwt.coords <- data.frame(country, 
                            ibwt.sel,
                            year.start,
                            ibwt.from.lon,
                            ibwt.from.lat,
                            ibwt.to.lon,
                            ibwt.to.lat,
                            n.reservoirs,
                            n.sections,
                            purpose.main,
                            purpose.mixed)
  
  ibwts.list[[i]] <- ibwt.coords
  
}

ibwt.coords.plot <- do.call(rbind, ibwts.list) 


#make dataframe for plotting
ibwt.plot.df <- ibwt.coords.plot %>% 
  mutate(lon.center = (ibwt.from.lon + ibwt.to.lon)/2,
         lat.center = (ibwt.from.lat + ibwt.to.lat)/2) %>% 
  rename(transfer.name = ibwt.sel) %>%
  inner_join(ibwt.segments.summarise) %>% 
  mutate(reservoirs.per.km = n.reservoirs / transfer.length) %>% 
  mutate(purpose.mixed = factor(purpose.mixed, levels = c('0','1')))

#### gather initial year per project
ibwt.years <- ibwt.info %>% 
  group_by(transfer.name) %>% 
  summarise(initial.year = mean(initial.year.transfer))

ibwt.info.updated <- inner_join(ibwt.years, ibwt.plot.df)

ibwt.cumulative.stats <- ibwt.info.updated %>% 
  group_by(initial.year) %>% 
  summarise(length.year = sum(transfer.length),
            reservoirs.year = sum(n.reservoirs)) %>% 
  mutate(cumulative.length = cumsum(length.year),
         cumulative.reservoirs = cumsum(reservoirs.year)) 


#### 
ibwt.cumulative.purpose <- ibwt.info.updated %>% 
  group_by(initial.year, purpose.main) %>% 
  summarise(purpose = n()) 

purposes <- unique(ibwt.cumulative.purpose$purpose.main)  

#### nice function to do a cumsum over groups 
pdf = data.frame(initial.year=as.Date(as.character()), group=as.character(), year.value=as.numeric())

for(grp in purposes){
  
  print(grp)
  
  subs = filter(ibwt.cumulative.purpose, purpose.main == grp) %>% arrange(initial.year)
  pdf = rbind(pdf, data.frame(initial.year=subs$initial.year, group=grp, year.value=subs$purpose, cs=cumsum(subs$purpose)))
  
  
}

pdf <- pdf  %>% 
  mutate(group = factor(group, levels=c('Domestic supply', 'Hydropower', 'Irrigation', 'Restoration'))) 