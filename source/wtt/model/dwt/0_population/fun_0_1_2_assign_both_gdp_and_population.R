#### case when both can be unmatched in a country
# first assign gdp to closest pixel (out of all pixels)
# then decide a threshold distance (50 km)
# then move or remove remaining population

#matched pixels
countries.both.to.assign.df <- first.join.5arcmin.filtered %>% 
  filter(Country %in% countries.unassigned.both)

#countries
countries.loop.both <- unique(countries.both.to.assign.df$Country)

# get unmatched gdp and population
unassigned.gdp.when.both <- gdp.5arcmin.matching %>%
  filter(Country %in% countries.loop.both) %>%
  filter(!cell_ID %in% countries.both.to.assign.df$cell_ID)

unassigned.pop.when.both <- population.5arcmin.matching %>%
  filter(Country %in% countries.loop.both) %>%
  filter(!cell_ID %in% countries.both.to.assign.df$cell_ID) 

# get full dataframes of these countries
both.full.df.gdp <- gdp.5arcmin.matching %>%
  filter(Country %in% countries.loop.both)

both.full.df.pop <- population.5arcmin.matching %>% 
  filter(Country %in% countries.loop.both)


#### processing per country ####
countries.both.assigned.list <- list()
pop.to.remove.list <- list()
gdp.to.remove.list <- list()


#### BOTTLENECK ####
for(idx in seq(1, length(countries.loop.both))){

  replaceMessage(paste0('Country ', idx, '/', length(countries.loop.both),
                        ': ', countries.loop.both[idx]))
  
  #### 1. get country dataframes (GDP AND POPULATION) ####
  country.gdp <- both.full.df.gdp %>% 
    filter(Country == countries.loop.both[idx]) %>% 
    filter(GDP_total_2015 != 0) %>% 
    filter(GDP_total_2015 != -9999)
  
  country.pop <- both.full.df.pop %>% 
    filter(Country == countries.loop.both[idx]) %>% 
    filter(pop_2015_total != 0) %>% 
    filter(pop_2015_total != -9999)
  
  unassigned.gdp.country <- unassigned.gdp.when.both %>% 
    filter(Country == countries.loop.both[idx])
  
  sum(unassigned.gdp.country$GDP_total_2015)
  
   
  #### 2. get coordinates of gdp (source) and population pixels (sink) ####
  cor_source_gdp <- as.data.frame(cbind(

    country.gdp$lon,
    country.gdp$lat

  ))

  cor_sink_pop <- as.data.frame(cbind(

    country.pop$lon,
    country.pop$lat

  ))
  
  #find nearest neighbour (plant)
  matches <- knnx.index(cor_sink_pop, cor_source_gdp, k = 1)

  #find nearest neighbour cell ID
  cell.id.sink <- as.data.frame(
    country.pop$cell_ID[matches[,1]])
  colnames(cell.id.sink) <- 'cell_ID'

  #make dataframe of source and sink cell ID 
  cell.id.source <- country.gdp %>%
    select(cell_ID)
  
  source.sink.ids <- data.frame(cell.id.source$cell_ID, cell.id.sink$cell_ID)
  colnames(source.sink.ids) <- c('cell_ID_source', 'cell_ID_sink')
  
  
  #get lat lon of source and sink points
  # use countries raster table to be consistent with lon lat
  latlon.source <- inner_join(cell.id.source, countries.raster.5arcmin %>% 
                                select(cell_ID, cell_lon, cell_lat))
  colnames(latlon.source) <- c('cell_ID_source', 'lon_source', 'lat_source')
  
  latlon.sink <- inner_join(cell.id.sink, countries.raster.5arcmin %>% 
                              select(cell_ID, cell_lon, cell_lat))
  colnames(latlon.sink) <- c('cell_ID_sink', 'lon_sink', 'lat_sink')
  
  #make dataframe of lat lon source sink
  latlon.source.sink <- as.data.frame(
    cbind(latlon.source, latlon.sink))
  
  #column names
  colnames(latlon.source.sink) <- c('cell_ID_source', 'lon_source', 'lat_source',
                                    'cell_ID_sink', 'lon_sink', 'lat_sink')
  
  #### 3. calculate distance between sources and sinks ####
  # package geosphere outputs distance in meters
  distance.list <- list()
  
  for(coord in seq(1, nrow(latlon.source.sink))){
    
    replaceMessage(paste0(coord, ' / ', nrow(latlon.source.sink)))
    
    distance <- as.data.frame(
      distm(latlon.source.sink[coord ,c('lon_source','lat_source')],
            latlon.source.sink[coord ,c('lon_sink','lat_sink')],
            fun = distHaversine))
    
    distance.list[[coord]] <- distance
    
  }
  
  #maybe store distance information
  distance.m <- do.call(rbind, distance.list)
  distance.km <- distance.m / 1000
  colnames(distance.km) <- 'ss_distance'
  
  distance.country.df <- cbind(latlon.source.sink, distance.km)
  
  #save as backup to not redo loop everytime o:)
  vroom_write(distance.country.df, 
              paste0(outputDirTemp, 'distance_when_both_unmatched_',
                     countries.loop.both[idx], '.csv'), ',')
  
  #### 4. final step: get (un)matched gdp and population and save####
  gdp.country.unassigned <- cbind(latlon.source.sink, distance.km) %>% 
    filter(ss_distance > 50) %>%
    inner_join(country.gdp, by = c("cell_ID_source" = "cell_ID"))
  
  gdp.country.assigned <- cbind(latlon.source.sink, distance.km) %>% 
    filter(ss_distance <= 50) %>%
    inner_join(country.gdp, by = c("cell_ID_source" = "cell_ID"))
  
  gdp.country.final <- gdp.country.assigned %>% 
    group_by(cell_ID_sink) %>% 
    summarise(GDP_total_2015 = sum(GDP_total_2015)) %>% 
    rename(cell_ID = cell_ID_sink)
  
  country.matched.pop.gdp <- country.pop %>% 
    inner_join(., gdp.country.final)
  
  pop.to.remove <- country.pop %>% 
    filter(cell_ID 
           %in% setdiff(country.pop$cell_ID, country.matched.pop.gdp$cell_ID))
  
  countries.both.assigned.list[[idx]] <- country.matched.pop.gdp
  pop.to.remove.list[[idx]] <- pop.to.remove
  gdp.to.remove.list[[idx]] <- gdp.country.unassigned
  
}

countries.both.assigned.df <- do.call(rbind, countries.both.assigned.list)
pop.to.remove <- do.call(rbind, pop.to.remove.list)
gdp.to.remove <- do.call(rbind, gdp.to.remove.list)

vroom_write(countries.both.assigned.df, 
            paste0(outputDirTemp, '03_countries_matching_with_removals.csv'),
            ',')
vroom_write(pop.to.remove, 
            paste0(outputDirTemp, '03_population_removed.csv'),
            ',')
vroom_write(gdp.to.remove, 
            paste0(outputDirTemp, '03_gdp_removed.csv'),
            ',')


