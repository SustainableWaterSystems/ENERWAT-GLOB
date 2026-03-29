#### the script assigns unassigned gdp to the closest population cell
#case when only gdp to reassign (all population pixels already match with a gdp value)

#get assigned pixels
countries.only.pop.to.assign.df <- first.join.5arcmin.filtered %>% 
  filter(Country %in% setdiff(countries.unassigned.pop, countries.unassigned.both))

countries.loop.pop.only <- unique(countries.only.pop.to.assign.df$Country)

#get unassigned pixels
checksum.unassigned.pop <- population.5arcmin.matching %>%
  filter(Country %in% countries.loop.pop.only) %>%
  filter(!cell_ID %in% countries.only.pop.to.assign.df$cell_ID) 

sum(checksum.unassigned.pop$pop_2015_total)
sum(checksum.unassigned.pop$pop_2015_urban)
sum(checksum.unassigned.pop$pop_2015_rural)

sum(population.5arcmin.matching$pop_2015_total) -
  sum(checksum.unassigned.pop$pop_2015_total)

sum(checksum.unassigned.pop$pop_2015_total) +
  sum(countries.only.pop.to.assign.df$pop_2015_total)

countries.to.remove <- data.frame(countries.loop.pop.only)
colnames(countries.to.remove) <- 'Country'

#### final verdict : keep matched gdp and store info on unmatched population (5) ####
write.csv(checksum.unassigned.pop, paste0(outputDirTemp, '02_unmatched_population.csv'),
          row.names = F)
vroom_write(countries.only.pop.to.assign.df, 
            paste0(outputDirTemp, '02_countries_matching_uncertain_nightlight.csv'), ',')
