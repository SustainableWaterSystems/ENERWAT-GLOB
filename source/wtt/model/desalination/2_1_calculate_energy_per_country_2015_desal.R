rm(list=ls())

#### calculate energy use of desal per country in 2015 (benchmark year) ####
library(dplyr)

#### input files ####
inputDirWorld <- '../../../../input/global_data/'
inputDirEnergy <- '../../../../output/wtt/model/desalination/2_energy/'

country.region.data <- read.csv(paste0(inputDirWorld, 
                                       'general/countries_id_regions.csv'))

plants.online <- read.csv(
  paste0(inputDirEnergy, 'energy_plants_2015_desal.csv')) 

#primary energy 
eia.primary.energy <- read.csv(paste0(
  inputDirWorld, 'energy/eia_total_energy_consumption_2015.csv'))

#electricity 
eia.electricity <- read.csv(paste0(
  inputDirWorld, 'energy/eia_electricity_net_consumption_TWh_2015.csv'))

#### country-level energy use #### 
energy.country.use <- plants.online %>%
  group_by(Country) %>%
  summarise(km3.y = sum(Capacity..m3.d. * 365 / (10^9)),
            kwh.y.low = sum(kwh.y.low),
            kwh.y.mean = sum(kwh.y.mean),
            kwh.y.high = sum(kwh.y.high),
            twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high),
            ej.y.low = sum(ej.y.low),
            ej.y.mean = sum(ej.y.mean),
            ej.y.high = sum(ej.y.high)
            
            ) %>%
  
  mutate(kwh.m3.low = kwh.y.low / (km3.y * 10^9),
         kwh.m3.mean = kwh.y.mean / (km3.y * 10^9),
         kwh.m3.high = kwh.y.high / (km3.y * 10^9)

         ) 

#### add world total volumes and data ####
world.sums <- t(as.data.frame(
  colSums(energy.country.use %>% dplyr::select(-Country))))
world.means <- t(as.data.frame(
  colMeans(energy.country.use %>% dplyr::select(-Country))))
world.sums.to.use <- t(as.data.frame(world.sums[, 1:10]))
world.means.to.use <- t(as.data.frame(world.means[, 11:13]))
world.string <- 'World'

world.row <- cbind(world.string, world.sums.to.use, world.means.to.use)

row.names(world.row) <- NULL
colnames(world.row)[1] <- 'Country'

country.energy.model <- rbind(world.row, energy.country.use)

#### save and remove temp (for string to numeric) ####
write.csv(
  country.energy.model, paste0(
    inputDirEnergy, 'energy_countries_2015_desalination.csv'),
  row.names = F)

country.energy.model <- read.csv(paste0(
  inputDirEnergy, 'energy_countries_2015_desalination.csv')) 

#### calculate country and global ratios ####
countries.with.data.primary <- inner_join(country.energy.model, eia.primary.energy)
countries.with.data.electricity <- inner_join(country.energy.model, eia.electricity)

countries.ratios <- inner_join(countries.with.data.primary, 
                               countries.with.data.electricity) %>% 
  mutate(ratio.primary.energy.low = (ej.y.low / X2015.primary.ej) * 100,
         ratio.primary.energy.mean = (ej.y.mean / X2015.primary.ej) * 100,
         ratio.primary.energy.high = (ej.y.high / X2015.primary.ej) * 100,
         ratio.electricity.low = (twh.y.low / X2015.net.electricity.twh) * 100,
         ratio.electricity.mean = (twh.y.mean / X2015.net.electricity.twh) * 100,
         ratio.electricity.high = (twh.y.high / X2015.net.electricity.twh) * 100)


write.csv(countries.ratios, paste0(
  inputDirEnergy, 'energy_countries_2015_desalination.csv'),
          row.names = F)