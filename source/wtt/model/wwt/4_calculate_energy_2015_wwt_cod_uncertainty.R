library(dplyr)
library(vroom)

#### input files ####
inputDirWorld <- '../../../../input/global_data/'
inputDirFlows <- '../../../../output/wtt/model/wwt/0_volumes/3_flows_model_output/'
inputDirEnergyData <- '../../../../input/wtt/wwt/'
outputDir <- '../../../../output/wtt/model/wwt/1_energy/'
dir.create(outputDir, showWarnings = F, recursive = T)

country.region.data <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv'))

#primary energy 
eia.primary.energy <- read.csv(paste0(
  inputDirWorld, 'energy/eia_total_energy_consumption_2015.csv'))

#electricity 
eia.electricity <- read.csv(paste0(
  inputDirWorld, 'energy/eia_electricity_net_consumption_TWh_2015.csv'))

#modelled data
plants.online <- read.csv(
  paste0(inputDirFlows, 'wwtp_modelled_flows.csv')) 

energy.data <- read.csv(
  paste0(inputDirEnergyData, 'Longo2016_secondary_wwt.csv'))

#### generate COD parameter per plant #### 
# start with simple 60 g cod / d capita
plants.online$COD.low <- plants.online$population_5arcmin * 50
plants.online$COD.mean <- plants.online$population_5arcmin * 70
plants.online$COD.high <- plants.online$population_5arcmin * 120

# 120 g cod / d capita for US and Canada
plants.online$COD.low[
  plants.online$Country == 'Canada' | plants.online$Country == 'United States'] <- 
  plants.online$population_5arcmin[
    plants.online$Country == 'Canada' | plants.online$Country == 'United States'] * 90

plants.online$COD.mean[
  plants.online$Country == 'Canada' | plants.online$Country == 'United States'] <- 
  plants.online$population_5arcmin[
    plants.online$Country == 'Canada' | plants.online$Country == 'United States'] * 120

plants.online$COD.high[
  plants.online$Country == 'Canada' | plants.online$Country == 'United States'] <- 
  plants.online$population_5arcmin[
    plants.online$Country == 'Canada' | plants.online$Country == 'United States'] * 160

#### standardize units in plants.online flows to day: volume [m3/d], COD [g/d] ####
plants.online$Average.flowrate..m3.d. <- plants.online$flow_5arcmin * 10^9 / 365 
# plants.online$Influent..COD..g.d.low <- plants.online$COD.low

#set dataframe to work with energy values
plants.online.energy <- plants.online



#### all the following are in kwh/d until further conversion
#### primary wwt ####
# energy use: 4.3 e-5 - 7.1 e-5 kwh/m3 

primary.intensity.low <- 0.000072
primary.intensity.high <- 0.013
primary.intensity.mean <- ( primary.intensity.low + primary.intensity.high ) / 2

plants.online.energy$primary.low <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.low

plants.online.energy$primary.mean <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.mean

plants.online.energy$primary.high <- 
  plants.online.energy$Average.flowrate..m3.d. * primary.intensity.high


#### secondary wwt ####
#multiple linear regression 
#fit mlr using log
mlr.model.ro <- lm(log(Total.electricity.consumption..kWh.d.) ~
                     log(Average.flowrate..m3.d.) + log(Influent..COD..g.d),
                   # log(Influent..COD..g.d) + log(Effluent..COD..g.d),
                   # log(Influent..COD..g.d) + COD..removed..ratio,
                   # COD..removed..g.d,
                   data=energy.data)

summary(mlr.model.ro)

#### predict energy use values for SECONDARY TREATMENT (log!!!) ####
#### and now we have it for 3 COD values and 3 predicted values -> 9 total values
#### uncertainty from mlr AND from COD

predict.wwt.energy.cod.low <- as_tibble(
  predict(mlr.model.ro, plants.online %>% 
            rename(Influent..COD..g.d = COD.low), 
          interval = "confidence", level = 0.95))

predict.wwt.energy.cod.mean <- as_tibble(
  predict(mlr.model.ro, plants.online %>% 
            rename(Influent..COD..g.d = COD.mean), 
          interval = "confidence", level = 0.95))

predict.wwt.energy.cod.high <- as_tibble(
  predict(mlr.model.ro, plants.online %>% 
            rename(Influent..COD..g.d = COD.high), 
          interval = "confidence", level = 0.95))

#bind to plant -> ELEVATING EXP(PREDICTED)
plants.online.energy$secondary.low <- exp(predict.wwt.energy.cod.low$lwr)
plants.online.energy$secondary.mean <- exp(predict.wwt.energy.cod.mean$fit)
plants.online.energy$secondary.high <- exp(predict.wwt.energy.cod.high$upr)


#### advanced wwt - phosphorus and nitrogen removal ####
#nitrogen : 0.4-0.5 kwh/m3
advanced.n.low <- 0.4
advanced.n.high <- 0.5
advanced.n.mean <- ( advanced.n.low + advanced.n.high ) / 2

#phosphorus : 0.06 - 1.6 kWh/m3
advanced.p.low <- 0.06
advanced.p.high <- 1.6
advanced.p.mean <- ( advanced.p.low + advanced.p.high ) / 2

#nitrogen
plants.online.energy$advanced.n.low <- 
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.low
plants.online.energy$advanced.n.mean <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.mean
plants.online.energy$advanced.n.high <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.n.high

#phosphorus
plants.online.energy$advanced.p.low <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.low
plants.online.energy$advanced.p.mean <-
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.mean
plants.online.energy$advanced.p.high <- 
  plants.online.energy$Average.flowrate..m3.d. * advanced.p.high


#### assign energy use depending on level in plants.online ####
#empty arrays
plants.online.energy$kwh.d.low <- NA
plants.online.energy$kwh.d.mean <- NA
plants.online.energy$kwh.d.high <- NA

#primary 
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.low[plants.online.energy$LEVEL == 'Primary']
plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.mean[plants.online.energy$LEVEL == 'Primary']
plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Primary'] <-
  plants.online.energy$primary.high[plants.online.energy$LEVEL == 'Primary']

#secondary
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.low[plants.online.energy$LEVEL == 'Secondary']
plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.mean[plants.online.energy$LEVEL == 'Secondary']
plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Secondary'] <-
  plants.online.energy$secondary.high[plants.online.energy$LEVEL == 'Secondary']

#advanced : add both nitrogen and phosphorus uncertainties
plants.online.energy$kwh.d.low[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.low[plants.online.energy$LEVEL == 'Advanced'] + 
  plants.online.energy$advanced.n.low[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.low[plants.online.energy$LEVEL == 'Advanced']

plants.online.energy$kwh.d.mean[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.mean[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.n.mean[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.mean[plants.online.energy$LEVEL == 'Advanced']

plants.online.energy$kwh.d.high[plants.online.energy$LEVEL == 'Advanced'] <-
  plants.online.energy$secondary.high[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.n.high[plants.online.energy$LEVEL == 'Advanced'] +
  plants.online.energy$advanced.p.high[plants.online.energy$LEVEL == 'Advanced']


#### make kwh/d -> kwh/y ####
plants.online.energy$kwh.y.low <- NA
plants.online.energy$kwh.y.mean <- NA
plants.online.energy$kwh.y.high <- NA

plants.online.energy$kwh.y.low <- plants.online.energy$kwh.d.low * 365
plants.online.energy$kwh.y.mean <- plants.online.energy$kwh.d.mean * 365
plants.online.energy$kwh.y.high <- plants.online.energy$kwh.d.high * 365

plants.online.energy$twh.y.low <- plants.online.energy$kwh.y.low / (10^9)
plants.online.energy$twh.y.mean <- plants.online.energy$kwh.y.mean / (10^9)
plants.online.energy$twh.y.high <- plants.online.energy$kwh.y.high / (10^9)

plants.online.energy$ej.y.low <- plants.online.energy$twh.y.low * 3.6 * 10^(-3)
plants.online.energy$ej.y.mean <- plants.online.energy$twh.y.mean * 3.6 * 10^(-3)
plants.online.energy$ej.y.high <- plants.online.energy$twh.y.high * 3.6 * 10^(-3)


#### country energy use #### 
energy.country.use.levels <- plants.online.energy %>%
  group_by(Country, LEVEL) %>%
  summarise(wwt.km3.y = sum(flow_5arcmin),
            pop.connected.2015 = sum(population_5arcmin),
            COD.kg.y.low = sum(COD.low * 365 / 1000),
            COD.kg.y.mean = sum(COD.mean * 365 / 1000),
            COD.kg.y.high = sum(COD.high * 365 / 1000),
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
  
  mutate(kwh.m3.low = kwh.y.low / (wwt.km3.y * 1000000000),
         kwh.m3.mean = kwh.y.mean / (wwt.km3.y * 1000000000),
         kwh.m3.high = kwh.y.high / (wwt.km3.y * 1000000000),
         kwh.kgCODin.low = kwh.y.low / (COD.kg.y.low),
         kwh.kgCODin.mean = kwh.y.mean / (COD.kg.y.mean),
         kwh.kgCODin.high = kwh.y.high / (COD.kg.y.high)
         
         ) 

energy.country.use.all <- plants.online.energy %>%
  group_by(Country) %>%
  summarise(wwt.km3.y = sum(flow_5arcmin),
            pop.connected.2015 = sum(population_5arcmin),
            COD.kg.y.low = sum(COD.low * 365 / 1000),
            COD.kg.y.mean = sum(COD.mean * 365 / 1000),
            COD.kg.y.high = sum(COD.high * 365 / 1000),
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
  
  mutate(kwh.m3.low = kwh.y.low / (wwt.km3.y * 1000000000),
         kwh.m3.mean = kwh.y.mean / (wwt.km3.y * 1000000000),
         kwh.m3.high = kwh.y.high / (wwt.km3.y * 1000000000),
         kwh.kgCODin.low = kwh.y.low / (COD.kg.y.low),
         kwh.kgCODin.mean = kwh.y.mean / (COD.kg.y.mean),
         kwh.kgCODin.high = kwh.y.high / (COD.kg.y.high)
         
  ) %>% 
  mutate(LEVEL = 'All') %>% relocate(LEVEL, .after='Country')

energy.country.use <- bind_rows(energy.country.use.levels, 
                                energy.country.use.all) %>% 
  arrange(Country)

#### add world total volumes or mean intensities and data ####
#### must filter by level or totals will be wrong
world.sums.all <- t(as.data.frame(
  colSums(energy.country.use  %>% 
            filter(LEVEL == 'All') %>% 
            ungroup() %>% 
            dplyr::select(-c(Country, LEVEL)))))

world.means.all <- t(as.data.frame(
  colMeans(energy.country.use %>% 
            filter(LEVEL == 'All') %>% 
            ungroup() %>% 
            select(-c(Country, LEVEL)))))

world.sums.primary <- t(as.data.frame(
  colSums(energy.country.use %>% 
            filter(LEVEL == 'Primary') %>% 
            ungroup() %>% 
            select(-c(Country, LEVEL)))))

world.means.primary <- t(as.data.frame(
  colMeans(energy.country.use %>% 
             filter(LEVEL == 'Primary') %>% 
             ungroup() %>% 
             select(-c(Country, LEVEL)))))

world.sums.secondary <- t(as.data.frame(
  colSums(energy.country.use %>% 
            filter(LEVEL == 'Secondary') %>% 
            ungroup() %>% 
            select(-c(Country, LEVEL)))))

world.means.secondary <- t(as.data.frame(
  colMeans(energy.country.use %>% 
             filter(LEVEL == 'Secondary') %>% 
             ungroup() %>% 
             select(-c(Country, LEVEL)))))

world.sums.advanced <- t(as.data.frame(
  colSums(energy.country.use %>% 
            filter(LEVEL == 'Advanced') %>% 
            ungroup() %>% 
            select(-c(Country, LEVEL)))))

world.means.advanced <- t(as.data.frame(
  colMeans(energy.country.use %>% 
             filter(LEVEL == 'Advanced') %>% 
             ungroup() %>% 
             select(-c(Country, LEVEL)))))


#### get levels rows and total row for the global totals and means
world.sums.to.use.all <- t(as.data.frame(world.sums.all[, 1:14]))
world.means.to.use.all <- t(as.data.frame(world.means.all[, 15:20]))
group.strings.all <- t(c('World', 'All'))

world.sums.to.use.primary <- t(as.data.frame(world.sums.primary[, 1:14]))
world.means.to.use.primary <- t(as.data.frame(world.means.primary[, 15:20]))
group.strings.primary <- t(c('World', 'Primary'))

world.sums.to.use.secondary <- t(as.data.frame(world.sums.secondary[, 1:14]))
world.means.to.use.secondary <- t(as.data.frame(world.means.secondary[, 15:20]))
group.strings.secondary <- t(c('World', 'Secondary'))

world.sums.to.use.advanced <- t(as.data.frame(world.sums.advanced[, 1:14]))
world.means.to.use.advanced <- t(as.data.frame(world.means.advanced[, 15:20]))
group.strings.advanced <- t(c('World', 'Advanced'))

world.row.all <- cbind(group.strings.all, 
                       world.sums.to.use.all, 
                       world.means.to.use.all)

world.row.primary <- cbind(group.strings.primary, 
                           world.sums.to.use.primary, 
                           world.means.to.use.primary)

world.row.secondary <- cbind(group.strings.secondary, 
                           world.sums.to.use.secondary, 
                           world.means.to.use.secondary)

world.row.advanced <- cbind(group.strings.advanced, 
                           world.sums.to.use.advanced, 
                           world.means.to.use.advanced)

row.names(world.row.all) <- NULL
row.names(world.row.primary) <- NULL
row.names(world.row.secondary) <- NULL
row.names(world.row.advanced) <- NULL

colnames(world.row.all)[1:2] <- c('Country', 'LEVEL')
colnames(world.row.primary)[1:2] <- c('Country', 'LEVEL')
colnames(world.row.secondary)[1:2] <- c('Country', 'LEVEL')
colnames(world.row.advanced)[1:2] <- c('Country', 'LEVEL')


#### bind world rows together with countries
#### now split amongst levels: all, primary, secondary, advanced
#### good for plotting

energy.country.use.world <- data.frame(rbind(world.row.all,
                                  world.row.primary) %>% 
  rbind(., world.row.secondary) %>% 
  rbind(., world.row.advanced)) %>% 
  rbind(., energy.country.use)
  

#### retain kwh.kgCODin only for secondary treatment (nonsensical for other levels)
energy.country.use.world$kwh.kgCODin.low[
  energy.country.use.world$LEVEL == 'All' | 
    energy.country.use.world$LEVEL == 'Primary' | 
    energy.country.use.world$LEVEL == 'Advanced' 
] <- NA
energy.country.use.world$kwh.kgCODin.mean[
  energy.country.use.world$LEVEL == 'All' | 
    energy.country.use.world$LEVEL == 'Primary' | 
    energy.country.use.world$LEVEL == 'Advanced' 
] <- NA
energy.country.use.world$kwh.kgCODin.high[
  energy.country.use.world$LEVEL == 'All' | 
    energy.country.use.world$LEVEL == 'Primary' | 
    energy.country.use.world$LEVEL == 'Advanced' 
] <- NA


#### save plants and countries tables ####
vroom_write(
  plants.online.energy, paste0(outputDir, 'energy_plants_2015_wwt.csv'),
  delim = ',' )
vroom_write(
  energy.country.use.world, paste0(outputDir, 'energy_countries_2015_wwt.csv'),
  delim = ',' )


#### ratios ####
country.energy.model <- read.csv(paste0(outputDir, 'energy_countries_2015_wwt.csv'))

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


write.csv(countries.ratios, paste0(outputDir, 'energy_countries_2015_wwt.csv'),
          row.names = F)