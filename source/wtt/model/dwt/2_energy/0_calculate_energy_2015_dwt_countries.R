rm(list=ls())

library(vroom)
library(dplyr)
library(tidyr)

inputDir <- '../../../../../output/wtt/model/dwt/1_volumes/2_catchments/'
inputDirWorld <- '../../../../../input/global_data/'
outputDir <- '../../../../../output/wtt/model/dwt/2_energy/'
dir.create(outputDir, recursive = T, showWarnings = F)

#modelled catchment-scale conventional drinking water treatment
treatment.rural <- vroom(paste0(inputDir, 'global_treatment_rural.csv'))
treatment.urban <- vroom(paste0(inputDir, 'global_treatment_urban.csv'))

#primary energy 
eia.primary.energy <- read.csv(paste0(
  inputDirWorld, 'energy/eia_total_energy_consumption_2015.csv'))

#electricity 
eia.electricity <- read.csv(paste0(
  inputDirWorld, 'energy/eia_electricity_net_consumption_TWh_2015.csv'))


#### processing ####
treatment.rural.countries <- treatment.rural %>% 
  group_by(Country) %>% 
  summarise(demands.to.SW.rural = sum(demands.to.SW),
            demands.to.GW.rural = sum(demands.to.GW))

treatment.urban.countries <- treatment.urban %>% 
  group_by(Country) %>% 
  summarise(demands.to.SW.urban = sum(demands.to.SW),
            demands.to.GW.urban = sum(demands.to.GW))


treatment.countries <- merge(treatment.rural.countries,
                             treatment.urban.countries, all = T) %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
  mutate(demands.to.SW.total = demands.to.SW.rural + demands.to.SW.urban,
         demands.to.GW.total = demands.to.GW.rural + demands.to.GW.urban) %>% 
  mutate(demands.total = demands.to.SW.total + demands.to.GW.total) %>% 
  filter(demands.total != 0)


#### energy ranges ###

#### aeration
aeration.low <- 0.01
aeration.high <- 0.05
aeration.mean <- (aeration.low + aeration.high) / 2

#### softening 
softening.low <- 0.084
softening.high <- 0.6
softening.mean <- (softening.low + softening.high) / 2

#### coagulation
coagulation.low <- 0.4
coagulation.high <- 0.7
coagulation.mean <- (coagulation.low + coagulation.high) / 2

#### dissolved air flotation
flotation.low <- 0.01
flotation.high <- 0.04
flotation.mean <- ( flotation.low + flotation.high ) / 2

#### sedimentation 
sedimentation.low <- 0.0005
sedimentation.high <- 0.001
sedimentation.mean <- (sedimentation.low + sedimentation.high ) / 2

#### filtration
gravity.filtration.low <- 0.005
gravity.filtration.high <- 0.014
gravity.filtration.mean <- (gravity.filtration.low + gravity.filtration.high) / 2

rapid.filtration.low <- 0.15
rapid.filtration.high <- 0.57
rapid.filtration.mean <- (rapid.filtration.low + rapid.filtration.high) / 2

microfiltration.low <- 0.1
microfiltration.high <- 0.2
microfiltration.mean <- (microfiltration.low + microfiltration.high)/2

# #### rapid mixing
# rapid.mixing.low <- 0.008
# rapid.mixing.high <- 0.022
# rapid.mixing.mean <- (rapid.mixing.low + rapid.mixing.high) /2

#### chlorination
chlorination.low <- 0.00002
chlorination.high <- 0.002
chlorination.mean <- (chlorination.low + chlorination.high)/2

#### ozonation
ozonation.low <- 0.03
ozonation.high <- 0.2
ozonation.mean <- (ozonation.low + ozonation.high)/2

#### UV
uv.low <- 0.01
uv.high <- 0.05
uv.mean <- (uv.low + uv.high)/2


#### energy calculations - treatment classes ####

#### groundwater classes
gw.aerobic.low <- aeration.low + softening.low
gw.aerobic.high <- aeration.high + softening.high

gw.s.anaerobic.low <- aeration.low + gravity.filtration.low + chlorination.low
gw.s.anaerobic.high <- aeration.high + gravity.filtration.high + chlorination.high

gw.deep.anaerobic.low <- gravity.filtration.low + rapid.filtration.low
gw.deep.anaerobic.high <- gravity.filtration.high + rapid.filtration.high

gw.riverbank.low <- aeration.low + gravity.filtration.low +
  aeration.low + rapid.filtration.low + uv.low
gw.riverbank.high <- aeration.high + gravity.filtration.high +
  aeration.high + rapid.filtration.high + uv.high

gw.aerobic.low
gw.aerobic.high

gw.s.anaerobic.low
gw.s.anaerobic.high

gw.deep.anaerobic.low
gw.deep.anaerobic.high

gw.riverbank.low
gw.riverbank.high

#### surface water classes
sw.traditional.low <- chlorination.low + coagulation.low +
  rapid.filtration.low + chlorination.low
sw.traditional.high <- chlorination.high + coagulation.high +
  rapid.filtration.high + chlorination.high

sw.modern.one.low <- chlorination.low + coagulation.low +
  sedimentation.low + aeration.low + chlorination.low 
sw.modern.one.high <- chlorination.high + coagulation.high +
  sedimentation.high + aeration.high + chlorination.high 

sw.modern.two.low <- coagulation.low + sedimentation.low + 
  ozonation.low + rapid.filtration.low + chlorination.low
sw.modern.two.high <- coagulation.high + sedimentation.high + 
  ozonation.high + rapid.filtration.high + chlorination.high

sw.direct.low <- microfiltration.low + coagulation.low + 
  sedimentation.low + rapid.filtration.low + uv.low + microfiltration.low
sw.direct.high <- microfiltration.high + coagulation.high + 
  sedimentation.high + rapid.filtration.high + uv.high + microfiltration.high


sw.traditional.low
sw.traditional.high

sw.modern.one.low 
sw.modern.one.high

sw.modern.two.low
sw.modern.two.high

sw.direct.low
sw.direct.high


#### energy calculations - country level ####
#get min max energy intensities for sw and gw 
sw.minimum <- min(sw.traditional.low,
                  sw.modern.one.low,
                  sw.modern.two.low,
                  sw.direct.low)

sw.maximum <- max(sw.traditional.high,
                  sw.modern.one.high,
                  sw.modern.two.high,
                  sw.direct.high)

sw.mean <- (sw.minimum + sw.maximum) / 2

gw.minimum <- min(gw.aerobic.low,
                  gw.s.anaerobic.low,
                  gw.deep.anaerobic.low,
                  gw.riverbank.low)

gw.maximum <- max(gw.aerobic.high,
                  gw.s.anaerobic.high,
                  gw.deep.anaerobic.high,
                  gw.riverbank.high)

gw.mean <- (gw.minimum + gw.maximum) / 2


sw.minimum
sw.mean
sw.maximum
gw.minimum
gw.mean
gw.maximum

#### calculate country-level (assuming one plant per catchment - max energy consumption)

treatment.countries.energy <- treatment.countries %>% 
  mutate(energy.sw.low = demands.to.SW.total * sw.minimum,
         energy.sw.mean = demands.to.SW.total * sw.mean,
         energy.sw.high = demands.to.SW.total * sw.maximum,
         energy.gw.low = demands.to.GW.total * gw.minimum,
         energy.gw.mean = demands.to.GW.total * gw.mean,
         energy.gw.high = demands.to.GW.total * gw.maximum) %>% 
  mutate(energy.total.low.kwh = energy.sw.low + energy.gw.low,
         energy.total.mean.kwh = energy.sw.mean + energy.gw.mean,
         energy.total.high.kwh = energy.sw.high + energy.gw.high) %>% 
  mutate(energy.sw.low.twh = energy.sw.low / 10^9,
         energy.sw.mean.twh = energy.sw.mean / 10^9,
         energy.sw.high.twh = energy.sw.high / 10^9,
         energy.gw.low.twh = energy.gw.low / 10^9,
         energy.gw.mean.twh = energy.gw.mean / 10^9,
         energy.gw.high.twh = energy.gw.high / 10^9,
         energy.total.low.twh = energy.total.low.kwh / 10^9,
         energy.total.mean.twh = energy.total.mean.kwh / 10^9,
         energy.total.high.twh = energy.total.high.kwh / 10^9) %>% 
  mutate(energy.total.low.ej = energy.total.low.kwh / 10^9 * 3.6 * 10^(-3),
         energy.total.mean.ej = energy.total.mean.kwh / 10^9 * 3.6 * 10^(-3),
         energy.total.high.ej = energy.total.high.kwh / 10^9 * 3.6 * 10^(-3))

#### add world total volumes and data ####
world.sums <- t(as.data.frame(
  colSums(treatment.countries.energy %>% select(-Country))))
world.string <- 'World'
world.row <- cbind(world.string, world.sums)
row.names(world.row) <- NULL
colnames(world.row)[1] <- 'Country'

energy.country.use.world <- rbind(world.row, treatment.countries.energy)

vroom_write(
  energy.country.use.world, paste0(outputDir, 'energy_countries_2015_dwt.csv'),
  delim = ',' )


#### ratios ####
country.energy.model <- read.csv(paste0(outputDir, 'energy_countries_2015_dwt.csv')) 

countries.with.data.primary <- inner_join(country.energy.model, eia.primary.energy)
countries.with.data.electricity <- inner_join(country.energy.model, eia.electricity)

countries.ratios <- inner_join(countries.with.data.primary, 
                               countries.with.data.electricity) %>% 
  mutate(ratio.primary.energy.low = (energy.total.low.ej / X2015.primary.ej) * 100,
         ratio.primary.energy.mean = (energy.total.mean.ej / X2015.primary.ej) * 100,
         ratio.primary.energy.high = (energy.total.high.ej / X2015.primary.ej) * 100,
         ratio.electricity.low = (energy.total.low.twh / X2015.net.electricity.twh) * 100,
         ratio.electricity.mean = (energy.total.mean.twh / X2015.net.electricity.twh) * 100,
         ratio.electricity.high = (energy.total.high.twh / X2015.net.electricity.twh) * 100)


write.csv(countries.ratios, paste0(outputDir, 'energy_countries_2015_dwt.csv'),
          row.names = F)