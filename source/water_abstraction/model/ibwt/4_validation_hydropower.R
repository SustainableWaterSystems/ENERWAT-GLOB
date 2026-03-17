rm(list=ls())

library(dplyr)
library(lubridate)
library(hydroGOF)

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')

inputDirGlobal <- '../../../../input/global_data/ibwt/Turner2023_1500_hydropower_usa/'

#### load ###

#### modelled hydropower
reservoirs.ibwt <- read.csv(paste0(inputDirDischarge, '2_infrastructure/0_information_hydropower.csv'))

hydropower.year <- read.csv(paste0(inputDirEnergy, '0_energy_production_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

hydropower.month <- read.csv(paste0(inputDirEnergy, '0_energy_production_monthly.csv'))%>% 
  mutate(datetime = as.Date(datetime)) %>% 
  mutate(
    year.continuous = (year(datetime)),
    year.factor = factor(year(datetime)),     # use year to define separate curves
    date = update(datetime, year = 1)  # use a constant year for the x-axis
  )

#### statistics
sum(reservoirs.ibwt$capacity.mw)
sum(reservoirs.ibwt$head.m)

hydropower.intensity <- hydropower.year %>% 
  group_by(transfer.name, section.id.unique) %>% 
  summarise(energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = mean(energy.m3.kwh.high,  na.rm=T)
            )

summary(hydropower.intensity$energy.m3.kwh.low)
summary(hydropower.intensity$energy.m3.kwh.mean)
summary(hydropower.intensity$energy.m3.kwh.high)




### hydropower validation
#### turner - 1,500 hydropower generation USA
turner.shah <- read.csv(paste0(inputDirGlobal, 'RectifHyd_v1.3_with_GloHydroRes_plant_ID_run_till_2022.csv'))

# turner.generation <- read.csv(paste0(inputDirGlobal, 'RectifHyd_v1.3.csv'))
# turner.coordinates <- read.csv(paste0(inputDirGlobal, 'ORNL_EHAHydroPlant_FY2023_rev.csv'), sep=';')
#
# turner.complete <- inner_join(turner.generation,
#                               turner.coordinates %>%
#                                 rename(EIA_ID = EIA_PtID) %>%
#                                 select(EIA_ID, Lat, Lon, Mode, CH_MW, CH_MWh))

#### process ####
reservoirs.ibwt.usa <- reservoirs.ibwt %>%
  filter(Country == 'USA')

hydropower.ibwt.month <- hydropower.month %>%
  filter(section.id.unique %in% reservoirs.ibwt.usa$section.id.unique) %>%
  mutate(energy.mwh.low = energy.kwh.low / 1000,
         energy.mwh.mean = energy.kwh.mean / 1000,
         energy.mwh.high = energy.kwh.high / 1000)

hydropower.ibwt.year <- hydropower.year %>%
  filter(section.id.unique %in% reservoirs.ibwt.usa$section.id.unique) %>%
  mutate(energy.mwh.low = energy.kwh.low / 1000,
         energy.mwh.mean = energy.kwh.mean / 1000,
         energy.mwh.high = energy.kwh.high / 1000)

turner.shah.ibwt <- turner.shah %>%
  filter(glohydrores_plant_id %in% reservoirs.ibwt.usa$id.shah2025)

#### calculate kge per reservoir ####
kge.list.month <- list()
kge.list.year <- list()

for(i in seq(nrow(reservoirs.ibwt.usa))){

  reservoir.select <- reservoirs.ibwt.usa[i,]

  ts.turner.month <- turner.shah %>%
    filter(glohydrores_plant_id == reservoir.select$id.shah2025) %>%
    select(date, generation_value) %>%
    rename(turner.mwh = generation_value) %>%
    mutate(datetime = as.Date(date))

  ts.turner.year <- ts.turner.month %>%
    mutate(datetime = floor_date(datetime, 'year')) %>%
    group_by(datetime) %>%
    summarise(turner.mwh = sum(turner.mwh))

  ts.modelled.month <- hydropower.ibwt.month %>%
    filter(section.id.unique == reservoir.select$section.id.unique) %>%
    select(datetime, energy.mwh.low, energy.mwh.mean, energy.mwh.high)

  ts.modelled.year <- hydropower.ibwt.year %>%
    filter(section.id.unique == reservoir.select$section.id.unique) %>%
    select(datetime, energy.mwh.low, energy.mwh.mean, energy.mwh.high)

  ts.validation.df.month <- inner_join(ts.turner.month, ts.modelled.month)
  ts.validation.df.year <- inner_join(ts.turner.year, ts.modelled.year)

  # nse.low.month <- NSE(ts.validation.df.month$energy.mwh.low, ts.validation.df.month$turner.mwh)
  # nse.mean.month <- NSE(ts.validation.df.month$energy.mwh.mean, ts.validation.df.month$turner.mwh)
  # nse.high.month <- NSE(ts.validation.df.month$energy.mwh.high, ts.validation.df.month$turner.mwh)


  kge.low.month <- KGE(ts.validation.df.month$energy.mwh.low, 
                       ts.validation.df.month$turner.mwh, na.rm = T)
  kge.mean.month <- KGE(ts.validation.df.month$energy.mwh.mean, 
                        ts.validation.df.month$turner.mwh, na.rm = T)
  kge.high.month <- KGE(ts.validation.df.month$energy.mwh.high, 
                        ts.validation.df.month$turner.mwh, na.rm = T)

  # nrmse.low.month <- nrmse(ts.validation.df.month$energy.mwh.low, ts.validation.df.month$turner.mwh)
  # nrmse.mean.month <- nrmse(ts.validation.df.month$energy.mwh.mean, ts.validation.df.month$turner.mwh)
  # nrmse.high.month <- nrmse(ts.validation.df.month$energy.mwh.high, ts.validation.df.month$turner.mwh)

  kge.low.year <- KGE(ts.validation.df.year$energy.mwh.low, 
                      ts.validation.df.year$turner.mwh, na.rm = T)
  kge.mean.year <- KGE(ts.validation.df.year$energy.mwh.mean, 
                       ts.validation.df.year$turner.mwh, na.rm = T)
  kge.high.year <- KGE(ts.validation.df.year$energy.mwh.high, 
                       ts.validation.df.year$turner.mwh, na.rm = T)


  # nse.low.year <- NSE(ts.validation.df.year$energy.mwh.low, ts.validation.df.year$turner.mwh)
  # nse.mean.year <- NSE(ts.validation.df.year$energy.mwh.mean, ts.validation.df.year$turner.mwh)
  # nse.high.year <- NSE(ts.validation.df.year$energy.mwh.high, ts.validation.df.year$turner.mwh)

  # nrmse.low.year <- nrmse(ts.validation.df.month$energy.mwh.low, ts.validation.df.month$turner.mwh)
  # nrmse.mean.year <- nrmse(ts.validation.df.month$energy.mwh.mean, ts.validation.df.month$turner.mwh)
  # nrmse.high.year <- nrmse(ts.validation.df.month$energy.mwh.high, ts.validation.df.month$turner.mwh)

  kge.df.month <- data.frame(reservoir.select$section.id.unique,
                       reservoir.select$id.shah2025,
                       reservoir.select$transfer.name,
                       kge.low.month, kge.mean.month, kge.high.month,
                       # nse.low.month, nse.mean.month, nse.high.month,
                       reservoir.select$plant.type,
                       reservoir.select$capacity.mw)

  kge.df.year <- data.frame(reservoir.select$section.id.unique,
                             reservoir.select$id.shah2025,
                            reservoir.select$transfer.name,
                             kge.low.year, kge.mean.year, kge.high.year,
                            # nse.low.year, nse.mean.year, nse.high.year,
                            reservoir.select$plant.type,
                            reservoir.select$capacity.mw)



  kge.list.month[[i]] <- kge.df.month
  kge.list.year[[i]] <- kge.df.year
}

kge.usa.df.month <- do.call(rbind, kge.list.month)
kge.usa.df.year <- do.call(rbind, kge.list.year)


# kge.usa.df.month.sto <- kge.usa.df.month %>%
#   filter(reservoir.select.plant.type == 'STO')
# 
# kge.usa.df.year.sto <- kge.usa.df.year %>%
#   filter(reservoir.select.plant.type == 'STO')

# summary(kge.usa.df.month.sto$kge.low.month)
# summary(kge.usa.df.month.sto$kge.mean.month)
# summary(kge.usa.df.month.sto$kge.high.month)
# 
# summary(kge.usa.df.year.sto$kge.low.year)
# summary(kge.usa.df.year.sto$kge.mean.year)
# summary(kge.usa.df.year.sto$kge.high.year)
