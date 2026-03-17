rm(list=ls())

library(dplyr)
library(lubridate)

inputDir <- '../../../../output/water_abstraction/model/ibwt/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
inputDirEnergy <- paste0(inputDir, '2_energy/')

#### load ####
discharge.yearly <- read.csv(paste0(inputDirDischarge, '1_bias_corrected/0_1_discharge_bias_corrected_yearly.csv'))

energy.production.yearly <- read.csv(paste0(inputDirEnergy, '0_energy_production_yearly.csv'))
energy.consumption.yearly <- read.csv(paste0(inputDirEnergy, '1_energy_consumption_yearly.csv'))
energy.budget.yearly <- read.csv(paste0(inputDirEnergy, '4_energy_budget_yearly.csv'))

energy.countries <- read.csv(paste0(inputDirEnergy, '6_energy_countries_1980_2021.csv'))
energy.global <- read.csv(paste0(inputDirEnergy, '6_energy_global_1980_2021.csv'))

energy.savings <- read.csv(paste0(inputDirEnergy, '2_energy_savings_yearly.csv'))

bypass.infrastructure <- read.csv(paste0(inputDirDischarge, 
                                         '2_infrastructure/2_information_bypass.csv'))


##### abstract 
countries.2021 <- energy.countries %>% 
  filter(datetime == '2021-01-01')

consumption.intensity <- energy.consumption.yearly %>% 
  group_by(country, transfer.name, section.id.unique, segment.id, pumping.lift.m) %>% 
  summarise(energy.kwh.m3.low = mean(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = mean(energy.kwh.m3.high,  na.rm=T)
  )

production.2021 <- energy.production.yearly %>% 
  group_by(datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

consumption.2021 <- energy.consumption.yearly %>% 
  group_by(datetime) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

# check inside production.2021, consumption.2021 and energy.global for national and global ratios in 2021

#### consumption avg per transfer discussion ####
swp.c <- energy.consumption.yearly %>% 
  filter(transfer.name == 'California_State_Water_Project') %>% 
  group_by(datetime, transfer.name) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

swp.p <- energy.production.yearly %>% 
  filter(transfer.name == 'California_State_Water_Project') %>% 
  group_by(datetime, transfer.name) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

avg.transfers.c <- energy.consumption.yearly %>% 
  group_by(transfer.name) %>% 
  summarise(energy.twh.low = mean(energy.twh.low, na.rm=T),
            energy.twh.mean = mean(energy.twh.mean,na.rm=T),
            energy.twh.high = mean(energy.twh.high,na.rm=T))

max.transfers.c <- energy.consumption.yearly %>% 
  group_by(transfer.name) %>% 
  summarise(energy.twh.low = max(energy.twh.low, na.rm=T),
            energy.twh.mean = max(energy.twh.mean,na.rm=T),
            energy.twh.high = max(energy.twh.high,na.rm=T))

avg.transfers.c.gwh <- avg.transfers.c %>%  
  mutate(gwh.low = energy.twh.low * 1000,
         gwh.mean = energy.twh.mean * 1000,
         gwh.high = energy.twh.high * 1000,
         ) %>% 
  filter(transfer.name == 'California_State_Water_Project' |
           transfer.name == 'Colorado_River_Aqueduct')


max.transfers.c.gwh <- max.transfers.c %>%  
  mutate(gwh.low = energy.twh.low * 1000,
         gwh.mean = energy.twh.mean * 1000,
         gwh.high = energy.twh.high * 1000,
  ) %>% 
  filter(transfer.name == 'California_State_Water_Project' |
           transfer.name == 'Colorado_River_Aqueduct')

sum(avg.transfers.c.gwh$gwh.low)
sum(avg.transfers.c.gwh$gwh.mean)
sum(avg.transfers.c.gwh$gwh.high)

sum(max.transfers.c.gwh$gwh.low)
sum(max.transfers.c.gwh$gwh.mean)
sum(max.transfers.c.gwh$gwh.high)

#### global fig. 4 ####
energy.global.first10 <- energy.global[1:10,]
energy.global.last10 <- energy.global[33:42,]



mean(energy.global$ratio.electricity.low.p)
mean(energy.global$ratio.electricity.mean.p)
mean(energy.global$ratio.electricity.high.p)

mean(energy.global.first10$ratio.electricity.low.p)
mean(energy.global.first10$ratio.electricity.mean.p)
mean(energy.global.first10$ratio.electricity.high.p)

mean(energy.global.first10$ratio.electricity.low.c)
mean(energy.global.first10$ratio.electricity.mean.c)
mean(energy.global.first10$ratio.electricity.high.c)

mean(energy.global.last10$ratio.electricity.low.p)
mean(energy.global.last10$ratio.electricity.mean.p)
mean(energy.global.last10$ratio.electricity.high.p)

#### countries fig.4 ###
countries.avg.hydro <- energy.production.yearly %>% 
  group_by(datetime, country) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

energy.countries.stuff <- energy.countries %>% 
  group_by(datetime, Country) %>% 
  summarise(
            ratio.electricity.low.p = mean(ratio.electricity.low.p),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p),
            ratio.electricity.high.p = mean(ratio.electricity.high.p)) %>% 
  filter(Country=='Canada')

canada.first.10 <- energy.countries.stuff[1:10,]
canada.last.10 <- energy.countries.stuff[33:42,]


mean(canada.first.10$ratio.electricity.low.p)
mean(canada.first.10$ratio.electricity.mean.p)
mean(canada.first.10$ratio.electricity.high.p)

mean(canada.last.10$ratio.electricity.low.p)
mean(canada.last.10$ratio.electricity.mean.p)
mean(canada.last.10$ratio.electricity.high.p)

# %>% 
#   group_by(country) %>% 
#   summarise(energy.twh.low = mean(energy.twh.low),
#             energy.twh.mean = mean(energy.twh.mean),
#             energy.twh.high = mean(energy.twh.high))

countries.avg.pump <- energy.consumption.yearly %>% 
  group_by(datetime, country) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high)) %>% 
  group_by(country) %>% 
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high))



#### ####
bypass.stuf <- bypass.infrastructure %>% 
  group_by(bypass.type) %>% 
  summarise(count=n(),
            bypass.length.km = sum(bypass.length.km),
            net.bypass = sum(net.bypass))

energy.savings.summary <- energy.savings %>% 
  group_by(datetime, transfer.name) %>% 
  summarise(energy.twh.low = sum(energy.twh.low),
            energy.twh.mean = sum(energy.twh.mean),
            energy.twh.high = sum(energy.twh.high))

energy.savings.mean <- energy.savings.summary %>% 
  group_by(transfer.name) %>% 
  summarise(energy.twh.low = mean(energy.twh.low),
            energy.twh.mean = mean(energy.twh.mean),
            energy.twh.high = mean(energy.twh.high))


# sum(reservoirs.ibwt$capacity.mw)
# sum(reservoirs.ibwt$head.m)

hydropower.intensity <- energy.production.yearly %>% 
  group_by(transfer.name, section.id.unique) %>% 
  summarise(energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
            energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
            energy.m3.kwh.high = mean(energy.m3.kwh.high,  na.rm=T)
  )
# %>% 
#   group_by(transfer.name) %>% 
#   summarise(energy.m3.kwh.low = mean(energy.m3.kwh.low, na.rm=T),
#             energy.m3.kwh.mean = mean(energy.m3.kwh.mean, na.rm=T),
#             energy.m3.kwh.high = mean(energy.m3.kwh.high,  na.rm=T))

summary(hydropower.intensity$energy.m3.kwh.low)
summary(hydropower.intensity$energy.m3.kwh.mean)
summary(hydropower.intensity$energy.m3.kwh.high)




consumption.intensity <- energy.consumption.yearly %>% 
  group_by(country, transfer.name, section.id.unique, segment.id, pumping.lift.m) %>% 
  summarise(energy.kwh.m3.low = mean(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = mean(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = mean(energy.kwh.m3.high,  na.rm=T)
  )

intensity.c.transfers <- consumption.intensity %>% 
  group_by(transfer.name)  %>% 
  summarise(energy.kwh.m3.low = median(energy.kwh.m3.low, na.rm=T),
            energy.kwh.m3.mean = median(energy.kwh.m3.mean, na.rm=T),
            energy.kwh.m3.high = median(energy.kwh.m3.high,  na.rm=T)
  )
  
mean(intensity.c.transfers$energy.kwh.m3.low)
mean(intensity.c.transfers$energy.kwh.m3.mean)
mean(intensity.c.transfers$energy.kwh.m3.high)

length(unique(consumption.intensity$country))
sum(consumption.intensity$pumping.lift.m)
summary(consumption.intensity$energy.kwh.m3.low)
summary(consumption.intensity$energy.kwh.m3.mean)
summary(consumption.intensity$energy.kwh.m3.high)


transfer.volumes <- discharge.yearly %>% 
  group_by(transfer.name) %>% 
  summarise(discharge.km3.y.corrected = mean(discharge.km3.y.corrected))


##### decades #####
#### summarise ####
summary.xd <- energy.countries %>% 
  group_by(Country) %>% 
  summarise(twh.low.c = mean(twh.low.c),
            twh.mean.c = mean(twh.mean.c),
            twh.high.c = mean(twh.high.c),
            twh.low.p = mean(twh.low.p),
            twh.mean.p = mean(twh.mean.p),
            twh.high.p = mean(twh.high.p),
            ratio.electricity.low.c = mean(ratio.electricity.low.c, na.rm=T),
            ratio.electricity.mean.c = mean(ratio.electricity.mean.c, na.rm=T),
            ratio.electricity.high.c = mean(ratio.electricity.high.c, na.rm=T),
            ratio.electricity.low.p = mean(ratio.electricity.low.p, na.rm=T),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p, na.rm=T),
            ratio.electricity.high.p = mean(ratio.electricity.high.p, na.rm=T)
            
  ) %>% 
  arrange(twh.mean.c)

summary.other <- summary.xd %>% 
  slice(1:7)

sum(summary.other$twh.low.c)
sum(summary.other$twh.mean.c)
sum(summary.other$twh.high.c)

mean(summary.xd$ratio.electricity.low.p)
mean(summary.xd$ratio.electricity.mean.p)
mean(summary.xd$ratio.electricity.high.p)

ratio.mean <- energy.countries %>% 
  group_by(datetime, Country) %>% 
  summarise(ratio.electricity.low.c = mean(ratio.electricity.low.c, na.rm = T),
            ratio.electricity.mean.c = mean(ratio.electricity.mean.c, na.rm = T),
            ratio.electricity.high.c = mean(ratio.electricity.high.c, na.rm = T),
            ratio.electricity.low.p = mean(ratio.electricity.low.p, na.rm = T),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p, na.rm = T),
            ratio.electricity.high.p = mean(ratio.electricity.high.p, na.rm = T),
  )


mean(ratio.mean$ratio.electricity.low.c)
mean(ratio.mean$ratio.electricity.mean.c)
mean(ratio.mean$ratio.electricity.high.c)

mean(ratio.mean$ratio.electricity.low.p)
mean(ratio.mean$ratio.electricity.mean.p)
mean(ratio.mean$ratio.electricity.high.p)

first.10 <- ratio.mean %>% 
  filter(year(datetime) <= 1989) %>% 
  group_by(Country) %>% 
  summarise(ratio.electricity.low.c = mean(ratio.electricity.low.c, na.rm = T),
            ratio.electricity.mean.c = mean(ratio.electricity.mean.c, na.rm = T),
            ratio.electricity.high.c = mean(ratio.electricity.high.c, na.rm = T),
            ratio.electricity.low.p = mean(ratio.electricity.low.p, na.rm = T),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p, na.rm = T),
            ratio.electricity.high.p = mean(ratio.electricity.high.p, na.rm = T),
  )

last.10 <- ratio.mean %>% 
  filter(datetime >= 2012) %>% 
  group_by(Country) %>% 
  summarise(ratio.electricity.low.c = mean(ratio.electricity.low.c, na.rm = T),
            ratio.electricity.mean.c = mean(ratio.electricity.mean.c, na.rm = T),
            ratio.electricity.high.c = mean(ratio.electricity.high.c, na.rm = T),
            ratio.electricity.low.p = mean(ratio.electricity.low.p, na.rm = T),
            ratio.electricity.mean.p = mean(ratio.electricity.mean.p, na.rm = T),
            ratio.electricity.high.p = mean(ratio.electricity.high.p, na.rm = T),
  )

mean(first.10$ratio.electricity.low.c, na.rm=T)
mean(first.10$ratio.electricity.mean.c, na.rm=T)
mean(first.10$ratio.electricity.high.c, na.rm=T)
mean(last.10$ratio.electricity.low.c, na.rm=T)
mean(last.10$ratio.electricity.mean.c, na.rm=T)
mean(last.10$ratio.electricity.high.c, na.rm=T)

mean(first.10$ratio.electricity.low.p, na.rm=T)
mean(first.10$ratio.electricity.mean.p, na.rm=T)
mean(first.10$ratio.electricity.high.p, na.rm=T)
mean(last.10$ratio.electricity.low.p, na.rm=T)
mean(last.10$ratio.electricity.mean.p, na.rm=T)
mean(last.10$ratio.electricity.high.p, na.rm=T)

mean(first.10$ratio.electricity.low.p, na.rm=T)
mean(first.10$ratio.electricity.mean.p, na.rm=T)
mean(first.10$ratio.electricity.high.p, na.rm=T)
mean(last.10$ratio.electricity.low.p, na.rm=T)
mean(last.10$ratio.electricity.mean.p, na.rm=T)
mean(last.10$ratio.electricity.high.p, na.rm=T)


