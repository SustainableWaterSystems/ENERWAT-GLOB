rm(list=ls())

library(dplyr)
library(vroom)

inputDir   <- '../../../../output/wtt/model/desalination/0_tech_categories/'
outputDir <- '../../../../output/wtt/model/desalination/1_cumulative/'

dir.create(outputDir, recursive = T, showWarnings = F)

#### load dataframes ####

desalData.energy.thermal <- read.csv(paste0(inputDir,'DesalDataEnergy_Thermal.csv')) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>%
  filter(!grepl('Construction', Plant.status)) %>% 
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status))

desalData.energy.ED <- read.csv(paste0(inputDir,'DesalDataEnergy_ED.csv')) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>%
  filter(!grepl('Construction', Plant.status)) %>%  
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status))

desalData.energy.RO <- vroom(
  paste0(inputDir,'DesalDataEnergy_RO_time_evolving.csv'), show_col_types = F) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  mutate(year.exponential=as.Date(paste(year.exponential, 1, 1, sep = "-"))) %>% 
  filter(!grepl('Construction', Plant.status)) %>%  
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status)) 

desalData.energy.RO.current <- desalData.energy.RO %>% 
  filter(year.exponential == max(desalData.energy.RO$Online.date)) %>% 
  dplyr::select(-year.exponential)

desal.thermal.ro.ed <- rbind(desalData.energy.thermal, 
                             desalData.energy.RO.current, 
                             desalData.energy.ED)


#### filter by tech ####
#msf
MSF <- desalData.energy.thermal[grepl("MSF", desalData.energy.thermal$tech), ]

#med
MED.all <- desalData.energy.thermal[grepl("MED", desalData.energy.thermal$tech), ]

#ro
RO <- desalData.energy.RO.current

#ed
ED <- desalData.energy.ED


#### calculate cumulative capacities (m3.d) ####

#calculate cumulative capacity
cum.capacity.all.tech.frame <- desal.thermal.ro.ed %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='Total')

#create vector with the whole timeframe of DesalData
years_range_desalData <- as.data.frame(
  as.Date(
    seq.Date(
      cum.capacity.all.tech.frame$Online.date[1],
      cum.capacity.all.tech.frame$Online.date[nrow(cum.capacity.all.tech.frame)],
      by='year'))) %>% dplyr::rename(Online.date=1)

cum.capacity.all.tech.frame <- cum.capacity.all.tech.frame %>%
  merge(years_range_desalData, ., all=T)
cum.capacity.all.tech.frame$m3.d[2] <- 0
cum.capacity.all.tech.frame$cum.m3.d[2] <- cum.capacity.all.tech.frame$cum.m3.d[1]
cum.capacity.all.tech.frame$cum.m3.y[2] <- cum.capacity.all.tech.frame$cum.m3.y[1]
cum.capacity.all.tech.frame$tech[2] <- cum.capacity.all.tech.frame$tech[1]

#calculate capacities of indibidual technologies
MSF.C <- MSF %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='MSF')

MED.all.C <- MED.all %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='MED (all)')

RO.C <- RO %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='RO')

ED.C <- ED %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(tech='ED')

#### calculate cumulative energy consumption (kwh.y) ####
MSF.E <- MSF %>% 
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low)) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean)) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high)) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='MSF')

MED.all.E <- MED.all %>% 
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low)) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean)) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high)) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='MED (all)')

RO.E <- desalData.energy.RO %>% 
  group_by(year.exponential) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData %>%  
          rename(year.exponential = Online.date), ., all=T) %>% 
  filter(year.exponential <= max(desalData.energy.RO$Online.date)) %>%
  replace(is.na(.),0) %>%
  
  #### it's not cumulative here because it's already cumulative in the previous dataset
  #### every year plants from the previous year remain but with lower energy use
  #### this is also why the final energy use is lower in 2019 than 2018
  mutate(cum.kwh.d.low=kwh.d.low) %>%
  mutate(cum.kwh.d.mean=kwh.d.mean) %>%
  mutate(cum.kwh.d.high=kwh.d.high) %>%

  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='RO') %>% 
  rename(Online.date = year.exponential)

ED.E <- ED %>% 
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low)) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean)) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high)) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(tech='ED')

#years
cum.energy.years <- years_range_desalData

#kwh.d
cum.all.tech.kwh.d.low <- MSF.E$kwh.d.low + MED.all.E$kwh.d.low + 
  RO.E$kwh.d.low + ED.E$kwh.d.low
cum.all.tech.kwh.d.mean <- MSF.E$kwh.d.mean + MED.all.E$kwh.d.mean + 
  RO.E$kwh.d.mean + ED.E$kwh.d.mean
cum.all.tech.kwh.d.high <- MSF.E$kwh.d.high + MED.all.E$kwh.d.high + 
  RO.E$kwh.d.high + ED.E$kwh.d.high

#cum kwh.d
cum.all.tech.cum.kwh.d.low <- MSF.E$cum.kwh.d.low + MED.all.E$cum.kwh.d.low + 
  RO.E$cum.kwh.d.low + ED.E$cum.kwh.d.low 
cum.all.tech.cum.kwh.d.mean <- MSF.E$cum.kwh.d.mean + MED.all.E$cum.kwh.d.mean + 
  RO.E$cum.kwh.d.mean + ED.E$cum.kwh.d.mean
cum.all.tech.cum.kwh.d.high <- MSF.E$cum.kwh.d.high + MED.all.E$cum.kwh.d.high + 
  RO.E$cum.kwh.d.high + ED.E$cum.kwh.d.high

#total energy
cum.energy.all.tech.low <- MSF.E$cum.kwh.y.low + MED.all.E$cum.kwh.y.low + 
  RO.E$cum.kwh.y.low + ED.E$cum.kwh.y.low 
cum.energy.all.tech.mean <- MSF.E$cum.kwh.y.mean + MED.all.E$cum.kwh.y.mean + 
  RO.E$cum.kwh.y.mean + ED.E$cum.kwh.y.mean 
cum.energy.all.tech.high <- MSF.E$cum.kwh.y.high + MED.all.E$cum.kwh.y.high + 
  RO.E$cum.kwh.y.high + ED.E$cum.kwh.y.high

cum.energy.all.tech.frame <- data.frame(
  cum.energy.years,
  cum.all.tech.kwh.d.low, cum.all.tech.kwh.d.mean, cum.all.tech.kwh.d.high,
  cum.all.tech.cum.kwh.d.low, cum.all.tech.cum.kwh.d.mean, cum.all.tech.cum.kwh.d.high,
  cum.energy.all.tech.low, cum.energy.all.tech.mean, cum.energy.all.tech.high) %>% 
  mutate(tech='Total')

colnames(cum.energy.all.tech.frame) <- colnames(MSF.E)

#### make dataframes for plotting ####
#### capacity 
cum.capacity.all.tech <- MSF.C$cum.m3.d + MED.all.C$cum.m3.d +
  RO.C$cum.m3.d + ED.C$cum.m3.d

capacity.plot.data <- rbind(cum.capacity.all.tech.frame %>% 
                              mutate(ratio.capacity=cum.m3.d/cum.capacity.all.tech),  
                            MSF.C %>% 
                              mutate(ratio.capacity=cum.m3.d/cum.capacity.all.tech), 
                            MED.all.C %>% 
                              mutate(ratio.capacity=cum.m3.d/cum.capacity.all.tech),
                            RO.C %>% 
                              mutate(ratio.capacity=cum.m3.d/cum.capacity.all.tech), 
                            ED.C %>% 
                              mutate(ratio.capacity=cum.m3.d/cum.capacity.all.tech)) %>% 
  mutate(cum.million.m3.d = cum.m3.d / 10^6) %>% 
  mutate(tech = factor(tech, levels=c('Total', 'MSF', 'MED (all)', 
                                      'RO','ED', 'other'))) 


#### energy
energy.plot.data <- rbind(cum.energy.all.tech.frame %>% 
                            mutate(
                              ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          MSF.E %>% 
                            mutate(
                              ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          MED.all.E %>% 
                            mutate(
                              ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          RO.E %>% 
                            mutate(
                              ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean),
                          ED.E %>% 
                            mutate(
                              ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.tech.mean)) %>%
  mutate(cum.twh.y.low = cum.kwh.y.low / 10^9) %>% 
  mutate(cum.twh.y.mean = cum.kwh.y.mean/ 10^9) %>% 
  mutate(cum.twh.y.high = cum.kwh.y.high / 10^9) %>% 
  mutate(tech = factor(tech, levels=c('Total',  'MSF', 'MED (all)', 'RO', 'ED')))


#### save ####
#tables tech
capacity.table <- capacity.plot.data %>%
  mutate(Online.date = format(as.Date(Online.date, format="%d/%m/%Y"),"%Y")) %>% 
  relocate(ratio.capacity, .after = cum.million.m3.d) %>% 
  relocate(tech, .after = ratio.capacity)

energy.table <- energy.plot.data %>%
  mutate(Online.date = format(as.Date(Online.date, format="%d/%m/%Y"),"%Y")) %>% 
  relocate(ratio.energy.mean, .after = cum.twh.y.high) %>% 
  relocate(tech, .after = ratio.energy.mean)

write.csv(capacity.table,
           paste0(outputDir, 'cumulative_capacity_1945_2019_tech.csv'),
          row.names = F)
write.csv(energy.table,
           paste0(outputDir, 'cumulative_energy_1945_2019_tech.csv'),
          row.names = F)

#all online plants
write.csv(desal.thermal.ro.ed, paste0(inputDir, 'DesalDataEnergy_online_2019.csv'), 
          row.names = F)
