rm(list=ls())

#----------- script to calculate energy use of electrodialysis desal with uncertainty -----------#

library(dplyr)

#--- functions
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#### load ####
inputDir <- '../../../../input/wtt/desalination/'
outputDir <- '../../../../output/wtt/model/desalination/0_tech_categories/'

DesalData <- read.csv(paste0(inputDir, 'DesalData_2019_ID_renamed_countries.csv')) 

#### clean desaldata
desal.filtered <- DesalData %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  filter(!is.na(Capacity..m3.d.)) %>% 
  filter(!is.na(Feedwater)) %>% 
  filter(!is.na(Online.date))

#### filter ED, exclude ED reversal ####
ED <- desal.filtered[grep("Electrodialysis", desal.filtered$Technology), ] %>% 
  filter(!grepl('reversal', Technology))

#### energy ranges ####
source('fun_0_1_energy_ranges_ED.R')

#### assign minimum energy use ####
ED.Pure.E <- ED %>% filter(grepl('Pure', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * pure.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * pure.mean) %>% 
  mutate(kwh.d.high = Capacity..m3.d. * pure.high) %>%
  mutate(tech='ED')

ED.River.E <- ED %>% filter(grepl('River', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * river.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * river.mean) %>% 
  mutate(kwh.d.high = Capacity..m3.d. * river.high) %>%
  mutate(tech='ED')

ED.Brackish.E <- ED %>% filter(grepl('Brackish', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * brackish.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * brackish.mean) %>% 
  mutate(kwh.d.high = Capacity..m3.d. * brackish.high) %>%
  mutate(tech='ED')

ED.Seawater.E <- ED %>% filter(grepl('Seawater', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * seawater.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * seawater.mean) %>% 
  mutate(kwh.d.high = Capacity..m3.d. * seawater.high) %>%
  mutate(tech='ED')

ED.Brine.E <- ED %>% filter(grepl('Brine', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * brine.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * brine.mean) %>% 
  mutate(kwh.d.high = Capacity..m3.d. * brine.high) %>%
  mutate(tech='ED')

ED.Wastewater.E <- ED %>% filter(grepl('Wastewater', Feedwater)) %>%
  mutate(kwh.d.low = Capacity..m3.d. * wastewater.low) %>%
  mutate(kwh.d.mean = Capacity..m3.d. * wastewater.mean) %>%
  mutate(kwh.d.high = Capacity..m3.d. * wastewater.high) %>%
  mutate(tech='ED')


#### rbind and save ####
energy.df.ED <- rbind(ED.Pure.E, ED.River.E, ED.Brackish.E, 
                      ED.Seawater.E, ED.Brine.E, ED.Wastewater.E)

#order by country
energy.df.ED <- energy.df.ED[order(energy.df.ED$Country),]
#save
write.csv(energy.df.ED, paste0(outputDir, 'DesalDataEnergy_ED.csv'), row.names = F)
