rm(list=ls())

#----------- script to calculate energy use of thermal tech desal with uncertainty -----------#
#MSF, MED (MED, MED-MVC, MED-TVC)

library(dplyr)

inputDir <- '../../../../input/wtt/desalination/'
outputDir <- '../../../../output/wtt/model/desalination/0_tech_categories/'

dir.create(outputDir, recursive = T, showWarnings = F)

#### load ####
DesalData <- read.csv(paste0(inputDir, 'DesalData_2019_ID_renamed_countries.csv')) 

#### clean desaldata
desal.filtered <- DesalData %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  filter(!is.na(Capacity..m3.d.)) %>% 
  filter(!is.na(Feedwater)) %>% 
  filter(!is.na(Online.date))

#### filter thermal technologies ####
#msf
MSF <- desal.filtered[grepl("MSF", desal.filtered$Technology), ]

#med
MED <- desal.filtered[grepl("MED", desal.filtered$Technology), ]

MED.Pure <- desal.filtered[grep("MED", desal.filtered$Technology), ] %>% 
  filter(!grepl('MVC', MED.Design)) %>%
  filter(!grepl('TVC', MED.Design))
MED.MVC <- desal.filtered[grep("MED", desal.filtered$Technology), ] %>% 
  filter(grepl('MVC', MED.Design))
MED.TVC <- desal.filtered[grep("MED", desal.filtered$Technology), ] %>% 
  filter(grepl('TVC', MED.Design))

#### thermal energy ####
#msf
MSF.low <- 19.58
MSF.high <- 27.25
MSF.mean <- (MSF.low + MSF.high) / 2

#med
MED.pure.low <- 14.45
MED.pure.high <- 21.35
MED.pure.mean <- (MED.pure.low + MED.pure.high) / 2 

#med-mvc
MED.MVC.low <- 7
MED.MVC.high <- 12
MED.MVC.mean <- (MED.MVC.low + MED.MVC.high) / 2

#med-tvc
MED.TVC.low <- 16.1
MED.TVC.high <- 16.3
MED.TVC.mean <- (MED.TVC.low + MED.TVC.high) / 2


#### assign energy use to each plant ####
#Energy [kwh/day] = Capacity [m3/day] * Specific Energy Use [kWh/m3] 

#msf
MSF.E <- MSF %>%
  mutate(kwh.d.low=Capacity..m3.d.*MSF.low) %>%
  mutate(kwh.d.mean=Capacity..m3.d.*MSF.mean) %>%
  mutate(kwh.d.high=Capacity..m3.d.*MSF.high) %>%
  mutate(tech='MSF')

#med
MED.Pure.E <- MED.Pure %>%
  mutate(kwh.d.low=Capacity..m3.d.* MED.pure.low) %>%
  mutate(kwh.d.mean=Capacity..m3.d.* MED.pure.mean) %>%
  mutate(kwh.d.high=Capacity..m3.d.* MED.pure.high) %>%
  mutate(tech='MED')


#med.mvc
MED.MVC.E <- MED.MVC %>%
  mutate(kwh.d.low=Capacity..m3.d.* MED.MVC.low) %>%
  mutate(kwh.d.mean=Capacity..m3.d.* MED.MVC.mean) %>%
  mutate(kwh.d.high=Capacity..m3.d.* MED.MVC.high) %>%
  mutate(tech='MED')


#med.tvc
MED.TVC.E <- MED.TVC %>%
  mutate(kwh.d.low=Capacity..m3.d.* MED.TVC.low) %>%
  mutate(kwh.d.mean=Capacity..m3.d.* MED.TVC.mean) %>%
  mutate(kwh.d.high=Capacity..m3.d.* MED.TVC.high) %>%
  mutate(tech='MED')


#### rbind and save ####
energy.df.thermal <- rbind(MSF.E, MED.Pure.E, MED.MVC.E, MED.TVC.E)
#order by country
energy.df.thermal <- energy.df.thermal[order(energy.df.thermal$Country),]
#save
write.csv(energy.df.thermal, paste0(outputDir, 'DesalDataEnergy_Thermal.csv'), row.names = F)
