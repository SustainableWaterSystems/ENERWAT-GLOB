palette.subr <- c('#000000','#332288', '#117733', '#44AA99',
                  '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')
palette.subr.only <- c('#332288', '#117733', '#44AA99',
                       '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')

#### load ####
desalData.energy <- read.csv(paste0(inputDir, 'DesalDataEnergy_online_2019.csv')) %>%
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) 

desalData.energy.RO <- vroom(
  paste0(inputDir,'DesalDataEnergy_RO_time_evolving.csv'), show_col_types = F) %>% 
  mutate(Online.date=as.Date(paste(Online.date, 1, 1, sep = "-"))) %>% 
  mutate(year.exponential=as.Date(paste(year.exponential, 1, 1, sep = "-"))) %>% 
  filter(!grepl('Construction', Plant.status)) %>%  
  filter(!grepl('Decommissioned', Plant.status)) %>% 
  filter(!grepl('Mothballed', Plant.status)) 

#### filter by subregion #### 
#Middle East / North Africa
MENA <- desalData.energy[grep("Middle", desalData.energy$Subregion), ]

#Latin America / Caribbean
LAC <- desalData.energy[grep("Latin", desalData.energy$Subregion), ]

#East Asia / Pacific
EAP <- desalData.energy[grep("Pacific", desalData.energy$Subregion), ] 

#North America
NAM <- desalData.energy[grep("North America", desalData.energy$Subregion), ]

#Sub-Saharan Africa
SSA <- desalData.energy[grep("Saharan", desalData.energy$Subregion), ] 

#Eastern Europe / Central Asia
EECA <- desalData.energy[grep("Central", desalData.energy$Subregion), ]  

#Western Europe
WE <- desalData.energy[grep("Western", desalData.energy$Subregion), ]

#Southern Asia
SAS <- desalData.energy[grep("Southern", desalData.energy$Subregion), ]

#### calculate cumulative capacities (m3.d) ####

#calculate cumulative capacity
cum.capacity.all.subregions.frame <- desalData.energy %>% group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='Total')

#create vector with the whole timeframe of DesalData
years_range_desalData <- as.data.frame(
  as.Date(
    seq.Date(
      cum.capacity.all.subregions.frame$Online.date[1],
      cum.capacity.all.subregions.frame$Online.date[nrow(cum.capacity.all.subregions.frame)],
      by='year'))) %>% dplyr::rename(Online.date=1)

#fill gaps in cumulative dataframe
cum.capacity.all.subregions.frame <- cum.capacity.all.subregions.frame %>%
  merge(years_range_desalData, ., all=T)
cum.capacity.all.subregions.frame$m3.d[2] <- 0
cum.capacity.all.subregions.frame$cum.m3.d[2] <- 
  cum.capacity.all.subregions.frame$cum.m3.d[1]
cum.capacity.all.subregions.frame$cum.m3.y[2] <- 
  cum.capacity.all.subregions.frame$cum.m3.y[1]
cum.capacity.all.subregions.frame$subregion[2] <- 
  cum.capacity.all.subregions.frame$subregion[1]

#calculate capacities of indibidual subregions
EAP.C <- EAP %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='EAP')

EECA.C <- EECA %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='EECA')

LAC.C <- LAC %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='LAC')

MENA.C <- MENA %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='MENA')

NAM.C <- NAM %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='NAM')

SAS.C <- SAS %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='SAS')

SSA.C <- SSA %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='SSA')

WE.C <- WE %>% 
  group_by(Online.date) %>%
  summarise(m3.d = sum(Capacity..m3.d.)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.m3.d=cumsum(m3.d)) %>%
  mutate(cum.m3.y=cum.m3.d * 365) %>%
  mutate(subregion='WE')

#checksum
cum.capacity.all.subregions <- EAP.C$cum.m3.d + EECA.C$cum.m3.d +
  LAC.C$cum.m3.d + MENA.C$cum.m3.d + NAM.C$cum.m3.d +
  SAS.C$cum.m3.d + SSA.C$cum.m3.d + WE.C$cum.m3.d



#### calculate energy evolving per subregion for RO ####
#calculate cumulative per subregion
RO.subr.og <- desalData.energy.RO %>%
  group_by(year.exponential, Subregion) %>%
  summarise(kwh.d.low = sum(kwh.d.low),
            kwh.d.mean = sum(kwh.d.mean),
            kwh.d.high = sum(kwh.d.high)) 

RO.subr.og$subregion <- NA
RO.subr.og$subregion[grep("Middle", RO.subr.og$Subregion)] <- 'MENA'
RO.subr.og$subregion[grep("Latin", RO.subr.og$Subregion)] <- 'LAC'
RO.subr.og$subregion[grep("Pacific", RO.subr.og$Subregion)] <- 'EAP'
RO.subr.og$subregion[grep("North America", RO.subr.og$Subregion)] <- 'NAM'
RO.subr.og$subregion[grep("Saharan", RO.subr.og$Subregion)] <- 'SSA'
RO.subr.og$subregion[grep("Central", RO.subr.og$Subregion)] <- 'EECA'
RO.subr.og$subregion[grep("Western", RO.subr.og$Subregion)] <- 'WE'
RO.subr.og$subregion[grep("Southern", RO.subr.og$Subregion)] <- 'SAS'

subrs <- unique(RO.subr.og$subregion)
subrs.cum.list <- list()

for(idx.subr in seq(1, length(subrs))){
  
  replaceMessage(paste0('Subregion: ', idx.subr, '/', length(subrs)))
  
  # idx.subr = 1
  subr <- subrs[idx.subr]
  
  RO.subr <- RO.subr.og %>% 
    filter(subregion == subr)
  
  RO.subr.years <- RO.subr %>%
    merge(years_range_desalData %>%  
            rename(year.exponential = Online.date), ., all=T) %>% 
    filter(year.exponential <= max(desalData.energy.RO$Online.date)) %>% 
    fill(Subregion, .direction = 'up') %>% 
    replace(is.na(.),0) %>%
    mutate(cum.kwh.d.low=kwh.d.low) %>%
    mutate(cum.kwh.d.mean=kwh.d.mean) %>%
    mutate(cum.kwh.d.high=kwh.d.high) %>%
    mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
    mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
    mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>% 
    mutate(subregion = subr) %>% 
    rename(Online.date = year.exponential)
  
  subrs.cum.list[[idx.subr]] <- RO.subr.years
  
}

subrs.cum <- do.call(rbind, subrs.cum.list)


MENA.E.RO <- subrs.cum %>% 
  filter(subregion == 'MENA')
SSA.E.RO <- subrs.cum %>% 
  filter(subregion == 'SSA')
WE.E.RO <- subrs.cum %>% 
  filter(subregion == 'WE')
EAP.E.RO <- subrs.cum %>% 
  filter(subregion == 'EAP')
NAM.E.RO <- subrs.cum %>% 
  filter(subregion == 'NAM')
LAC.E.RO <- subrs.cum %>% 
  filter(subregion == 'LAC')
EECA.E.RO <- subrs.cum %>% 
  filter(subregion == 'EECA')
SAS.E.RO <- subrs.cum %>% 
  filter(subregion == 'SAS')

#### calculate cumulative energy consumption (kwh.y) ####
EAP.E <- EAP %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + EAP.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + EAP.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + EAP.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='EAP')

EECA.E <- EECA %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + EECA.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + EECA.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + EECA.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='EECA')

LAC.E <- LAC %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>% 
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + LAC.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + LAC.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + LAC.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='LAC')

MENA.E <- MENA %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + MENA.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + MENA.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + MENA.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='MENA')

NAM.E <- NAM %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + NAM.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + NAM.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + NAM.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='NAM')

SAS.E <- SAS %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>% 
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + SAS.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + SAS.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + SAS.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='SAS')

SSA.E <- SSA %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + SSA.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + SSA.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + SSA.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='SSA')

WE.E <- WE %>% 
  filter(tech != 'RO') %>%
  group_by(Online.date) %>%
  summarise(kwh.d.low = sum(kwh.d.low), 
            kwh.d.mean = sum(kwh.d.mean), 
            kwh.d.high = sum(kwh.d.high)) %>%
  merge(years_range_desalData, ., all=T) %>% 
  replace(is.na(.),0) %>%
  mutate(cum.kwh.d.low=cumsum(kwh.d.low) + WE.E.RO$cum.kwh.d.low) %>%
  mutate(cum.kwh.d.mean=cumsum(kwh.d.mean) + WE.E.RO$cum.kwh.d.mean) %>%
  mutate(cum.kwh.d.high=cumsum(kwh.d.high) + WE.E.RO$cum.kwh.d.high) %>%
  mutate(cum.kwh.y.low=cum.kwh.d.low * 365) %>%
  mutate(cum.kwh.y.mean=cum.kwh.d.mean * 365) %>%
  mutate(cum.kwh.y.high=cum.kwh.d.high * 365) %>%
  mutate(subregion='WE')


#years vector
cum.energy.years <- cum.capacity.all.subregions.frame$Online.date

#kwh/d
cum.all.regions.kwh.d.low <- EAP.E$kwh.d.low + EECA.E$kwh.d.low +
  LAC.E$kwh.d.low + MENA.E$kwh.d.low + NAM.E$kwh.d.low +
  SAS.E$kwh.d.low + SSA.E$kwh.d.low + WE.E$kwh.d.low

cum.all.regions.kwh.d.high <- EAP.E$kwh.d.high + EECA.E$kwh.d.high +
  LAC.E$kwh.d.high + MENA.E$kwh.d.high + NAM.E$kwh.d.high +
  SAS.E$kwh.d.high + SSA.E$kwh.d.high + WE.E$kwh.d.high

cum.all.regions.kwh.d.mean <- EAP.E$kwh.d.mean + EECA.E$kwh.d.mean +
  LAC.E$kwh.d.mean + MENA.E$kwh.d.mean + NAM.E$kwh.d.mean +
  SAS.E$kwh.d.mean + SSA.E$kwh.d.mean + WE.E$kwh.d.mean

#cumulative kwh/d
cum.all.regions.cum.kwh.d.low <-  EAP.E$cum.kwh.d.low + EECA.E$cum.kwh.d.low +
  LAC.E$cum.kwh.d.low + MENA.E$cum.kwh.d.low + NAM.E$cum.kwh.d.low +
  SAS.E$cum.kwh.d.low + SSA.E$cum.kwh.d.low + WE.E$cum.kwh.d.low

cum.all.regions.cum.kwh.d.high <-  EAP.E$cum.kwh.d.high + EECA.E$cum.kwh.d.high +
  LAC.E$cum.kwh.d.high + MENA.E$cum.kwh.d.high + NAM.E$cum.kwh.d.high +
  SAS.E$cum.kwh.d.high + SSA.E$cum.kwh.d.high + WE.E$cum.kwh.d.high

cum.all.regions.cum.kwh.d.mean <-  EAP.E$cum.kwh.d.mean + EECA.E$cum.kwh.d.mean +
  LAC.E$cum.kwh.d.mean + MENA.E$cum.kwh.d.mean + NAM.E$cum.kwh.d.mean +
  SAS.E$cum.kwh.d.mean + SSA.E$cum.kwh.d.mean + WE.E$cum.kwh.d.mean

#cumulative kwh/y
cum.energy.all.regions.low <- EAP.E$cum.kwh.y.low + EECA.E$cum.kwh.y.low +
  LAC.E$cum.kwh.y.low + MENA.E$cum.kwh.y.low + NAM.E$cum.kwh.y.low +
  SAS.E$cum.kwh.y.low + SSA.E$cum.kwh.y.low + WE.E$cum.kwh.y.low

cum.energy.all.regions.high <- EAP.E$cum.kwh.y.high + EECA.E$cum.kwh.y.high +
  LAC.E$cum.kwh.y.high + MENA.E$cum.kwh.y.high + NAM.E$cum.kwh.y.high +
  SAS.E$cum.kwh.y.high + SSA.E$cum.kwh.y.high + WE.E$cum.kwh.y.high

cum.energy.all.regions.mean <- EAP.E$cum.kwh.y.mean + EECA.E$cum.kwh.y.mean +
  LAC.E$cum.kwh.y.mean + MENA.E$cum.kwh.y.mean + NAM.E$cum.kwh.y.mean +
  SAS.E$cum.kwh.y.mean + SSA.E$cum.kwh.y.mean + WE.E$cum.kwh.y.mean

cum.energy.all.regions.frame <- data.frame(
  cum.energy.years,
  cum.all.regions.kwh.d.low, cum.all.regions.kwh.d.mean, cum.all.regions.kwh.d.high,
  cum.all.regions.cum.kwh.d.low, cum.all.regions.cum.kwh.d.mean, cum.all.regions.cum.kwh.d.high,
  cum.energy.all.regions.low, cum.energy.all.regions.mean, cum.energy.all.regions.high) %>% 
  mutate(regions='Total')

colnames(cum.energy.all.regions.frame) <- colnames(EAP.E)

#### make dataframes for plotting ####
#capacity
capacity.plot.data <- rbind(cum.capacity.all.subregions.frame %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            EAP.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            EECA.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            LAC.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            MENA.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            NAM.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            SAS.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            SSA.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions),
                            WE.C %>%
                              mutate(ratio.capacity=
                                       cum.m3.d/cum.capacity.all.subregions)) %>%
  mutate(cum.million.m3.d = cum.m3.d / 10^6) %>%
  mutate(subregion = factor(subregion, levels=c('Total', 'EAP', 'EECA', 'LAC', 'MENA',
                                                'NAM', 'SAS', 'SSA', 'WE')))

#energy
energy.plot.data <- rbind(cum.energy.all.regions.frame %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean),
                          EAP.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          EECA.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          LAC.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          MENA.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          NAM.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          SAS.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          SSA.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean), 
                          WE.E %>% 
                            mutate(ratio.energy.mean=cum.kwh.y.mean/cum.energy.all.regions.mean)) %>% 
  mutate(cum.twh.y.low = cum.kwh.y.low / 10^9) %>% 
  mutate(cum.twh.y.mean = cum.kwh.y.mean/ 10^9) %>% 
  mutate(cum.twh.y.high = cum.kwh.y.high / 10^9) %>% 
  mutate(subregion = factor(subregion, levels=c('Total',
                                                'EAP', 'EECA', 'LAC', 'MENA',
                                                'NAM', 'SAS', 'SSA', 'WE')))

#### plots
m3.day.plot.subr <- ggplot(capacity.plot.data, aes(x=Online.date, y=cum.m3.y / 10^9,
                                                   color=subregion)) +
  geom_line(linewidth=1.2) +
  xlab('Year') +
  ylab('Mm3/d\n')+
  ggtitle('Cumulative capacity') +
  scale_color_manual(values=palette.subr)  +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=14),
        axis.title.y = element_text(size=12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  guides(color=guide_legend(title="Subregion", ncol=3))


twh.day.plot.subr <- ggplot(energy.plot.data, aes(x=Online.date, y=cum.twh.y.mean, 
                                                  color=subregion)) +
  geom_line(linewidth=1.2) +
  geom_ribbon(aes(x=Online.date, ymin=cum.twh.y.low, ymax=cum.twh.y.high, fill=subregion),
              alpha=0.4, inherit.aes=F, show.legend=F) +
  scale_color_manual(values=palette.subr) +
  scale_fill_manual(values=palette.subr)+
  xlab('Year') +
  ylab('\nTWh/year\n')+
  scale_y_continuous(sec.axis = sec_axis(~.*0.0036, name='EJ/year\n')) +
  ggtitle('Cumulative energy use') +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.title.y = element_text(size=12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position = 'bottom') +
  guides(color=guide_legend(title="Subregion", ncol=3))

ratio.capacity.plot.subr <- ggplot(capacity.plot.data %>%
                                     filter(!grepl('Total',subregion)),
                                   aes(x=Online.date, y=ratio.capacity,
                                       color=subregion)) +
  geom_line(linewidth=1.2) +
  xlab('\nYear') +
  ylab('Ratio')+
  ggtitle('\nCapacity share') +
  scale_color_manual(values=palette.subr.only, guide="none") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.title.y = element_text(size=12))

ratio.energy.plot.subr <- ggplot(energy.plot.data %>%
                                   filter(!grepl('Total',subregion)),
                                 aes(x=Online.date, y=ratio.energy.mean,
                                     color=subregion) ) +
  geom_line(linewidth=1.2) +
  xlab('\nYear\n') +
  ggtitle('\nEnergy use share') +
  scale_color_manual(values=palette.subr.only, guide="none") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size=14),
        axis.text.y=element_blank(),
        axis.title.y=element_blank()) 

# 
combined.cumulative.subr <- m3.day.plot.subr + twh.day.plot.subr

# combined_cumulative
combined.ratios.subr <- (ratio.capacity.plot.subr + 
                           plot_spacer() + ratio.energy.plot.subr) +
  plot_layout(widths = c(4, 0.38 ,4 ))

# combined_ratios
combined.all.subr <- (combined.cumulative.subr / combined.ratios.subr) +
  plot_annotation('Subregions\n',
                  theme=theme(plot.title=element_text(hjust=0.5, size=16))) +
  plot_layout(guides = 'collect') &
  theme(plot.title = element_text(hjust= 0.5, face='bold'),
        legend.position="bottom")