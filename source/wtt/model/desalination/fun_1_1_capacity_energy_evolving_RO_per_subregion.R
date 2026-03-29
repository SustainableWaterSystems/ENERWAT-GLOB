#### separate script to allow for RO time evolving calculations 

#### calculate cumulative for RO ####
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