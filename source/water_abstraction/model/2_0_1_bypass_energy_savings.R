library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)


replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#### costants to calculate energy consumption ####
g <- 9.81 #m s-2
rho <- 1000 #density of water
delta.time <- 1 #year 
p.efficiency.low <- 0.5 #pumping efficiency (low value for uncertainty)
p.efficiency.high <- 0.9 #pumping efficiency (high value for uncertainty)


outputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(outputDir, '0_elevation_profiles/3_pumping_elevations/')

inputDirDischarge <- paste0(outputDir, '1_discharge_timeseries/')

outputDirEnergy <- paste0(outputDir, '2_energy/')
dir.create(outputDirEnergy, recursive = T, showWarnings = F)

#### load ####
intake.data.natural <- read.csv(paste0(inputDirElevation, 'intake_information_natural.csv'))
intake.data.infrastructure <- read.csv(paste0(inputDirElevation, 'intake_information_infrastructure.csv'))
bypass.data <- read.csv(paste0(inputDirElevation, 'bypass_information.csv'))


#### ####
# pumping.lift.data.natural <- intake.data.natural %>% 
#   filter(intake.type == 'Pumping station')
# 
# pumping.lift.data.infr <- intake.data.infrastructure %>% 
#   filter(intake.type == 'Pumping station')

tunnels.data <- bypass.data %>% 
  filter(bypass.type == 'Tunnel')

aqdct.data <- bypass.data %>% 
  filter(bypass.type == 'Aqueduct')

sum(tunnels.data$bypass.length.km)
sum(aqdct.data$bypass.length.km)
sum(tunnels.data$net.bypass)
sum(aqdct.data$net.bypass)

# #geodar id reservoirs connected to their sections
# transfer.sections.df <- 
#   read.csv(paste0(inputDirDischarge, 'ibwt_zones.csv')) 
# 
#reservoirs not found in Jen's data
non.matched.intakes <-
  read.csv(paste0(inputDirDischarge, 'ibwt_other_intakes.csv'))

#### process ####
# intake.data.df <- intake.data.natural %>% 
#   select(1:7) %>% 
#   mutate(max.elevation.natural = intake.data.natural$maximum.elevation,
#          max.elevation.infrastructure = intake.data.infrastructure$maximum.elevation,
#          pumping.lift.natural = intake.data.natural$pumping.lift,
#          pumping.lift.infrastructure = intake.data.infrastructure$pumping.lift,
#          segment.length = intake.data.natural$segment.length
#          )

n.countries <- unique(bypass.data$Country)


#### loop by country
for(i in seq(length(n.countries))){
  
  country <- n.countries[i]
  
  print(paste0('Country: ', country))
  
  #get transfers in selected country
  country.transfers <- bypass.data %>% 
    filter(Country == country) 
  
  country.transfers.unmatched <- non.matched.intakes %>% 
    filter(Country == country) %>% 
    select(ibwt.project.name, Section) %>% 
    rename(transfer.name = ibwt.project.name,
           section.id = Section)
  
  outputDirCountry <- paste0(outputDirEnergy, country, '/')
  # dir.create(outputDirCountry, recursive = T, showWarnings = F)
  
  n.transfers <- unique(country.transfers$transfer.name)
  
  #### loop by transfer
  for(j in seq(length(n.transfers))){
    
    transfer.name.sel <- n.transfers[j]
    
    print(paste0('Transfer: ', transfer.name.sel))
    
    #### get segments of transfer
    n.segments <- country.transfers %>% 
      filter(transfer.name == transfer.name.sel)
    
    #### remove sections of transfer that do not have a matched reservoir in global dataset
    
    unmatched.sections <- non.matched.intakes %>% 
      filter(ibwt.project.name == transfer.name.sel)
    
    n.sections <- setdiff(unique(n.segments$section.id), 
                                   unique(unmatched.sections$Section))
    
    ##if all sections do not have a reservoir skip to next transfer
    if(length(n.sections) == 0) next
    
    initial.year.transfer <- unique(transfer.sections.df$initial.year[
      transfer.sections.df$ibwt.project.name == transfer.name.sel
    ])
    
    outputDirTransfer <- paste0(outputDirCountry, transfer.name.sel, '/')
    dir.create(outputDirTransfer, recursive = T, showWarnings = F)
    
    #### loop by section
    for(k in seq(length(n.sections))){
      
      section.number <- n.sections[k]
      
      section.segments <- n.segments %>% 
        filter(section.id == section.number)
      
      print(paste0('Section: ', section.number))
      
      #get reservoir id
      section.reservoir <- transfer.sections.df %>% 
        filter(Country == country) %>% 
        filter(ibwt.project.name == transfer.name.sel) %>% 
        filter(Section == section.number)
      
      reservoir.id <- section.reservoir$reservoir_ID

      #read reservoir discharge
      reservoir.discharge <- read.csv(paste0(
        inputDirDischarge, 'timeseries/m3_s_outlet_', reservoir.id, '.csv'))
      
      #upscale discharge from monthly to yearly
      #sum or mean?
      discharge.year <- reservoir.discharge %>% 
        mutate(datetime = as.Date(datetime)) %>% 
        mutate(datetime = floor_date(datetime, 'year')) %>% 
        group_by(datetime) %>% 
        summarise(discharge.m3.s = mean(discharge)) %>%
        # summarise(discharge.m3.s = sum(discharge)) %>% 
        mutate(discharge.m3.y = discharge.m3.s * 60 * 60 * 24 * 365) %>% 
        mutate(discharge.km3.y = discharge.m3.y / 1000000000)
      
      initial.year.transfer.date <- as.Date(paste(initial.year.transfer, 1, 1, sep = "-"))
      
      #### cut timeseries at reported beginning year of operations
      discharge.year.cut <- discharge.year %>% 
        filter(datetime > initial.year.transfer.date)
      
      
      
      #### looop by segment
      #calculate segment energy
      for(l in seq(nrow(section.segments))){
        
        segment.data <- section.segments[l,]
        
        # print(paste0('Segment: ', segment.data$segment.full))
        
        ####
        # pumping.lift.natural <- 
        
        #calculate yearly energy consumption range (high - low efficiency)
        energy.j.low <-
          discharge.year.cut$discharge.m3.y * 
          (g * segment.data$pumping.lift.infrastructure) * (rho * delta.time) / p.efficiency.high
        
        energy.j.high <-
          discharge.year.cut$discharge.m3.y * 
          (g * segment.data$pumping.lift.infrastructure) * (rho * delta.time) / p.efficiency.low
        
        #tidy dataframe
        energy.year.segment <- discharge.year.cut %>% 
          mutate(energy.j.low = energy.j.low,
                 energy.j.high = energy.j.high) %>% 
          mutate(energy.kwh.low = energy.j.low / (3.6 * 10^6),
                 energy.kwh.high = energy.j.high / (3.6 * 10^6)) %>%
          mutate(energy.twh.low = energy.kwh.low / 10^9,
                 energy.twh.high = energy.kwh.high / 10^9) %>%
          mutate(energy.kwh.m3.low = energy.kwh.low / discharge.m3.y,
                 energy.kwh.m3.high = energy.kwh.high / discharge.m3.y) %>%
          mutate(energy.kwh.m3.km.low = energy.kwh.m3.low / max(segment.data$segment.length),
                 energy.kwh.m3.km.high = energy.kwh.m3.high/ max(segment.data$segment.length))
        
        energy.year.segment.df <- energy.year.segment %>% 
          mutate(segment.name = segment.data$segment.full) %>%
          relocate(segment.name, .before = datetime) %>% 
          mutate(transfer.name = transfer.name.sel) %>% 
          relocate(transfer.name, .before = segment.name) %>% 
          mutate(Country = country) %>% 
          relocate(Country, .before = transfer.name)
        
        write.csv(energy.year.segment.df, 
                  paste0(outputDirBypass, 'segment_', segment.data$segment.full, '.csv'), 
                  row.names = F)

      }
    }
  }
}
