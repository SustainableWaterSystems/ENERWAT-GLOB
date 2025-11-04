library(dplyr)
library(vroom)

inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_pumping_elevations/')
inputDirEnergyMonth <- paste0(inputDir, '2_energy/monthly/')
inputDirEnergyYear <- paste0(inputDir, '2_energy/yearly/')


#### load ####
intake.data <- read.csv(paste0(inputDirElevation, 'intake_information.csv'))

pumping.lift.data <- intake.data %>% 
  filter(intake.type == 'Pumping station')

n.countries <- list.dirs(inputDirEnergyYear, recursive = F, full.names = F)
# n.countries


countries.summary.list <- list()
countries.ts.list <- list()

for(i in seq(length(n.countries))){
  
  country <- n.countries[i]
  
  inputDirCountry <- paste0(inputDirEnergy, 'yearly/', country, '/')
  
  #get country transfers
  n.transfers <- list.dirs(inputDirCountry, recursive = F, full.names = F)
  
  transfers.summary.list <- list()
  transfers.ts.list <- list()
  
  for(j in seq(length(n.transfers))){
    
    transfer <- n.transfers[j]
    
    inputDirTransfer <- paste0(inputDirCountry, transfer, '/')
    
    n.segments <- list.files(inputDirTransfer)
    
    segments.summary.list <- list()
    segments.ts.list <- list()
    
    for(k in seq(length(n.segments))){
      
      segment <- n.segments[k]
      
      segment.energy <- read.csv(paste0(inputDirTransfer, segment))
      
      segment.energy.summary <- data.frame(
        unique(segment.energy$segment.name),
        mean(segment.energy$discharge.km3.y),
        mean(segment.energy$energy.twh.low),
        mean(segment.energy$energy.twh.high),
        mean(segment.energy$energy.kwh.m3.low),
        mean(segment.energy$energy.kwh.m3.high),
        mean(segment.energy$energy.kwh.m3.km.low),
        mean(segment.energy$energy.kwh.m3.km.high)
      )
      
      colnames(segment.energy.summary) <- c(
        'segment.name','mean.yearly.discharge',
        'mean.twh.y.low','mean.twh.y.high',
        'mean.kwh.m3.low','mean.kwh.m3.high',
        'mean.kwh.m3.km.low','mean.kwh.m3.km.high'
      )
      
      segments.ts.list[[k]] <- segment.energy
      segments.summary.list[[k]] <- segment.energy.summary 
      
    }
    
    #### timeseries dataframe
    segments.ts.df <- do.call(rbind, segments.ts.list)
    
    #### summary dataframe
    segments.summary <- do.call(rbind, segments.summary.list)
    
    segments.summary.df <- segments.summary %>% 
      mutate(transfer.name = transfer) %>% 
      relocate(transfer.name, .before=segment.name)
    
    transfers.summary.list[[j]] <- segments.summary.df
    transfers.ts.list[[j]] <- segments.ts.df
    
  }
  
  transfers.summary <- do.call(rbind, transfers.summary.list)
  
  transfers.summary.df <- transfers.summary %>% 
    mutate(Country = country) %>% 
    relocate(Country, .before=transfer.name)
  
  countries.summary.list[[i]] <- transfers.summary.df
  countries.ts.list[[i]] <- do.call(rbind, transfers.ts.list)
  
}

pumping.summary <- do.call(rbind, countries.summary.list)
pumping.ts.all <- do.call(rbind, countries.ts.list)

write.csv(pumping.summary, paste0(inputDirEnergy, 'pumping_summary.csv'), row.names = F)
vroom_write(pumping.ts.all, paste0(inputDirEnergy, 'pumping_timeseries_all_yearly.csv'), delim = ',')
