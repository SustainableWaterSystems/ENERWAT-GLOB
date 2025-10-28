#### script to match the GeoDAR reservoirs 
#### with a PCR-GLOBWB zone to later extract reservoir discharge

library(readxl)
library(vroom)
library(dplyr)
library(tidyr)

inputDir <- '../../../../input/water_abstraction/ibwt/'

inputDirTS <- '../../../../input/water_abstraction/ibwt/1_discharge_timeseries/'
outputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'

dir.create(outputDir, recursive = T, showWarnings = F)

#load stuff
megaprojects.outlets <-
  read_excel(paste0(inputDir, 'geodar_outlets_5arcmin_at_ibwt.xlsx'), sheet = 'Sections') %>%
  mutate_if(is.character, ~na_if(., '')) %>%
  fill(Country, ibwt.project.name, Purpose.main, Purpose.mixed, initial.year)

zones.5arcmin <- vroom(paste0(inputDirTS, 'maskGlobal.csv')) %>% 
  filter(zone_ID != 0)

reservoirs.5arcmin <- read.csv(paste0(inputDirTS, 'outlet_map_geodar_lakes_dams_unique.csv'))      


#### join reservoirs to zone ####
reservoirs.zone.5arcmin <- inner_join(
  
  reservoirs.5arcmin %>% select(cell_ID, reservoir_ID),
                                #, type),
  zones.5arcmin %>% select(cell_ID, zone_ID)
  
)

reservoirs.in.zone <- inner_join(megaprojects.outlets %>%
                                   rename(reservoir_ID = id.geodar.v11),
                                 reservoirs.zone.5arcmin %>%
                                   select(cell_ID, reservoir_ID,
                                          zone_ID))

#### reservoirs or rivers that are unmatched in PCR-GLOBWB
other.intakes <- megaprojects.outlets %>% 
  filter(!id.geodar.v11 %in% reservoirs.in.zone$reservoir_ID)

write.csv(reservoirs.in.zone, paste0(outputDir, 'ibwt_zones.csv'), row.names = F)
write.csv(other.intakes, paste0(outputDir, 'ibwt_other_intakes.csv'), row.names = F)
