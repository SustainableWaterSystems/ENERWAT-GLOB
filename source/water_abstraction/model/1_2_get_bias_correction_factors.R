rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/0_timeseries_raw/')

#### load ####
section.information <- read.csv(
  paste0(inputDirDischarge, '0_information_sections.csv'))

discharge.data.reported <- read.csv(
  '../../../../input/global_data/ibwt_mp_volumes_reported.csv') 

data.segments <- read.csv(paste0(inputDirElevation, '1_information_intakes.csv'))

#### calculate bias correction factor ####
discharge.info.df <- inner_join(section.information, discharge.data.reported) 

discharge.totals <- discharge.info.df %>% 
  group_by(transfer.name) %>%
  summarise(total.discharge.biased = sum(discharge.km3.y.mean))
  
bias.corrector <- merge(discharge.info.df, discharge.totals, all = T) %>%  
  mutate(discharge.ratio = discharge.km3.y.mean / total.discharge.biased) %>% 
  # mutate(bias.mult.factor =
  #          volumes.km3.y / total.discharge.biased * discharge.ratio) %>%
  mutate(bias.mult.factor =
           volumes.km3.y / total.discharge.biased) %>%
  arrange(Country) %>% 
  relocate(Country, .before=transfer.name) %>% 
  relocate(discharge.km3.y.mean, .before = total.discharge.biased)
  
#### tidy ####
bias.corrector$bias.mult.factor <- round(bias.corrector$bias.mult.factor, 4)

#table with proper bias correction factors
bias.corrector.filter <- bias.corrector %>%
  filter(!is.na(bias.mult.factor)) %>%
  filter(bias.mult.factor != Inf) 

#table with unsuable correction factors
bias.corrector.false <- bias.corrector %>%
  filter(is.na(bias.mult.factor) | bias.mult.factor == Inf)

#create unique id for sections across all transfers, because
#it can happen that the same reservoir is used for different sections
bias.corrector.unique <- bias.corrector.filter %>% 
  mutate(section.id.unique = seq(nrow(bias.corrector.filter))) %>% 
  relocate(section.id.unique, .before = Country)

#get reservoirs with bias correction factor
reservoirs.bias.corrector <- unique(bias.corrector.unique$reservoir.id)

#segments that will be able to be corrected
data.segments.corrector <- data.segments %>%
  filter(reservoir.id %in% reservoirs.bias.corrector) %>% 
  inner_join(bias.corrector.unique)

#segments that won't be able to be corrected
data.segments.missing <- data.segments %>%
  filter(!reservoir.id %in% reservoirs.bias.corrector)
 
#### save ####
write.csv(bias.corrector.unique,
          paste0(inputDirDischarge, '1_bias_correction_sections.csv'), row.names = F)
write.csv(data.segments.corrector,
          paste0(inputDirDischarge, '2_bias_correction_segments.csv'), row.names = F)
