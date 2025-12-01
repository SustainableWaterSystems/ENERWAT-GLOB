rm(list=ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(vroom)

#### functions ####
replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


rsq <- function (x, y) cor(x, y) ^ 2

#### folders ####
inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirElevation <- paste0(inputDir, '0_elevation_profiles/3_information/')

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

#### load ####
section.information <- read.csv(
  paste0(inputDirDischarge, '0_timeseries_raw/1_bias_correction_sections.csv'))

discharge.yearly.corrected <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/0_1_discharge_bias_corrected_yearly.csv'))

#### process ####
discharge.corrected.summarise <- discharge.yearly.corrected %>% 
  group_by(section.id.unique) %>% 
  summarise(discharge.km3.y.mean.corrected = mean(discharge.km3.y.corrected))

validation.df <- inner_join(section.information, discharge.corrected.summarise) 

#### check number of reservoirs per transfer
reservoirs.n <- validation.df %>% 
  group_by(transfer.name) %>% 
  summarise(reservoirs.count=n())


#### validation dataframe

validation.df.alt <- validation.df %>% 
  group_by(transfer.name) %>% 
  summarise(discharge.km3.y.mean = mean(discharge.km3.y.mean),
            discharge.km3.y.mean.corrected = sum(discharge.km3.y.mean.corrected),
            volumes.km3.y = mean(volumes.km3.y)
            ) 

validation.df.res <- inner_join(validation.df.alt, reservoirs.n)

validation.df.singel <- validation.df.res %>% 
  filter(reservoirs.count == 1)
validation.df.multiple <- validation.df.res %>% 
  filter(reservoirs.count != 1)

#### check correlations
#before bias correction
rsq(validation.df.alt$discharge.km3.y.mean, validation.df.alt$volumes.km3.y) #r2 original
#after bias correction (all)
rsq(validation.df.alt$discharge.km3.y.mean.corrected, validation.df.alt$volumes.km3.y) #r2 bias corrected
#after bias correction (one reservoir)
rsq(validation.df.singel$discharge.km3.y.mean.corrected, validation.df.singel$volumes.km3.y) #r2 bias corrected
#after bias correction (multiple reservoirs)
rsq(validation.df.multiple$discharge.km3.y.mean.corrected, validation.df.multiple$volumes.km3.y) #r2 bias corrected


sum(validation.df.res$discharge.km3.y.mean.corrected)
sum(validation.df.res$volumes.km3.y)

sum(validation.df.singel$discharge.km3.y.mean.corrected)
sum(validation.df.singel$volumes.km3.y)

sum(validation.df.multiple$discharge.km3.y.mean)
sum(validation.df.multiple$discharge.km3.y.mean.corrected)
sum(validation.df.multiple$volumes.km3.y)

# length(unique())C