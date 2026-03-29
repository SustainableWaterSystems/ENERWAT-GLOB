rm(list=ls())

library(dplyr)
library(vroom)

inputDirRaw <- '../../../../../input/wtt/dwt/'
inputDirOutput <- '../../../../../output/wtt/model/dwt/'

dwtp.usa <- read.csv(paste0(inputDirRaw, 'dwtp_geocoded_USA_gridcells_2015.csv'))
eureau.data <- read.csv(paste0(inputDirRaw, 'EurEau_drinking_water_sources.csv'))

dwt.sources.output <- read.csv(paste0(inputDirOutput, '2_energy/energy_countries_2015_dwt.csv'))

sum(dwt.sources.output$demands.total) / 10^9

#### analyse ratios #### 
output.gw.sw.ratios <- dwt.sources.output %>% 
  select(Country, demands.to.SW.total, demands.to.GW.total, demands.total) %>% 
  mutate(ratio.sw.modelled = demands.to.SW.total / demands.total * 100,
         ratio.gw.modelled = demands.to.GW.total / demands.total * 100)

#### USA 
#reported population served ratio
sum(dwtp.usa$pop.served[dwtp.usa$water.source=='SW'])
sum(dwtp.usa$pop.served[dwtp.usa$water.source=='GW'])

ratio.sw.usa <- 
  sum(dwtp.usa$pop.served[dwtp.usa$water.source=='SW']) / 
  ( sum(dwtp.usa$pop.served[dwtp.usa$water.source=='SW']) +
      sum(dwtp.usa$pop.served[dwtp.usa$water.source=='GW']) )

ratio.gw.usa <- 
  sum(dwtp.usa$pop.served[dwtp.usa$water.source=='GW']) / 
  ( sum(dwtp.usa$pop.served[dwtp.usa$water.source=='SW']) +
  sum(dwtp.usa$pop.served[dwtp.usa$water.source=='GW']) )

output.usa <- output.gw.sw.ratios %>% filter(Country == 'United States of America')


#### EUROPE 
validate.europe <- inner_join(output.gw.sw.ratios,
                              eureau.data)

gw.check <- validate.europe %>% 
  filter(ratio.sw.modelled > ratio.surface.water) %>% 
  mutate(sw.ratio.gap = ratio.surface.water - ratio.sw.modelled)
