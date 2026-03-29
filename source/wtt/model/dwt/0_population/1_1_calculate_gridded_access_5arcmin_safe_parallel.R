rm(list=ls())

#### script to calculate pixel-level safe water access (AQUASTAT -> within 30 minutes walk) 
#### and connection to a water treatment plant (World Bank -> safely managed drinking water)

library(vroom)
library(dplyr)
library(parallel)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


#### read files ####
inputDir <- '../../../../../output/wtt/model/dwt/0_population/1_input/'
outputDir <- '../../../../../output/wtt/model/dwt/0_population/2_output/'

countries.water.access <- read.csv(paste0(inputDir, 'countries_safe_water_access_input.csv'))
pop.gdp.5arcmin <- vroom(paste0(inputDir, 'pop_gdp_input_df.csv'))

length(unique(pop.gdp.5arcmin$Country))

treatment.type <- 'SAFE'

#### processing ####
countries.water.access.matched <- countries.water.access %>% 
  filter(Country %in% unique(pop.gdp.5arcmin$Country))
  
# do per country natuurlijk
countries.loop <- unique(countries.water.access.matched$Country)

#order by: 1 km gdp / capita (upscaled to 5 arcmin)

#### function #### 
source('fun_1_0_get_un_treated_cells.R')

#apply function in parallel 
mclapply(1:length(countries.loop), get_un_treated_cells, mc.cores=1)