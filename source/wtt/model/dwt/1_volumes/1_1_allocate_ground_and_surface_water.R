rm(list=ls())

#### script to assign demands that were not assigned to desalination ####
#### rural-urban, surface water - groundwater

library(vroom)
library(dplyr)
library(FNN)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


inputDirWorld <- '../../../../../input/global_data/'
inputDirRaw <- '../../../../../input/wtt/dwt/'
inputDirTreatment <- '../../../../../output/wtt/model/dwt/0_population/'

outputDir <- '../../../../../output/wtt/model/dwt/1_volumes/1_allocation_desalination_SW_GW/'

countries.5arcmin <- vroom(
  paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'))

population.treatment.5arcmin.2015 <- vroom(
  paste0(inputDirTreatment, '3_output_clean/population_treatment_5arcmin_SAFE.csv'))

water.withdrawals.2015 <- vroom(
  paste0(inputDirRaw, 'aqueduct_withdrawal_sources_5arcmin_2015.csv'),
  show_col_types = F)

water.demands.2015 <- vroom(
  paste0(inputDirRaw, 'domestic_demands_2015_tot_m3.csv'), show_col_types = F)

desalination.drinking <- vroom(
  paste0(outputDir, 'demands_domestic_allocated_to_desalination.csv'),
  show_col_types = F)


pop.treatment.demands <- inner_join(water.demands.2015 %>% 
                                      select(cell_ID, demands.m3.2015),
                                    population.treatment.5arcmin.2015
                                    )
#### processing ####
treatment.countries <- sort(unique(population.treatment.5arcmin.2015$Country))

# get demands that were not allocated to desalination
water.demands.2015.not.desal <- population.treatment.5arcmin.2015 %>% 
  filter(!cell_ID %in% desalination.drinking$cell_ID) %>% 
  inner_join(water.demands.2015 %>% select(cell_ID, demands.m3.2015))

water.withdrawals.2015.filtered <- water.withdrawals.2015 %>%
  filter(SW.ww.m3 !=0 | nonFossilGW.ww.m3 !=0 | fossilGW.ww.m3 != 0) %>%
  inner_join(countries.5arcmin) %>%
  mutate(GW.total.m3 = fossilGW.ww.m3 + nonFossilGW.ww.m3) %>%
  mutate(ratio.SW = SW.ww.m3 / (SW.ww.m3 + GW.total.m3)) %>% 
  mutate(ratio.GW = 1 - ratio.SW)

country.domestic.rural.list <- list()
country.domestic.urban.list <- list()


#### loop over countries
for(idx in 1:length(treatment.countries)){

  replaceMessage(paste0('Country ', idx, '/', length(treatment.countries)))
  
  country.sel <- treatment.countries[idx]

  country.demands.to.allocate <- water.demands.2015.not.desal %>%
    filter(Country == country.sel)
  
  if(nrow(country.demands.to.allocate) == 0){next}
  
  country.demands.urban <- country.demands.to.allocate %>% 
    filter(pop_type_cell == 'U' | pop_type_cell == 'UR')
  
  country.demands.rural <- country.demands.to.allocate %>% 
    filter(pop_type_cell == 'R' | pop_type_cell == 'RU')


  country.withdrawals <- water.withdrawals.2015.filtered %>%
    filter(Country == country.sel)

  if(nrow(country.withdrawals) == 0){next}
  
  #coordinates of demands (without ID for FNN computation)
  cor_demands_rural <- as.data.frame(cbind(
    country.demands.rural$lon,
    country.demands.rural$lat))
  
  cor_demands_urban <- as.data.frame(cbind(
    country.demands.urban$lon,
    country.demands.urban$lat))

  #coordinates of wastewater sources (without ID for FNN computation)
  cor_ww <- as.data.frame(cbind(
    country.withdrawals$cell_lon,
    country.withdrawals$cell_lat))
  
  if(nrow(country.demands.rural) == 0){
    matches_rural = 0
  } 
  else(matches_rural <- knnx.index(cor_ww, cor_demands_rural, k = 1))
  
  if(nrow(country.demands.urban) == 0){
    matches_urban = 0
    }
  else(matches_urban <- knnx.index(cor_ww, cor_demands_urban, k = 1))

  
  #get withdrawal pixels matches
  country.withdrawals.matches.rural <- country.withdrawals[matches_rural,] %>% 
    select(cell_ID, ratio.SW, ratio.GW) %>% 
    rename(cell_ID_source_water = cell_ID)
  
  country.withdrawals.matches.urban <- country.withdrawals[matches_urban,] %>% 
    select(cell_ID, ratio.SW, ratio.GW) %>% 
    rename(cell_ID_source_water = cell_ID)
  
  
  #allocate surface and groundwater to rural and urban based on pixel ratios
  country.rural.sources <- cbind(country.withdrawals.matches.rural,
                                 country.demands.rural) %>% 
    mutate(demands.to.SW = demands.m3.2015 * ratio.SW,
           population.to.SW = pop_2015_total * ratio.SW) %>% 
    mutate(demands.to.GW = demands.m3.2015 - demands.to.SW,
           population.to.GW = pop_2015_total - population.to.SW)
  
  country.urban.sources <- cbind(country.withdrawals.matches.urban,
                                 country.demands.urban) %>% 
    mutate(demands.to.SW = demands.m3.2015 * ratio.SW,
           population.to.SW = pop_2015_total * ratio.SW) %>% 
    mutate(demands.to.GW = demands.m3.2015 - demands.to.SW,
           population.to.GW = pop_2015_total - population.to.SW)

  #append to list
  country.domestic.rural.list[[idx]] <- country.rural.sources
  country.domestic.urban.list[[idx]] <- country.urban.sources
  

}

#undo lists
domestic.sources.table.rural <- do.call(rbind, country.domestic.rural.list) %>% 
  select(cell_ID, Country, pop_2015_total, demands.m3.2015, GDP_capita_2015, treatment, 
         demands.to.SW, demands.to.GW, population.to.SW, population.to.GW)
domestic.sources.table.urban <- do.call(rbind, country.domestic.urban.list) %>% 
  select(cell_ID, Country, pop_2015_total, demands.m3.2015, GDP_capita_2015, treatment, 
         demands.to.SW, demands.to.GW, population.to.SW, population.to.GW)

#split rural / urban - treated / untreated
domestic.sources.treated.rural <- domestic.sources.table.rural %>% 
  filter(treatment == 1)
domestic.sources.treated.urban <- domestic.sources.table.urban %>% 
  filter(treatment == 1)

domestic.sources.untreated.rural <- domestic.sources.table.rural %>%
  filter(treatment == 0)
domestic.sources.untreated.urban <- domestic.sources.table.urban %>%
  filter(treatment == 0)


vroom_write(domestic.sources.treated.rural, paste0(
  outputDir, 'demands_domestic_allocated_to_SW_and_GW_treated_rural.csv'), ',')
vroom_write(domestic.sources.treated.urban, paste0(
  outputDir, 'demands_domestic_allocated_to_SW_and_GW_treated_urban.csv'), ',')
vroom_write(domestic.sources.untreated.rural, paste0(
  outputDir, 'demands_domestic_allocated_to_SW_and_GW_untreated_rural.csv'), ',')
vroom_write(domestic.sources.untreated.urban, paste0(
  outputDir, 'demands_domestic_allocated_to_SW_and_GW_untreated_urban.csv'), ',')