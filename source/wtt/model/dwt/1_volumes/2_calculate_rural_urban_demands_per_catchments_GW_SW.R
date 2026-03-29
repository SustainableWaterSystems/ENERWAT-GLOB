rm(list=ls())

library(vroom)
library(dplyr)


replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


inputDirWorld <- '../../../../../input/global_data/'
inputDirHybas <- paste0(inputDirWorld, 'general/HydroBASINS_5arcmin/1_hybas_higher_levels/')
inputDirTreatmentPop <- '../../../../../output/wtt/model/dwt/0_population/'
inputDirTreatmentVol <- '../../../../../output/wtt/model/dwt/1_volumes/1_allocation_desalination_SW_GW/'

outputDir <- paste0('../../../../../output/wtt/model/dwt/1_volumes/2_catchments/')
dir.create(outputDir, recursive = T, showWarnings = F)


hybas.continents <- c('af','ar','as','au','eu','na','sa','si')

#### read ####
countries.5arcmin <- vroom(paste0(inputDirWorld,
                                  'general/countries_raster_5arcmin.csv'))


population.treatment.5arcmin <- vroom(
  paste0(inputDirTreatmentPop, 
         '3_output_clean/population_treatment_5arcmin_SAFE.csv')) 

treatment.desalination <- vroom(
  paste0(inputDirTreatmentVol,
         'demands_domestic_allocated_to_desalination.csv')
)


treatment.rural <- vroom(
  paste0(inputDirTreatmentVol,
         'demands_domestic_allocated_to_SW_and_GW_treated_rural.csv')
)
treatment.urban <- vroom(
  paste0(inputDirTreatmentVol,
         'demands_domestic_allocated_to_SW_and_GW_treated_urban.csv')
)


untreated.rural <- vroom(
  paste0(inputDirTreatmentVol,
         'demands_domestic_allocated_to_SW_and_GW_untreated_rural.csv')
)
untreated.urban <- vroom(
  paste0(inputDirTreatmentVol,
         'demands_domestic_allocated_to_SW_and_GW_untreated_urban.csv')
)

#### processing ####
#loop over continents and levels 
#will likely use level 10 (smallest catchments)
global.treatment.cc.list.rural <- list()
global.treatment.cc.list.urban <- list()

for(continent in 1:length(hybas.continents)){
  
  # continent = 1
  continent.sel <- hybas.continents[continent]
  level <- '10'
  
  hydrobasins.continent <- vroom(
    paste0(inputDirHybas,  continent.sel, '/lev', level, '.csv'),
                                show_col_types = F, delim = ',')
  
  #join catchment with country information
  hydrobasins.cc <- inner_join(
    hydrobasins.continent, countries.5arcmin,
    by = 'cell_ID'
  )
  
  #get countries with treatment volumes
  countries.treatment <- unique(c(
    unique(treatment.rural$Country),
    unique(treatment.urban$Country)))
  countries.continent <- unique(hydrobasins.cc$Country)

  
  #### get countries in continent that have treated or missing from dataset
  countries.continent.treatment <- intersect(
    countries.treatment, countries.continent
  )
  
  continent.rural.list <- list()
  continent.urban.list <- list()
  
  #filter population and demand by country
  for(country in 1:length(countries.continent.treatment)){

    country.sel <- countries.continent.treatment[country]
    
    replaceMessage(
      paste0(
        'HydroBASINS continent ', continent, '/',
        length(hybas.continents), ' : ', continent.sel,
        ' - Country ', country, '/',
             length(countries.continent.treatment),
             ' : ',country.sel))
    
    country.basins <- hydrobasins.cc %>% 
      filter(Country == country.sel)
    
    #filter rural and urban
    country.basin.treatment.rural <- treatment.rural %>% 
      filter(Country == country.sel) %>%
      inner_join(country.basins %>% select(cell_ID,
                                           catchment),
                 by = 'cell_ID') %>%
      relocate(catchment, .after = cell_ID)
    
    country.basin.treatment.urban <- treatment.urban %>% 
      filter(Country == country.sel) %>%
      inner_join(country.basins %>% select(cell_ID,
                                           catchment),
                 by = 'cell_ID') %>%
      relocate(catchment, .after = cell_ID)
  
    
    #filter surface water and groundwater
    summarise.catchment.rural <- country.basin.treatment.rural %>% 
      group_by(catchment) %>% 
      summarise(demands.to.SW = sum(demands.to.SW),
                demands.to.GW = sum(demands.to.GW),
                population.to.SW = sum(population.to.SW),
                population.to.GW = sum(population.to.GW)
                ) %>% 
      mutate(Country = country.sel) %>% 
      relocate(Country, .before=catchment)
    
    summarise.catchment.urban <- country.basin.treatment.urban %>% 
      group_by(catchment) %>% 
      summarise(demands.to.SW = sum(demands.to.SW),
                demands.to.GW = sum(demands.to.GW),
                population.to.SW = sum(population.to.SW),
                population.to.GW = sum(population.to.GW)
      ) %>% 
      mutate(Country = country.sel) %>% 
      relocate(Country, .before=catchment)
    
    continent.rural.list[[country]] <- summarise.catchment.rural
    continent.urban.list[[country]] <- summarise.catchment.urban
    
    
  }
  
  continent.rural <- do.call(rbind, continent.rural.list) 
  continent.urban <- do.call(rbind, continent.urban.list)
  
  write.csv(continent.rural,
            paste0(outputDir, 'treatment_rural_',
                   continent.sel, '.csv'),
            row.names = F)
  
  write.csv(continent.urban,
            paste0(outputDir, 'treatment_urban_',
                   continent.sel, '.csv'),
            row.names = F)
  
  global.treatment.cc.list.rural[[continent]] <- continent.rural
  global.treatment.cc.list.urban[[continent]] <- continent.urban
  
}

global.treatment.rural <- do.call(rbind, global.treatment.cc.list.rural)
global.treatment.urban <- do.call(rbind, global.treatment.cc.list.urban)

vroom_write(global.treatment.rural,
          paste0(outputDir, 'global_treatment_rural.csv'),
          ',')

vroom_write(global.treatment.urban,
          paste0(outputDir, 'global_treatment_urban.csv'),
          ',')
