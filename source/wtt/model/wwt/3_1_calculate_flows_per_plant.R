rm(list=ls())

library(dplyr)
library(vroom)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(x, appendLF = F)
}


inputDirPlants <- '../../../../input/wtt/wwt/'
inputDirVolumes <- '../../../../output/wtt/model/wwt/0_volumes/0_raw/0_hybas_lev01/'
inputDirFlows <- '../../../../output/wtt/model/wwt/0_volumes/3_flows_model_output/'

#read original hydrowaste.raw
hydrowaste.raw <- 
  read.csv(paste0(inputDirPlants, 'HydroWASTE_updated_China.csv')) %>% 
  mutate(WASTE_DIS_KM3_Y = WASTE_DIS * 365 / 10^9) %>% 
  filter(!is.na(POP_SERVED)) %>% 
  filter(!is.na(WASTE_DIS_KM3_Y))

global.flows.raw <- vroom(paste0(inputDirVolumes, 'wwt_global.csv'),
                        col_names = TRUE, show_col_types = FALSE)

modelled.flows <- vroom(paste0(inputDirFlows, 'nearest_ss_flows.csv'),
                        col_names = TRUE, show_col_types = FALSE)


sum(hydrowaste.raw$WASTE_DIS_KM3_Y) #pre-flow assignment hydrowaste 
sum(global.flows.raw$wwtv) / 1000 #original Jones 2021
sum(modelled.flows$wwtv) / 1000 #post-flow assignment Jones


#### calculate population and volumes per plant #### 
#---- 1.1. Reassign volumes when more than a plant is in the same pixel ----#

#calculate volumes and population per cell id sink (plant pixels)
cell.id.volumes <- modelled.flows %>% 
  filter(ss_distance <= 50) %>% #cut at maximum 50 km distance 
  group_by(cell_ID_sink) %>% 
  summarise(volume.jones = sum(wwtv) / 1000, #KM3
            population = sum(population))

sum(cell.id.volumes$volume.jones)

# #check difference between modelled and cut at 50 km
# sum(modelled.flows$wwtv)
# sum(cell.id.volumes$volume.jones)

#find cell ids that contain more than one plant
hydrowaste.raw.multiple <- hydrowaste.raw %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(count = n())  %>% 
  filter(count > 1)

#get volumes and plants in these pixels
cell.id.multiple <- cell.id.volumes[
  cell.id.volumes$cell_ID_sink %in% hydrowaste.raw.multiple$pcrglobwb_cellID, ]

hydrowaste.raw.multiple.data <- hydrowaste.raw[
  hydrowaste.raw$pcrglobwb_cellID %in% hydrowaste.raw.multiple$pcrglobwb_cellID, ]

sum(hydrowaste.raw.multiple.data$WASTE_DIS_KM3_Y)
sum(cell.id.multiple$volume.jones)
sum(hydrowaste.raw.multiple.data$POP_SERVED)
sum(cell.id.multiple$population)

#### BUG IN THIS LOOP ####
hydrowaste.raw.flows.list <- list()
#loop over each cell id
for(id in seq(1, nrow(cell.id.multiple))){
  
  replaceMessage(paste0(
    'Calculating flows of pixels with more than 1 plant... ', 
    id, ' / ', nrow(cell.id.multiple)))
  
  cell.id <- cell.id.multiple$cell_ID_sink[id]
  
  #get wastewater volumes and population for that cellID (Jones)
  flows.selected.id <- modelled.flows[
    modelled.flows$cell_ID_sink == cell.id, ]
  
  # get sums to reassign volumes and population proportionally (ACTUAL DATA)
  sum.volumes.jones <- sum(flows.selected.id$wwtv) / 1000
  sum.population.jones <- sum(flows.selected.id$population)
  
  # get hydrowaste.raw plants for that cell id (hydrowaste.raw)
  hydrowaste.raw.selected.id <- hydrowaste.raw[
    hydrowaste.raw$pcrglobwb_cellID %in% cell.id, ]
  
  # get sums to reassign volumes and population proportionally (RATIOS TO REASSIGN)
  sum.volumes.hydrowaste.raw <- sum(hydrowaste.raw.selected.id$WASTE_DIS_KM3_Y, na.rm = T)
  sum.population.hydrowaste.raw <- sum(hydrowaste.raw.selected.id$POP_SERVED, na.rm = T)
  
  # loop per plant for a cell id 
  reassign.cell.id.list <- list()
  for(plant in seq(1, nrow(hydrowaste.raw.selected.id))){
    
    ratio.plant.volume <- 
      hydrowaste.raw.selected.id$WASTE_DIS_KM3_Y[plant] / sum.volumes.hydrowaste.raw
    
    ratio.plant.population <- 
      hydrowaste.raw.selected.id$POP_SERVED[plant] / sum.population.hydrowaste.raw
    
    #reassign volumes and population to each plant
    plant.volume <- sum.volumes.jones * ratio.plant.volume
    
    plant.population <- sum.population.jones * ratio.plant.population
    
    #make dataframe to later cbind
    data.df <- as.data.frame(cbind(plant.volume, plant.population))
    
    colnames(data.df) <- c('flow_5arcmin', 'population_5arcmin')
    
    reassign.cell.id.list[[plant]] <- data.df
    
    #0.2365
    #0.51
  }
  
  #explode reassigned gridcell plants
  reassign.cell.id.df <- do.call(rbind, reassign.cell.id.list)
  
  hydrowaste.raw.cell.id.reassigned <- cbind(hydrowaste.raw.selected.id, reassign.cell.id.df)
  
  #assign to list of all plants
  hydrowaste.raw.flows.list[[id]] <- hydrowaste.raw.cell.id.reassigned
  
}


hydrowaste.raw.multiple.flows <- do.call(rbind, hydrowaste.raw.flows.list)

sum(hydrowaste.raw.multiple.flows$WASTE_DIS_KM3_Y)
sum(hydrowaste.raw.multiple.flows$flow_5arcmin)
sum(hydrowaste.raw.multiple.flows$POP_SERVED)
sum(hydrowaste.raw.multiple.flows$population_5arcmin)






#### 1.2. assign volumes and populWASTE_DIS_KM3_Y#### 1.2. assign volumes and population when only one plant in a pixel
cell.id.single <- cell.id.volumes[
  !cell.id.volumes$cell_ID_sink %in% hydrowaste.raw.multiple$pcrglobwb_cellID, ] %>% 
  rename(pcrglobwb_cellID = cell_ID_sink)

#find cell ids that contain a single plant
hydrowaste.raw.single <- hydrowaste.raw %>% 
  group_by(pcrglobwb_cellID) %>% 
  summarise(count = n()) %>% 
  filter(count == 1)

hydrowaste.raw.single.data <- hydrowaste.raw[
  !hydrowaste.raw$pcrglobwb_cellID %in% hydrowaste.raw.multiple$pcrglobwb_cellID, ]

hydrowaste.raw.single.flows <- inner_join(
  hydrowaste.raw.single.data, 
  cell.id.single) %>% 
  rename(flow_5arcmin = volume.jones,
         population_5arcmin = population)

#### 3. bind single and multiple plants pixels ####
hydrowaste.raw.modelled.flows <- rbind(hydrowaste.raw.single.flows, 
                                   hydrowaste.raw.multiple.flows) %>%
  arrange(Country) %>% 
  filter(population_5arcmin != 0) %>% 
  filter(flow_5arcmin !=0)

write.csv(hydrowaste.raw.modelled.flows, paste0(inputDirFlows, 'wwtp_modelled_flows.csv'),
          row.names = F)

sum(hydrowaste.raw.modelled.flows$WASTE_DIS_KM3_Y) 
sum(hydrowaste.raw.modelled.flows$flow_5arcmin) 
sum(hydrowaste.raw.modelled.flows$POP_SERVED, na.rm = T)
sum(hydrowaste.raw.modelled.flows$population_5arcmin)


#### 4. get plants that are modelled without any wastewaters (empty) ####
plants.modelled.offline <- hydrowaste.raw %>% 
  filter(!WASTE_ID %in% hydrowaste.raw.modelled.flows$WASTE_ID)

write.csv(plants.modelled.offline, paste0(inputDirFlows, 'wwtp_modelled_offline.csv'),
          row.names = F)
