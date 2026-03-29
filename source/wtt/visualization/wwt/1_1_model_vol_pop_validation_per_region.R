#### general data analysis for flows
#### countries, distance, volumes, etc...

library(dplyr)
library(vroom)
library(ggplot2)
library(ggpointdensity)
library(patchwork)
library(ggtext)

formatterlog10 <- function(x){ 
  10^x
}

#### function for global label ####
add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}



#### paths and files ####
#paths
inputDirWorld <- '../../../../input/global_data/'
inputDirPlants <- '../../../../input/wtt/wwt/'
inputDirVolumes <- '../../../../output/wtt/model/wwt/0_volumes/0_raw/0_hybas_lev01/'
inputDirFlows <- '../../../../output/wtt/model/wwt/0_volumes/3_flows_model_output/'

outputDir <- '../../../../output/wtt/visualization/wwt/'

#raw input (at beginning of model)
hydrowaste.raw <- read.csv(paste0(inputDirPlants, 'HydroWASTE_updated_China.csv')) %>% 
  mutate(WASTE_DIS_KM3_Y = WASTE_DIS * 365 / 10^9)

population.raw <- vroom(paste0(inputDirWorld, 'general/population_5arcmin_2015.csv'),
                        col_names = TRUE, show_col_types = FALSE)

global.volumes.raw <- vroom(paste0(inputDirVolumes, 'wwt_global.csv'),
                          col_names = TRUE, show_col_types = FALSE)

countries.regions <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv')) %>%
  mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT, levels=c('EAP', 'EECA', 'LAC', 'MENA',
                                                'NAM', 'SAS', 'SSA', 'WE')))

#modelled active plants and flows to these plants
modelled.active.plants <- read.csv(
  paste0(inputDirFlows, 'wwtp_modelled_flows.csv'))  

modelled.flows <- vroom(
  paste0(inputDirFlows, 'nearest_ss_flows.csv'),
                        col_names = TRUE, show_col_types = FALSE)

#calculate inactive plants according to model 
modelled.inactive.plants <- hydrowaste.raw[
  !hydrowaste.raw$pcrglobwb_cellID %in% modelled.active.plants$pcrglobwb_cellID, ]


#### calculate things ####

print(paste0(' Summary of distances souce-sink [km] : '))
summary(modelled.flows$ss_distance / 1000)

print(paste0('total treated wastewater [km3] : ', sum(modelled.flows$wwtv / 1000)))
print(paste0('total estimated wastewater discharge [km3] : ', sum(modelled.flows$wwtp / 1000)))

print(paste0('total attached population 5arcmin [billion people] : ', 
             sum(modelled.flows$population / 1000000000)))
print(paste0('total population hydrowaste [billion people] : ', 
             sum(modelled.active.plants$POP_SERVED / 1000000000, na.rm = T)))



ratio.attached.hw <-  sum(modelled.active.plants$POP_SERVED / 1000000000) /
  sum(population.raw$pop_2015 / 1000000000)
ratio.attached.model <-  sum(modelled.flows$population / 1000000000) /
  sum(population.raw$pop_2015 / 1000000000)


print(paste0('Ratio 2015 population attached to wwtp (hydrowaste): ', ratio.attached.hw))
print(paste0('Ratio 2015 population attached to wwtp (modelled): ', ratio.attached.model))

summary(modelled.active.plants$flow_5arcmin)
summary(modelled.active.plants$WASTE_DIS/ 1000)

#### plots density validation flows, population ####
density.plot.volumes <- ggplot(data = modelled.active.plants, 
                               aes(x = log10(WASTE_DIS_KM3_Y), y = log10(flow_5arcmin))) +
  geom_pointdensity() +
  geom_abline (slope=1, linetype = "dashed") +
  xlab('Hydrowaste') +
  ylab('Modelled') +
  theme_light() +
  labs(title = "Treated wastewater volumes by plant (km<sup>3</sup> y<sup>-1</sup>)", 
       subtitle = "Global") +
  theme(plot.title=element_markdown(size=26, hjust=0.5),
        plot.subtitle=element_text(size=20, hjust=0.5),
        axis.title = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'None') +
  scale_x_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5)) 
# density.plot.volumes

density.plot.population <- ggplot(data = modelled.active.plants, 
                                  aes(x = log10(POP_SERVED), y = log10(population_5arcmin))) +
  geom_pointdensity() +
  geom_abline (slope=1, linetype = "dashed") +
  xlab('Hydrowaste') +
  theme_light() + 
  labs(title = "Population served by plant\n", 
       subtitle = "Global") +
  theme(plot.title=element_text(size=26, hjust=0.5),
        plot.subtitle=element_text(size=20, hjust=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'None') +
  scale_x_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5))

#### do by region ####
regions <- unique(countries.regions$REGION_WB_SHORT)

plot.volumes.list <- list()
for(idx in seq(1, length(regions))){
  
  sel.region <- regions[idx]
  
  countries.region.sel <- countries.regions %>% 
    filter(REGION_WB_SHORT == sel.region)
  
  plot.data.region <- modelled.active.plants[
    modelled.active.plants$Country %in% countries.region.sel$Country,
  ]
  
  density.plot.volumes.region <- ggplot(data = plot.data.region, 
                                        aes(x = log10(WASTE_DIS_KM3_Y), 
                                            y = log10(flow_5arcmin))) +
    geom_pointdensity() +
    geom_abline (slope=1, linetype = "dashed") +
    ylab('Modelled') +
    ggtitle(sel.region) +
    theme_light() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'None') +
    scale_x_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5))
  
  plot.volumes.list[[idx]] <- density.plot.volumes.region
  
}

plot.population.list <- list()
for(idx in seq(1, length(regions))){
  
  sel.region <- regions[idx]
  
  countries.region.sel <- countries.regions %>% 
    filter(REGION_WB_SHORT == sel.region)
  
  plot.data.region <- modelled.active.plants[
    modelled.active.plants$Country %in% countries.region.sel$Country,
  ]
  
  density.plot.pop.region <- ggplot(data = plot.data.region, 
                                        aes(x = log10(POP_SERVED), 
                                            y = log10(population_5arcmin))) +
    geom_pointdensity() +
    geom_abline (slope=1, linetype = "dashed") +
    xlab('Hydrowaste') +
    ylab('Modelled') +
    ggtitle(sel.region) +
    theme_light() +
    theme(plot.title = element_text(size = 20, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'None') +
    scale_x_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(labels = formatterlog10, breaks = scales::pretty_breaks(n = 5))
  
  plot.population.list[[idx]] <- density.plot.pop.region
  
}


patch.regions <- 
  (density.plot.volumes | density.plot.population) /
  (plot.volumes.list[[4]] | plot.volumes.list[[2]] |
     plot.population.list[[4]] | plot.population.list[[2]]) /
  (plot.volumes.list[[7]] | plot.volumes.list[[3]] |
     plot.population.list[[7]] | plot.population.list[[3]]) /
  (plot.volumes.list[[8]] | plot.volumes.list[[1]] |
     plot.population.list[[8]] | plot.population.list[[1]]) /
  (plot.volumes.list[[6]] | plot.volumes.list[[5]] |
     plot.population.list[[6]] | plot.population.list[[5]]) 

patch.regions.guides <- patch.regions + 
  plot_layout(guides = "collect") &
  scale_color_viridis_c() & 
  theme(axis.text = element_text(size=12))

patch.regions.labs <- patch.regions.guides %>%
  add_global_label(Xlab = 'Reported',
                   Ylab = "Modelled",
                   size = 10)

ggsave(paste0(outputDir,'flows_pop_validation_regions.png'), patch.regions.labs,
       height=18, width=18, units='in', dpi=300, bg='white')

file.show(paste0(outputDir,'flows_pop_validation_regions.png'))
