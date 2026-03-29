rm(list=ls())

library(vroom)
library(dplyr)
library(reshape2)
library(data.table)
library(forcats)
library(ggplot2)
library(ggtext)
library(patchwork)

inputDirPop <- '../../../../output/wtt/model/dwt/0_population/3_output_clean/'
inputDirVol <- '../../../../output/wtt/model/dwt/1_volumes/1_allocation_desalination_SW_GW/'
inputDirEnergy <- '../../../../output/wtt/model/dwt/2_energy/'
inputDirWorld <- '../../../../input/global_data/'
outputDir <- '../../../../output/wtt/visualization/dwt/'
dir.create(outputDir, showWarnings = F, recursive = T)

#### input files ####

countries.regions <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv')) 

countries.raster.5arcmin <- vroom(paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv'),
                                  show_col_types = F)

population.safe <- vroom(
  paste0(inputDirPop ,
         'population_treatment_5arcmin_SAFE.csv'),
  show_col_types = F) %>% 
  filter(treatment == 1)

population.basic <- vroom(
  paste0(inputDirPop ,
         'population_treatment_5arcmin_BASIC.csv'),
  show_col_types = F) %>% 
  filter(treatment == 1) %>% 
  filter(!cell_ID %in% population.safe$cell_ID)

population.desalination <- vroom(
  paste0(inputDirVol ,
         'demands_domestic_allocated_to_desalination.csv'),
  show_col_types = F)

population.treated.volumes.rural <- vroom(
  paste0(inputDirVol ,
         'demands_domestic_allocated_to_SW_and_GW_treated_rural.csv'),
  show_col_types = F)

population.treated.volumes.urban <- vroom(
  paste0(inputDirVol ,
         'demands_domestic_allocated_to_SW_and_GW_treated_urban.csv'),
  show_col_types = F)

population.untreated.volumes.rural <- vroom(
  paste0(inputDirVol ,
         'demands_domestic_allocated_to_SW_and_GW_untreated_rural.csv'),
  show_col_types = F)

population.untreated.volumes.urban <- vroom(
  paste0(inputDirVol ,
         'demands_domestic_allocated_to_SW_and_GW_untreated_urban.csv'),
  show_col_types = F)

modelled.energy <- read.csv(paste0(inputDirEnergy, 
                                   'energy_countries_2015_dwt.csv'))


#### processing ####

#### calculate volumes and population per type of (non-)treatment
#### TO DO: rural/urban, gw/sw splits

population.treated.sw.gw <- rbind(population.treated.volumes.urban,
                                population.treated.volumes.rural)

population.untreated.sw.gw <- rbind(population.untreated.volumes.rural,
                                    population.untreated.volumes.urban)

population.desalination.summarise <- population.desalination %>% 
  inner_join(countries.raster.5arcmin) %>% 
  group_by(Country) %>% 
  summarise(pop_total_desalination = sum(pop_2015_total),
            volumes_total_desalination = sum(demands.m3.2015))

population.conventional.treatment.summarise <- population.treated.sw.gw %>% 
  group_by(Country) %>% 
  summarise(pop_total_conventional = sum(pop_2015_total),
            volumes_total_conventional = sum(demands.m3.2015),
            volumes_total_conventional_sw = sum(demands.to.SW),
            volumes_total_conventional_gw = sum(demands.to.GW)
            )

population.basic.summarise <- population.untreated.sw.gw %>% 
  filter(cell_ID %in% population.basic$cell_ID) %>% 
  group_by(Country) %>% 
  summarise(pop_total_basic = sum(pop_2015_total),
            volumes_total_basic = sum(demands.m3.2015),
            volumes_total_basic_sw = sum(demands.to.SW),
            volumes_total_basic_gw = sum(demands.to.GW))
  
population.untreated.summarise <- population.untreated.sw.gw %>% 
  filter(!cell_ID %in% population.basic$cell_ID) %>% 
  group_by(Country) %>% 
  summarise(pop_total_untreated = sum(pop_2015_total),
            volumes_total_untreated = sum(demands.m3.2015),
            volumes_total_untreated_sw = sum(demands.to.SW),
            volumes_total_untreated_gw = sum(demands.to.GW)) 

energy.regions.summarise <- modelled.energy %>% 
  inner_join(countries.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(energy.sw.low.twh = sum(energy.sw.low.twh),
            energy.sw.mean.twh = sum(energy.sw.mean.twh),
            energy.sw.high.twh = sum(energy.sw.high.twh),
            energy.gw.low.twh = sum(energy.gw.low.twh),
            energy.gw.mean.twh = sum(energy.gw.mean.twh),
            energy.gw.high.twh = sum(energy.gw.high.twh),
            energy.total.low.twh = sum(energy.total.low.twh),
            energy.total.mean.twh = sum(energy.total.mean.twh),
            energy.total.high.twh = sum(energy.total.high.twh)
            )


#### put together in one df
df.all.sources <- merge(population.desalination.summarise, 
                        population.conventional.treatment.summarise, all = T) %>% 
  merge(., population.basic.summarise, all = T) %>% 
  merge(., population.untreated.summarise, all = T) %>% 
  replace(is.na(.), 0)

df.all.sources.regions <- df.all.sources %>% 
  inner_join(countries.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(pop_total_desalination = sum(pop_total_desalination),
            pop_total_conventional = sum(pop_total_conventional),
            pop_total_basic = sum(pop_total_basic),
            pop_total_untreated = sum(pop_total_untreated),
            volumes_total_desalination = sum(volumes_total_desalination),
            volumes_total_conventional = sum(volumes_total_conventional),
            volumes_total_basic = sum(volumes_total_basic),
            volumes_total_untreated = sum(volumes_total_untreated),
            
            volumes_total_conventional_gw = sum(volumes_total_conventional_gw),
            volumes_total_conventional_sw = sum(volumes_total_conventional_sw),
            volumes_total_basic_sw = sum(volumes_total_basic_sw),
            volumes_total_basic_gw = sum(volumes_total_basic_gw),
            volumes_total_untreated_gw = sum(volumes_total_untreated_gw),
            volumes_total_untreated_sw = sum(volumes_total_untreated_sw),
            
            
            ) %>% 
  mutate(pop_total = pop_total_desalination + pop_total_conventional +
           pop_total_basic + pop_total_untreated,
         vol_total = volumes_total_desalination + volumes_total_conventional +
           volumes_total_basic + volumes_total_untreated) %>% 
  #safe access ratio (population order)
  #municipal demands km3/capita (volumes order)
  mutate(safe.access.ratio.no.desal = pop_total_conventional / pop_total * 100,
         safe.acces.ratio.with.desal = 
           (pop_total_conventional + pop_total_desalination) / pop_total * 100,
         m3.capita.all.source = vol_total / pop_total) %>% 
  #population ratios
  mutate(pop.ratio.desalination = pop_total_desalination / pop_total * 100,
         pop.ratio.conventional = pop_total_conventional / pop_total * 100,
         pop.ratio.basic = pop_total_basic / pop_total * 100,
         pop.ratio.untreated = pop_total_untreated / pop_total * 100) %>% 
  #volumetric ratios
  mutate(vol.ratio.desalination = volumes_total_desalination / vol_total * 100,
         vol.ratio.conventional = volumes_total_conventional / vol_total * 100,
         vol.ratio.basic = volumes_total_basic / vol_total * 100,
         vol.ratio.untreated = volumes_total_untreated / vol_total * 100) %>% 
  mutate(vol.ratio.conventional.sw = volumes_total_conventional_sw / vol_total * 100,
         vol.ratio.conventional.gw = volumes_total_conventional_gw / vol_total * 100, 
         vol.ratio.basic.sw = volumes_total_basic_sw / vol_total * 100, 
         vol.ratio.basic.gw = volumes_total_basic_gw / vol_total * 100, 
         vol.ratio.untreated.sw = volumes_total_untreated_sw / vol_total * 100, 
         vol.ratio.untreated.gw = volumes_total_untreated_gw / vol_total * 100
         )
            
#### make dfs for plots, first select and then melt ####
#population absolute
plot.df.pop.abs <- df.all.sources.regions %>% 
    dplyr::select(REGION_WB_SHORT, 
           pop_total_desalination, pop_total_conventional, 
           pop_total_basic, pop_total_untreated)

plot.df.melted.pop.abs <- melt(setDT(plot.df.pop.abs), id.vars = "REGION_WB_SHORT",
                               variable.name = "pop.abs") %>% 
  mutate(pop.abs = factor(pop.abs,
                                      levels=c(
                                        'pop_total_untreated',
                                        'pop_total_basic',
                                        'pop_total_desalination',
                                        'pop_total_conventional'
                                        ))) %>% 
  inner_join(df.all.sources.regions %>% 
               dplyr::select(REGION_WB_SHORT, safe.access.ratio.no.desal, 
                      safe.acces.ratio.with.desal)) %>%
  mutate(REGION_WB_SHORT = fct_reorder(REGION_WB_SHORT, 
                                       safe.acces.ratio.with.desal, .desc = T))

#population relative
plot.df.pop.ratio <- df.all.sources.regions %>% 
  dplyr::select(REGION_WB_SHORT, 
         pop.ratio.desalination, pop.ratio.conventional, 
         pop.ratio.basic, pop.ratio.untreated)

plot.df.melted.pop.ratio <- melt(setDT(plot.df.pop.ratio), id.vars = "REGION_WB_SHORT",
                               variable.name = "pop.ratio") %>% 
  mutate(pop.ratio = factor(pop.ratio,
                          levels=c(
                            'pop.ratio.untreated',
                            'pop.ratio.basic',
                            'pop.ratio.desalination',
                            'pop.ratio.conventional'
                            ))) %>% 
  inner_join(df.all.sources.regions %>% 
               dplyr::select(REGION_WB_SHORT, safe.access.ratio.no.desal, 
                      safe.acces.ratio.with.desal)) %>%
  mutate(REGION_WB_SHORT = fct_reorder(REGION_WB_SHORT, 
                                       safe.acces.ratio.with.desal, .desc = T))


#volumes absolute
plot.df.vol.abs <- df.all.sources.regions %>% 
  dplyr::select(REGION_WB_SHORT, 
         volumes_total_desalination, 
         volumes_total_conventional_sw, volumes_total_conventional_gw, 
         volumes_total_basic_sw, volumes_total_basic_gw,
         volumes_total_untreated_sw, volumes_total_untreated_gw)

plot.df.melted.vol.abs <- melt(setDT(plot.df.vol.abs), id.vars = "REGION_WB_SHORT",
                               variable.name = "vol.abs") %>% 
  mutate(vol.abs = factor(vol.abs,
                            levels=c(
                              'volumes_total_untreated_sw',
                              'volumes_total_untreated_gw',
                              'volumes_total_basic_sw',
                              'volumes_total_basic_gw',
                              'volumes_total_desalination',
                              'volumes_total_conventional_sw',
                              'volumes_total_conventional_gw'
                              ))) %>% 
  inner_join(df.all.sources.regions %>% 
               dplyr::select(REGION_WB_SHORT, safe.acces.ratio.with.desal, m3.capita.all.source)) %>%
  mutate(REGION_WB_SHORT = fct_reorder(REGION_WB_SHORT, 
                                       safe.acces.ratio.with.desal, .desc = T))

#volumes relative
plot.df.vol.ratio <- df.all.sources.regions %>% 
  dplyr::select(REGION_WB_SHORT, 
         vol.ratio.desalination, 
         vol.ratio.conventional.sw, vol.ratio.conventional.gw,
         vol.ratio.basic.sw, vol.ratio.basic.gw,
         vol.ratio.untreated.sw, vol.ratio.untreated.gw) 

plot.df.melted.vol.ratio <- melt(setDT(plot.df.vol.ratio), id.vars = "REGION_WB_SHORT",
                               variable.name = "vol.ratio") %>% 
  mutate(vol.ratio = factor(vol.ratio,
                          levels=c(
                            'vol.ratio.untreated.sw',
                            'vol.ratio.untreated.gw',
                            'vol.ratio.basic.sw',
                            'vol.ratio.basic.gw',
                            'vol.ratio.desalination',
                            'vol.ratio.conventional.sw',
                            'vol.ratio.conventional.gw'
                            ))) %>% 
  inner_join(df.all.sources.regions %>% 
               dplyr::select(REGION_WB_SHORT, safe.acces.ratio.with.desal, m3.capita.all.source)) %>%
  mutate(REGION_WB_SHORT = fct_reorder(REGION_WB_SHORT, 
                                       safe.acces.ratio.with.desal, .desc = T))



var1 <- data.frame(energy.regions.summarise$energy.total.low.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption')
var2 <- data.frame(energy.regions.summarise$energy.total.mean.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption')
var3 <- data.frame(energy.regions.summarise$energy.total.high.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'Total energy consumption')
var4 <- data.frame(energy.regions.summarise$energy.sw.low.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'SWT energy consumption')
var5 <- data.frame(energy.regions.summarise$energy.sw.mean.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'SWT energy consumption')
var6 <- data.frame(energy.regions.summarise$energy.sw.high.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'SWT energy consumption')
var7 <- data.frame(energy.regions.summarise$energy.gw.low.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'GWT energy consumption')
var8 <- data.frame(energy.regions.summarise$energy.gw.mean.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'GWT energy consumption')
var9 <- data.frame(energy.regions.summarise$energy.gw.high.twh,
                   energy.regions.summarise$REGION_WB_SHORT) %>%
  mutate(cat = 'GWT energy consumption')


colnames(var1)[1] <- 'low'
colnames(var2)[1] <- 'mean'
colnames(var3)[1] <- 'high'
colnames(var4)[1] <- 'low'
colnames(var5)[1] <- 'mean'
colnames(var6)[1] <- 'high'
colnames(var7)[1] <- 'low'
colnames(var8)[1] <- 'mean'
colnames(var9)[1] <- 'high'
colnames(var1)[2] <- 'REGION_WB_SHORT'
colnames(var2)[2] <- 'REGION_WB_SHORT'
colnames(var3)[2] <- 'REGION_WB_SHORT'
colnames(var4)[2] <- 'REGION_WB_SHORT'
colnames(var5)[2] <- 'REGION_WB_SHORT'
colnames(var6)[2] <- 'REGION_WB_SHORT'
colnames(var7)[2] <- 'REGION_WB_SHORT'
colnames(var8)[2] <- 'REGION_WB_SHORT'
colnames(var9)[2] <- 'REGION_WB_SHORT'

df.low <- data.frame(rbind(var1, var4, var7))
df.mean <- data.frame(rbind(var2, var5, var8))
df.high <- data.frame(rbind(var3, var6, var9))

df.plot.energy <- cbind(df.low, df.mean$mean, df.high$high)
colnames(df.plot.energy)[4:5] <- c('mean','high')
df.plot.energy.cleaned <- df.plot.energy %>%
  relocate(mean, .after=low) %>%
  relocate(high, .after=mean) %>%
  relocate(REGION_WB_SHORT, .before = low) %>%
  mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT,
                                  levels=c('EAP','NAM','EECA','SAS',
                                           'LAC','WE','MENA','SSA')))

df.plot.energy.cleaned$cat <- factor(df.plot.energy.cleaned$cat, 
                              levels = c('Total energy consumption',
                                         'SWT energy consumption',
                                         'GWT energy consumption'
))

#### plot #### 
palette.population <- c('#FE6100', '#FFB000','#785EF0' , '#648FFF')

palette.volumes <- c('#332288', '#117733', '#44AA99',
                       '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')

palette.energy <- c("#7FC97F", "#BEAED4", "#FDC086"
                       )

p.pop.abs <- ggplot(plot.df.melted.pop.abs, 
                       mapping = aes(x = REGION_WB_SHORT, 
                                     y = value, fill = pop.abs)) +
  geom_bar(stat='identity', position='stack') + 
  scale_fill_manual(labels = c('Untreated drinking water',
                               'Basic water access',
                               'Desalination',
                               'Conventional drinking water treatment'
                               ) ,
                    values = palette.population, guide="none") +
  ylab('People') +
  ggtitle('A. Modelled drinking water access (2015)') +
  labs(fill = 'Status') +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 28),
        axis.title = element_text(size = 24))

p.pop.ratio <- ggplot(plot.df.melted.pop.ratio, 
                    mapping = aes(x = REGION_WB_SHORT, 
                                  y = value, fill = pop.ratio)) +
  geom_bar(stat='identity', position='stack') + 
  scale_fill_manual(labels = c('Untreated drinking water',
                               'Basic water access',
                               'Desalination',
                               'Conventional drinking water treatment'
  ) ,
  values = palette.population) +
  xlab('Subregion') +
  ylab('Population ratio (%)') +
  labs(fill = 'Status') +
  theme_light() +
  guides(fill = guide_legend(ncol = 1, title.position = 'top')) +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24,
                                    margin = margin(t = 20, r = 0, b = 10, l = 0)),
        legend.text=element_text(size=22),
        legend.title = element_text(size = 24, hjust = 0.5,
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
        legend.spacing = unit(1, 'cm'),
        legend.position = 'bottom'
        )

p.vol.abs <- ggplot(plot.df.melted.vol.abs, 
                      mapping = aes(x = REGION_WB_SHORT, 
                                    y = value / 10^9, fill = vol.abs)) +
  geom_bar(stat='identity', position='stack') + 
  scale_fill_manual(labels = c('Untreated SW',
                               'Untreated GW',
                               'Basic water access - GW',
                               'Basic water access - SW',
                               'Desalination',
                               'Conventional SWT',
                               'Conventional GWT'
  ) ,
  values = palette.volumes, guide="none") +
  ylab('<br />Volumes (km<sup>3</sup> y<sup>-1</sup>) <br />') +
  ggtitle('B. Modelled drinking water sources (2015)') +
  labs(fill = 'Source') +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 28),
        axis.text = element_text(size = 18),
        axis.title.y = element_markdown(size = 24),
        legend.position = 'bottom'
        ) +
  scale_y_continuous(position = "right")

p.vol.ratio <- ggplot(plot.df.melted.vol.ratio, 
                      mapping = aes(x = REGION_WB_SHORT, 
                                    y = value, fill = vol.ratio)) +
  geom_bar(stat='identity', position='stack') + 
  scale_fill_manual(labels = c('Untreated SW',
                               'Untreated GW',
                               'Basic water access - GW',
                               'Basic water access - SW',
                               'Desalination',
                               'Conventional SWT',
                               'Conventional GWT'
  ) ,
  values = palette.volumes) +
  xlab('Subregion') +
  ylab('\nVolumetric ratio (%)') +
  labs(fill = 'Source') +
  guides(fill = guide_legend(ncol = 2, title.position = 'top')) +
  theme_light() +
  theme(axis.text = element_text(size = 18),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24,
                                    margin = margin(t = 20, r = 0, b = 10, l = 0)),
        legend.text=element_text(size=22, margin = margin(r = 10, l=5, unit = "pt")),
        legend.title = element_text(size = 24, hjust = 0.5,
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
        legend.spacing = unit(1, 'cm'),
        legend.position = 'bottom'
        ) +
  scale_y_continuous(position = "right")


p.energy <- ggplot(data = df.plot.energy.cleaned, aes(x=REGION_WB_SHORT, y=mean, fill=cat)) +
  geom_bar(stat="identity", position='dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9))+
  scale_fill_manual(values = palette.energy) +
  theme_light() +
  xlab('Subregion') +
  ylab("TWh y<sup>-1</sup>") +
  ggtitle('\n\nC. Energy consumption of conventional drinking water treatment (2015)') +
  theme(plot.title = element_text(hjust = 0.5, size = 28),
        axis.text = element_text(size = 18),
        axis.title.y = element_markdown(size = 24),
        axis.title.x = element_text(size = 24,
                                    margin = margin(t = 20, r = 0, b = 10, l = 0)),
        legend.text=element_text(size=22),
        legend.title = element_blank(),
        legend.key.spacing.y = unit(0.2, 'cm'),
        legend.position = 'bottom',
        legend.direction = 'vertical'
                                  )

#### patch ####
patch.plot <- (( p.pop.abs + p.vol.abs ) / (p.pop.ratio + p.vol.ratio) / p.energy ) 

ggsave(paste0(outputDir,'pop_volumes_status_dwt_2015.png'), patch.plot,
       height=22, width=18, units='in', dpi=300)

file.show(paste0(outputDir,'pop_volumes_status_dwt_2015.png'))