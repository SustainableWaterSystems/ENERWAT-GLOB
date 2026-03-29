library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)

formatterlog10 <- function(x){ 
  10^x
}

inputDirWorld <- '../../../../input/global_data/'
inputDirData <- '../../../../output/wtt/model/'
outputDir <- '../../../../output/wtt/visualization/general/'

#### inputs ####
liu.wwt <- read.csv(paste0(inputDirWorld,
                           'energy/Liu2016-est-wwt_country_energy.csv'), sep = ';')
liu.dwt <- read.csv(paste0(inputDirWorld,
                           'energy/Liu2016-est-dwt_country_energy.csv'), sep = ';')

energy.wwt <- read.csv(paste0(
  inputDirData, 'wwt/1_energy/energy_countries_2015_wwt.csv'))

energy.dwt <- read.csv(paste0(
  inputDirData, 'dwt/2_energy/energy_countries_2015_dwt.csv'))

countries.regions <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv')) %>%
  mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT, levels=c('EAP', 'EECA', 'LAC', 'MENA',
                                                            'NAM', 'SAS', 'SSA', 'WE')))


#### processing ####

#### wastewater 
liu.wwt$trtww..km3. <- 
  as.numeric(gsub(",", ".", liu.wwt$trtww..km3.))
liu.wwt$E4W_ww_trt..EJ. <- 
  as.numeric(gsub(",", ".", liu.wwt$E4W_ww_trt..EJ.))

# bind data for plot #
plot.data.wwt <- inner_join(
  liu.wwt %>% 
    rename(Country = country.renamed, 
           volumes.liu = trtww..km3., 
           energy.liu = E4W_ww_trt..EJ.), 
  energy.wwt %>%
    select(Country, wwt.km3.y, ej.y.low, ej.y.mean, ej.y.high) %>% 
    rename(volumes.this.study = wwt.km3.y)) %>% 
  inner_join(countries.regions %>% 
               select(Country, REGION_WB_SHORT))

#### cwdt
plot.data.dwt <- inner_join(energy.dwt %>% 
                                  select(Country, demands.total,
                                         energy.total.low.ej,
                                         energy.total.mean.ej,
                                         energy.total.high.ej), 
                                liu.dwt) %>% 
  mutate(demands.km3 = demands.total / 10^9) %>% 
  inner_join(countries.regions %>%
               select(Country, REGION_WB_SHORT)) %>% select(-demands.total)

#### plotting ####
palette.subr.only <- c('#332288', '#117733', '#44AA99',
                       '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#882255')


#### wwt 
volumes.wwt <- ggplot() +
  geom_point(data = plot.data.wwt, 
             aes(log10(volumes.liu), log10(volumes.this.study),
                 color = REGION_WB_SHORT)) +
  geom_abline (slope=1, linetype = "dashed") +
  ggtitle('Wastewater treatment <br /><br /> Volumes (km<sup>3</sup> y<sup>-1</sup>)') +
  ylab('This study\n') +
  scale_color_manual(values=palette.subr.only) +
  theme_light() +
  guides(color=guide_legend(title="Subregion", ncol=4, override.aes = list(size = 3))) +
  theme(plot.title = element_markdown(hjust = 0.5),
        axis.title.x = element_blank()) +
  scale_x_continuous(labels = formatterlog10) +
  scale_y_continuous(labels = formatterlog10)

energy.wwt <- ggplot() +
  geom_pointrange(data = plot.data.wwt,
                  aes(x = log10(energy.liu), y = log10(ej.y.mean),
                      ymin = log10(ej.y.low), ymax = log10(ej.y.high),
                      color = REGION_WB_SHORT), size = 0.2) +
  geom_abline (slope=1, linetype = "dashed") +
  ggtitle('<br />Energy consumption (EJ y<sup>-1</sup>)') +
  xlab('Liu et al. (2016)') +
  ylab('This study\n') +
  scale_color_manual(values=palette.subr.only, guide="none") +
  theme_light() +
  theme(plot.title = element_markdown(hjust = 0.5))  +
  scale_x_continuous(labels = formatterlog10) +
  scale_y_continuous(labels = formatterlog10, 
                     breaks = scales::pretty_breaks(n = 5))

#### cwdt
volumes.dwt <- ggplot() +
  geom_point(data = plot.data.dwt, 
             aes(log10(demands.km3), log10(Municipal.withdrawal),
                 color = REGION_WB_SHORT)) +
  geom_abline (slope=1, linetype = "dashed") +
  ggtitle('Drinking water treatment <br /><br /> Volumes (km<sup>3</sup> y<sup>-1</sup>)') +
  scale_color_manual(values=palette.subr.only, guide = 'none') +
  theme_light() +
  theme(plot.title = element_markdown(hjust = 0.5),
        axis.title = element_blank())  +
  scale_x_continuous(labels = formatterlog10) +
  scale_y_continuous(labels = formatterlog10, position = "right")


energy.dwt <- ggplot() +
  geom_pointrange(data = plot.data.dwt,
                  aes(x = log10(E4W_trt..EJ.), y = log10(energy.total.mean.ej),
                      ymin = log10(energy.total.low.ej), ymax = log10(energy.total.high.ej),
                      color = REGION_WB_SHORT), size = 0.2) +
  geom_abline (slope=1, linetype = "dashed") +
  xlab('Liu et al. (2016)') +
  ggtitle('<br />Energy consumption (EJ y<sup>-1</sup>)') +
  scale_color_manual(values=palette.subr.only, guide="none") +
  theme_light() +
  theme(plot.title = element_markdown(hjust = 0.5),
        axis.title.y = element_blank()) +
  scale_x_continuous(labels = formatterlog10) +
  scale_y_continuous(labels = formatterlog10, position = "right") 

#### patch ####
patch.validation <- (volumes.wwt + volumes.dwt) / (energy.wwt + energy.dwt) + 
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom')

ggsave(paste0(outputDir, 'modelled_here_vs_liu.png'), patch.validation,
       height=8, width=8, units='in', dpi=300)

file.show(paste0(outputDir, 'modelled_here_vs_liu.png'))
