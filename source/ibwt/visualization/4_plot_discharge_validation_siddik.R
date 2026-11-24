rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggtext)
library(ggthemes)
library(patchwork)


palette.timeseries <- c("#000000", "#BEAED4", '#648FFF','#FE6100') 
palette.map <- c("#7FC97F", '#FFB000', '#785EF0', "#F0027F") 

inputDir <- '../../../output/ibwt/model/'
inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')
outputDirViz <- '../../../output/ibwt/visualization/'

#### load ####
validation.monthly <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/3_validation_siddik_monthly.csv'))
validation.yearly <- read.csv(
  paste0(inputDirDischarge, '1_bias_corrected/3_validation_siddik_yearly.csv'))

# validation.monthly <- read.csv(
#   paste0(inputDirDischarge, '1_bias_corrected/3_1_validation_siddik_monthly_uncertainty.csv'))

#### 
#### pivot dataframe ####
df.pivot.yearly <- pivot_longer(validation.yearly,
                                cols=c(discharge.model.original, 
                                       discharge.model.corrected),
                                values_to='discharge.model') %>% 
  mutate(name = factor(name, levels = c('discharge.model.original',
                                        'discharge.model.corrected')))

df.pivot.monthly <- pivot_longer(validation.monthly,
                                 cols=c(discharge.model.original,
                                        discharge.model.corrected),
                                 values_to='discharge.model') %>%
  mutate(name = factor(name, levels = c('discharge.model.original',
                                        'discharge.model.corrected')))

# df.pivot.monthly <- pivot_longer(validation.monthly,
#                                  cols=c(discharge.model.original, 
#                                         discharge.model.corrected,
#                                         fit),
#                                  values_to='discharge.model') %>% 
#   mutate(name = factor(name, levels = c('discharge.model.original',
#                                         'discharge.model.corrected',
#                                         'fit')))

#### check plot ####
p.yearly <- ggplot(df.pivot.yearly) +
  geom_point(aes(x=discharge.siddik, y=discharge.model, 
                 color = name, shape = transfer.name)) +
  geom_abline(slope=1) + 
  xlab('Reported discharge (km<sup>3</sup> y<sup>-1</sup>)')+
  ylab('Modelled discharge (km<sup>3</sup> y<sup>-1</sup>)') +
  scale_y_continuous(position = "right")+
  scale_color_manual(values = c('#648FFF','#FE6100'),
                     labels = c('Original','Bias-corrected')) +
  theme_bw() +
  guides(color = guide_legend(title = 'Modelled discharge'),
         shape = guide_legend(title = 'Inter-basin water transfer', nrow=3))+
  theme(axis.title = element_markdown(),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5),
        
        ) 

# p.yearly

p.monthly <- ggplot(df.pivot.monthly) +
  geom_point(aes(x=discharge.siddik, y=discharge.model, 
                 color = name, shape = transfer.name)) +
  geom_abline(slope=1) +
  # geom_ribbon(aes(x=discharge.siddik, 
  #                 ymin=lwr, 
  #                 ymax=upr),
  #             alpha=0.2, 
  #             inherit.aes=F, show.legend=F) +
  xlab('Reported discharge (km<sup>3</sup> month<sup>-1</sup>)')+
  ylab('Modelled discharge (km<sup>3</sup> month<sup>-1</sup>)') +
  scale_color_manual(values = c('#648FFF','#FE6100'),
                     labels = c('Original','Bias-corrected')) +
  theme_bw() +
  guides(color = guide_legend(title = 'Modelled discharge'),
         shape = guide_legend(title = 'Inter-basin water transfer', nrow=3))+
  theme(axis.title = element_markdown(),
        legend.position = 'bottom',
        legend.title.position = 'top',
        legend.title = element_text(hjust=0.5)
  ) 
p.monthly

p.patch <- p.monthly + p.yearly +
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom')
p.patch

ggsave(paste0(outputDirViz,'discharge_validation_siddik.png'), p.patch,
       height=5, width=9, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz,'discharge_validation_siddik.png'))

sqrt(mean((validation.yearly$discharge.siddik - validation.yearly$discharge.model.corrected)^2, na.rm = TRUE))
sqrt(mean((validation.yearly$discharge.siddik - validation.yearly$discharge.model.original)^2, na.rm = TRUE))
