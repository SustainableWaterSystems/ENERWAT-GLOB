library(ggplot2)
library(vroom)
library(dplyr)
library(ggtext)

inputDir <- '../../../../output/wtt/model/wwt/0_volumes/3_flows_model_output/'
outputDir <- '../../../../output/wtt/visualization/wwt/'

wwt.flows.all.levels <- vroom(
  paste0(inputDir, 'nearest_ss_flows.csv'),
  col_names = TRUE, show_col_types = FALSE) 

#### plot ####
plot.data.all.levels <- wwt.flows.all.levels %>% 
  select(ss_distance, wwtv, ss_info) %>% 
  arrange(ss_distance) %>% 
  mutate(cum.wwtv = cumsum(wwtv) / 1000,
         method = 'all levels') 

p <- ggplot(plot.data.all.levels, 
            aes(x=ss_distance, y=cum.wwtv, color = ss_info)) +
  geom_point() +
  xlab('Source-Sink distance (km)')+
  ylab("Cumulative treated wastewater (km<sup>3</sup> y<sup>-1</sup>)") +
  geom_vline(xintercept = 50, linetype = "dashed") +
  theme_light() +
  theme(legend.position = 'None',
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title.y = element_markdown())

ggsave(paste0(outputDir,'cumulative_flows_distance_dashed.png'), p,
       height=5, width=10, units='in', dpi=300, bg='white')

file.show(paste0(outputDir,'cumulative_flows_distance_dashed.png'))