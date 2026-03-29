library(dplyr)
library(vroom)
library(forcats)
library(ggplot2)
library(reshape2)
library(patchwork)
library(ggtext)

inputDirWorld <- '../../../../input/global_data/'
inputDirRaw <- '../../../../input/wtt/wwt/'
inputDirEnergy <- '../../../../output/wtt/model/wwt/1_energy/'
outputDir <- '../../../../output/wtt/visualization/wwt/'
outputDirTables <- '../../../../output/wtt/model/wwt/1_energy/'


#### files #### 

wwp.data <- vroom(paste0(inputDirRaw, 'WWP_Jones2021_5arcmin.csv'))
wwt.data <- read.csv(paste0(inputDirEnergy, 'energy_countries_2015_wwt.csv'))

countries.raster <- vroom(
  paste0(inputDirWorld, 'general/countries_raster_5arcmin.csv')) %>% 
  select(cell_ID, country_ID) %>% 
  filter(country_ID != -9999)

countries.regions <- read.csv(
  paste0(inputDirWorld, 'general/countries_id_regions.csv')) 

regions.prod <- wwp.data %>% 
  inner_join(countries.raster) %>% 
  rename(ID = country_ID) %>%
  inner_join(., countries.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(wwp.km3.2015 = sum(wwp))

wwt.data.regions <- inner_join(wwt.data, countries.regions) 

#### processing and plotting ####
regions.summarise <- wwt.data.regions %>% 
  group_by(REGION_WB_SHORT, LEVEL) %>% 
  summarise(wwt.km3.y = sum(wwt.km3.y),
            twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high)
  ) %>% 
  inner_join(regions.prod) %>% 
  mutate(ratio.treated.2015 = wwt.km3.y / wwp.km3.2015 * 100)


regions.summarise$LEVEL[regions.summarise$LEVEL == "All" ] <- "Total"

untreated <- data.frame(regions.summarise$REGION_WB_SHORT,
                        regions.summarise$LEVEL,
                        regions.summarise$wwt.km3.y,
                        regions.summarise$wwp.km3.2015,
                        regions.summarise$wwp.km3.2015 - regions.summarise$wwt.km3.y) 

colnames(untreated) <- c('REGION_WB_SHORT','LEVEL','wwt.km3.y',
                         'wwp.km3.2015','wwu.km3.2015')

#### plot volumes #### 
#### plot 1 -> Regional volumes, produced/treated and ratio between the two
regions.summarise.all.levels <- regions.summarise %>% 
  filter(LEVEL == 'Total')

#side by side bar plot with point ratio#side by side bar plot with point ratiowwt.km3.y
var1 <- regions.summarise.all.levels$wwt.km3.y
var2 <- regions.summarise.all.levels$wwp.km3.2015
var3 <- regions.summarise.all.levels$ratio.treated.2015
x <- regions.summarise.all.levels$REGION_WB_SHORT

plot.df <- data.frame(var1,var2,var3,x) %>% 
  # sort your dataframe
  arrange(desc(var3)) %>%          
  # reset your factor-column based on that order
  mutate(x = factor(x, unique(x))) %>%  
  select(-var3)

colnames(plot.df) <- c('Treated wastewater',
                       'Produced wastewater',
                       'Subregion')

plot.df.melted <- melt(plot.df, id.vars = 'Subregion')

plot.df2 <- data.frame(x, var3)
colnames(plot.df2) <- c('Subregion',
                        'var3')

scaling.factor.vol <- 1.2 #scaling for second variable

p.vol <- ggplot(plot.df.melted %>% 
                  mutate(variable = factor(variable,
                                levels = c(
                                  'Produced wastewater',
                                  'Treated wastewater'
                                ))), aes(x=Subregion, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  geom_point(data = plot.df2 %>% 
               mutate(var = 'Ratio treated (%)'), aes(x=Subregion, 
                                                  y=var3 * scaling.factor.vol,
                                                  color = var),
             inherit.aes = F) +
  geom_hline(
    yintercept = mean(regions.summarise.all.levels$ratio.treated.2015) * scaling.factor.vol,
    linetype = 'dashed') +
  ggtitle('A. Wastewater volumes (2015)') +
  xlab('Subregion')+
  ylab('km<sup>3</sup> y<sup>-1</sup> <br />') +
  scale_y_continuous(
    sec.axis = sec_axis(~./scaling.factor.vol, name = "<br /> Ratio treated (%)")) +
  scale_fill_manual(values = c("#c09120", "#58809C")) +
  scale_color_manual(values = '#000000') +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5)) 
# p.vol

#### plot 2 -> Volumes treated per wastewater treatment level
regions.summarise.p2 <- regions.summarise %>% 
  filter(LEVEL != 'Total')

factor.p2.table <- regions.summarise.all.levels %>% 
  arrange(desc(wwt.km3.y))
factor.p2.table$REGION_WB_SHORT <- factor(
  factor.p2.table$REGION_WB_SHORT, levels = factor.p2.table$REGION_WB_SHORT)
factor.p2 <- factor.p2.table$REGION_WB_SHORT

p2.treated.volumes.levels <- ggplot(regions.summarise.p2 %>% 
               #reorder treatment levels
               mutate(LEVEL = factor(LEVEL,levels = c(
               'Primary', 'Secondary', 'Advanced'))) %>% 
               # reorder subregions
               mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT, factor.p2)), 
             aes(x=REGION_WB_SHORT, y=wwt.km3.y, fill=LEVEL)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values = c("#F0027F", "#BEAED4", "#54A29B"), guide='none')+
  ggtitle('B. Treated volumes by level (2015)') +
  xlab('Subregion')+
  ylab('<br /> km<sup>3</sup> y<sup>-1</sup> <br />')+
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5))  +
  scale_y_continuous(position = "right")

#### energy plot 
regions.summarise.p3 <- regions.summarise 

factor.p3.table <- regions.summarise.all.levels %>% 
  arrange(desc(twh.y.mean))
factor.p3.table$REGION_WB_SHORT <- factor(
  factor.p3.table$REGION_WB_SHORT, levels = factor.p3.table$REGION_WB_SHORT)
factor.p3 <- factor.p3.table$REGION_WB_SHORT

p.energy <- ggplot(data = regions.summarise.p3 %>% 
                     #reorder treatment levels
                     mutate(LEVEL = factor(LEVEL,levels = c(
                       'Total', 'Advanced', 'Secondary', 'Primary' ))) %>% 
                     # reorder subregions
                     mutate(REGION_WB_SHORT = factor(REGION_WB_SHORT, factor.p2)), 
                   aes(x=REGION_WB_SHORT, y=twh.y.mean, fill=LEVEL)) +
  geom_bar(stat="identity", position='dodge') +
  geom_errorbar(aes(ymin=twh.y.low, ymax=twh.y.high), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("#c6c507", "#54A29B", "#BEAED4", "#F0027F"))+
  theme_light() +
  xlab('Subregion') +
  ylab("TWh y<sup>-1</sup>") +
  ggtitle('<br /> C. Energy consumption of wastewater treatment (2015)') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.title = element_blank(),
        legend.position = 'bottom')


#### patch ####
p.patch <- (p.vol | p2.treated.volumes.levels) / p.energy +
  plot_layout(guides='collect') &
  theme(legend.position="bottom",
        legend.direction = 'vertical',
        legend.spacing = unit(1,"cm"),
        legend.text = element_markdown(size=11),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_markdown(size = 13),
        axis.text = element_text(size=10),
        plot.title = element_markdown(hjust=0.5, size=16)
  )

#### save things ####
ggsave(paste0(outputDir,'wwt_vol_energy.png'), p.patch,
       height=10, width=10, units='in', dpi=300)

write.csv(regions.summarise, paste0(outputDirTables, 'energy_regions_2015_wwt.csv'),
          row.names = F)

file.show(paste0(outputDir,'wwt_vol_energy.png'))