library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)


#### files ####
inputDirWorld <- '../../../../input/global_data/'
inputDirData <- '../../../../output/wtt/model/'
outputDir <- '../../../../output/wtt/visualization/general/'
dir.create(outputDir, recursive = T, showWarnings = F)

#### global data
world.regions <- read.csv(paste0(inputDirWorld, 'general/countries_id_regions.csv'))

electricity.2015.countries <- read.csv(paste0(
  inputDirWorld, 'energy/eia_electricity_net_consumption_TWh_2015.csv'))
primary.energy.2015.countries <- read.csv(paste0(
  inputDirWorld, 'energy/eia_total_energy_consumption_2015.csv'))

population.jan2016 <- read.csv(paste0(inputDirWorld, 'general/un_country_population_jan2016.csv'))
gdp.2015 <-  read.csv(paste0(inputDirWorld, 'general/un_country_gdp_total_2015.csv'))

ratios.desal <- read.csv(paste0(inputDirData, 'desalination/2_energy/energy_countries_2015_desalination.csv'))
ratios.wwt <- read.csv(paste0(inputDirData, 'wwt/1_energy/energy_countries_2015_wwt.csv'))
ratios.dwt <- read.csv(paste0(inputDirData, 'dwt/2_energy/energy_countries_2015_dwt.csv'))


#### process ####
#summarise regional energy consumption (primary and electricity)

energy.2015.countries <- inner_join(electricity.2015.countries,
                                    primary.energy.2015.countries)

energy.2015.countries.regions <- inner_join(energy.2015.countries,
                                            world.regions) %>% 
  group_by(REGION_WB_SHORT) %>%
  summarise(X2015.net.electricity.twh = sum(X2015.net.electricity.twh),
            X2015.primary.quadbtu = sum(X2015.primary.quadbtu),
            X2015.primary.ej = sum(X2015.primary.ej),
            X2015.primary.twh = sum(X2015.primary.twh)
  )

population.jan2016.regions <- inner_join(population.jan2016,
                                         world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(population.jan2016 = sum(population.jan2016))

gdp.2015.regions <- inner_join(gdp.2015 %>% na.omit(),
                               world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(GDP.2015.usd = sum(GDP.2015.usd))


#### group all regional data in one dataframe ####
data.regions <- inner_join(energy.2015.countries.regions,
                           population.jan2016.regions) %>% 
  inner_join(., gdp.2015.regions) 

#summarise technologies 
regions.summarise.desal <- ratios.desal %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high)
  ) %>% 
  mutate(tech = 'Desalination') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100) %>% 
  #### HERE ADDING KWH CAPITA AND INNER JOINING WITH GDP AND GDP / CAPITA ####
  inner_join(., data.regions) %>% 
  #### twh = 10^9 kwh 
  mutate(energy.capita.low = twh.y.low * 10e9 / population.jan2016,
         energy.capita.mean = twh.y.mean * 10e9 / population.jan2016,
         energy.capita.high = twh.y.high * 10e9 / population.jan2016,
         energy.gdp.low = twh.y.low * 10e9 / GDP.2015.usd,
         energy.gdp.mean = twh.y.mean * 10e9 / GDP.2015.usd,
         energy.gdp.high = twh.y.high * 10e9 / GDP.2015.usd
  )

regions.summarise.wwt <- ratios.wwt %>% 
  filter(LEVEL == 'All') %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(twh.y.low),
            twh.y.mean = sum(twh.y.mean),
            twh.y.high = sum(twh.y.high)
  ) %>% 
  mutate(tech = 'Wastewater treatment') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100) %>% 
  #### HERE ADDING KWH CAPITA AND INNER JOINING WITH GDP AND GDP / CAPITA ####
  inner_join(., data.regions) %>% 
  #### twh = 10^9 kwh 
  mutate(energy.capita.low = twh.y.low * 10e9 / population.jan2016,
         energy.capita.mean = twh.y.mean * 10e9 / population.jan2016,
         energy.capita.high = twh.y.high * 10e9 / population.jan2016,
         energy.gdp.low = twh.y.low * 10e9 / GDP.2015.usd,
         energy.gdp.mean = twh.y.mean * 10e9 / GDP.2015.usd,
         energy.gdp.high = twh.y.high * 10e9 / GDP.2015.usd
  )

regions.summarise.dwt <- ratios.dwt %>% 
  inner_join(., world.regions) %>% 
  group_by(REGION_WB_SHORT) %>% 
  summarise(twh.y.low = sum(energy.total.low.twh),
            twh.y.mean = sum(energy.total.mean.twh),
            twh.y.high = sum(energy.total.high.twh)
  ) %>% 
  mutate(tech = 'Conventional drinking water treatment') %>% 
  inner_join(., energy.2015.countries.regions) %>% 
  mutate(ratio.primary.energy.low = twh.y.low / X2015.primary.twh * 100,
         ratio.primary.energy.mean = twh.y.mean / X2015.primary.twh * 100,
         ratio.primary.energy.high = twh.y.high / X2015.primary.twh * 100,
         ratio.electricity.low = twh.y.low / X2015.net.electricity.twh * 100,
         ratio.electricity.mean = twh.y.mean / X2015.net.electricity.twh * 100,
         ratio.electricity.high = twh.y.high / X2015.net.electricity.twh * 100) %>% 
  #### HERE ADDING KWH CAPITA AND INNER JOINING WITH GDP AND GDP / CAPITA ####
  inner_join(., data.regions) %>% 
  #### twh = 10^9 kwh 
  mutate(energy.capita.low = twh.y.low * 10e9 / population.jan2016,
         energy.capita.mean = twh.y.mean * 10e9 / population.jan2016,
         energy.capita.high = twh.y.high * 10e9 / population.jan2016,
         energy.gdp.low = twh.y.low * 10e9 / GDP.2015.usd,
         energy.gdp.mean = twh.y.mean * 10e9 / GDP.2015.usd,
         energy.gdp.high = twh.y.high * 10e9 / GDP.2015.usd
  )


#### prepare plot data ####
#ratio to primary energy
#ratio to electricity
#energy per capita
#energy per $gdp

#desalination
var1 <- data.frame(regions.summarise.desal$ratio.primary.energy.low,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var2 <- data.frame(regions.summarise.desal$ratio.primary.energy.mean,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var3 <- data.frame(regions.summarise.desal$ratio.primary.energy.high,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')

var4 <- data.frame(regions.summarise.desal$ratio.electricity.low,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var5 <- data.frame(regions.summarise.desal$ratio.electricity.mean,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var6 <- data.frame(regions.summarise.desal$ratio.electricity.high,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')

var7 <- data.frame(regions.summarise.desal$energy.capita.low,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var8 <- data.frame(regions.summarise.desal$energy.capita.mean,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var9 <- data.frame(regions.summarise.desal$energy.capita.high,
                   regions.summarise.desal$tech,
                   regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')

var10 <- data.frame(regions.summarise.desal$energy.gdp.low,
                    regions.summarise.desal$tech,
                    regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var11 <- data.frame(regions.summarise.desal$energy.gdp.mean,
                    regions.summarise.desal$tech,
                    regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var12 <- data.frame(regions.summarise.desal$energy.gdp.high,
                    regions.summarise.desal$tech,
                    regions.summarise.desal$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')

#wwt
var13 <- data.frame(regions.summarise.wwt$ratio.primary.energy.low,
                   regions.summarise.wwt$tech,
                   regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var14 <- data.frame(regions.summarise.wwt$ratio.primary.energy.mean,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var15 <- data.frame(regions.summarise.wwt$ratio.primary.energy.high,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')

var16 <- data.frame(regions.summarise.wwt$ratio.electricity.low,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var17 <- data.frame(regions.summarise.wwt$ratio.electricity.mean,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var18 <- data.frame(regions.summarise.wwt$ratio.electricity.high,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')

#### add energy consumption / capita  
var19 <- data.frame(regions.summarise.wwt$energy.capita.low,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var20 <- data.frame(regions.summarise.wwt$energy.capita.mean,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var21 <- data.frame(regions.summarise.wwt$energy.capita.high,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')

var22 <- data.frame(regions.summarise.wwt$energy.gdp.low,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var23 <- data.frame(regions.summarise.wwt$energy.gdp.mean,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var24 <- data.frame(regions.summarise.wwt$energy.gdp.high,
                    regions.summarise.wwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')

#cdwt
var25 <- data.frame(low = regions.summarise.dwt$ratio.primary.energy.low,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var26 <- data.frame(mean = regions.summarise.dwt$ratio.primary.energy.mean,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')
var27 <- data.frame(high = regions.summarise.dwt$ratio.primary.energy.high,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Primary energy')

var28 <- data.frame(low = regions.summarise.dwt$ratio.electricity.low,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var29 <- data.frame(mean = regions.summarise.dwt$ratio.electricity.mean,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')
var30 <- data.frame(high = regions.summarise.dwt$ratio.electricity.high,
                    tech = regions.summarise.dwt$tech,
                    regions.summarise.wwt$REGION_WB_SHORT) %>%
  mutate(type = 'Electricity')

var31 <- data.frame(regions.summarise.dwt$energy.capita.low,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var32 <- data.frame(regions.summarise.dwt$energy.capita.mean,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')
var33 <- data.frame(regions.summarise.dwt$energy.capita.high,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per capita')

var34 <- data.frame(regions.summarise.dwt$energy.gdp.low,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var35 <- data.frame(regions.summarise.dwt$energy.gdp.mean,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')
var36 <- data.frame(regions.summarise.dwt$energy.gdp.high,
                    regions.summarise.dwt$tech,
                    regions.summarise.dwt$REGION_WB_SHORT) %>%
  mutate(type = 'Energy per $gdp')

#desal
colnames(var1)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var2)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var3)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var4)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var5)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var6)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var7)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var8)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var9)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var10)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var11)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var12)[1:3] <- c('high', 'Technology', 'Subregion')

#wwt
colnames(var13)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var14)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var15)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var16)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var17)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var18)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var19)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var20)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var21)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var22)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var23)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var24)[1:3] <- c('high', 'Technology', 'Subregion')

#drinking water treatment
colnames(var25)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var26)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var27)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var28)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var29)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var30)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var31)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var32)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var33)[1:3] <- c('high', 'Technology', 'Subregion')
colnames(var34)[1:3] <- c('low', 'Technology', 'Subregion')
colnames(var35)[1:3] <- c('mean', 'Technology', 'Subregion')
colnames(var36)[1:3] <- c('high', 'Technology', 'Subregion')


df.low <- data.frame(rbind(var1, var4, var7, var10, var13, var16, 
                           var19, var22, var25, var28, var31,var34))
df.mean <- data.frame(rbind(var2, var5, var8, var11, var14, var17, 
                            var20, var23,var26, var29, var32, var35))
df.high <- data.frame(rbind(var3, var6, var9, var12, var15, var18, 
                            var21, var24, var27, var30, var33, var36))

df.plot <- cbind(df.low, df.mean$mean, df.high$high)
colnames(df.plot)[5:6] <- c('mean','high')

df.plot.cleaned <- df.plot %>%
  relocate(mean, .after=low) %>%
  relocate(high, .after=mean) %>%
  mutate(Technology = factor(Technology,
                             levels = c('Desalination',
                                        'Wastewater treatment',
                                        'Conventional drinking water treatment')))

#### plot ####
df.primary <- df.plot.cleaned %>%
  filter(type == 'Primary energy')

df.primary.order <- df.primary %>%
  group_by(Subregion) %>%
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>%
  arrange(desc(sum.mean))

df.electricity <- df.plot.cleaned %>%
  filter(type == 'Electricity')

df.electricity.order <- df.electricity %>%
  group_by(Subregion) %>%
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>%
  arrange(desc(sum.mean))

df.energy.capita <- df.plot.cleaned %>%
  filter(type == 'Energy per capita')

df.energy.capita.order <- df.energy.capita %>%
  group_by(Subregion) %>%
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>%
  arrange(desc(sum.mean))

df.energy.gdp <- df.plot.cleaned %>%
  filter(type == 'Energy per $gdp')

df.energy.gdp.order <- df.energy.gdp %>%
  group_by(Subregion) %>%
  summarise(sum.low = sum(low),
            sum.mean = sum(mean),
            sum.high = sum(high)) %>%
  arrange(desc(sum.mean))

p.primary <- ggplot(data = df.primary %>%
                      mutate(Subregion = factor(
                        Subregion, levels = df.primary.order$Subregion)) ,
                    aes(x=Subregion, y=mean, color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  theme_light() +
  ylab('Ratio (%)\n') +
  ggtitle(label = 'A. Primary energy',
          subtitle = '% of total regional consumption') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE'), name = 'Technology') +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  guides(fill = guide_legend(override.aes = list(linetype = 0), title.position = 'top'),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 13))

p.electricity <- ggplot(data = df.electricity %>%
                          mutate(Subregion = factor(
                            Subregion, levels = df.electricity.order$Subregion)), 
                        aes(x=Subregion, y=mean,
                            color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  theme_light() +
  ylab('Ratio (%)\n') +
  scale_y_continuous(position = "right") +
  ggtitle(label = 'B. Electricity',
          subtitle = '% of total regional consumption') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE'), name = 'Technology') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE'), name = 'Technology') +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  guides(fill = guide_legend(override.aes = list(linetype = 0), title.position = 'top'),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title.x = element_blank(),
        axis.title.y  = element_text(size = 13,
                                         margin = margin(t = 0, r = 0, b = 0, l = 30)))

p.energy.capita <- ggplot(data = df.energy.capita %>%
                            mutate(Subregion = factor(
                              Subregion, levels = df.energy.capita.order$Subregion)), 
                          aes(x=Subregion, y=mean,
                              color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  theme_light() +
  ylab('kWh capita<sup>-1</sup> y<sup>-1</sup>') +
  ggtitle(label = '\nC. Energy consumption per capita') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE'), name = 'Technology') +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  guides(fill = guide_legend(override.aes = list(linetype = 0), title.position = 'top'),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title.x  = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                     size = 13),
        axis.title.y  = element_markdown(size = 13))

p.energy.gdp <- ggplot(data = df.energy.gdp %>%
                             mutate(Subregion = factor(
                               Subregion, levels = df.energy.gdp.order$Subregion)) ,
                           aes(x=Subregion, y=mean, color = Technology, fill=Technology)) +
  geom_bar(aes(linetype = Technology), stat="identity", position = 'dodge') +
  geom_errorbar(aes(ymin=low, ymax=high), width=.2,
                position=position_dodge(.9)) +
  theme_light() +
  ylab('kWh USD<sup>-1</sup> y<sup>-1</sup>') +
  scale_y_continuous(position = "right") + 
  ggtitle(label = '\nD. Energy consumption per USD GDP') +
  scale_fill_manual(values = c('#FE6100', '#DDCC77', '#88CCEE'), name = 'Technology') +
  scale_color_manual(values = c('#000000', '#000000', '#000000')) +
  scale_linetype_manual(values = c("blank", "blank", 'blank')) +
  guides(fill = guide_legend(override.aes = list(linetype = 0), title.position = 'top'),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme(axis.title.x  = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                     size = 13),
        axis.title.y  = element_markdown(size = 13,
                                         margin = margin(t = 0, r = 0, b = 0, l = 30)))

p.patch <- (p.primary | p.electricity ) / (p.energy.capita | p.energy.gdp)  +
  plot_annotation(
    title = 'Regional energetic toll of water treatment processes (2015) \n') +
  plot_layout(guides='collect') &
  theme(legend.position="bottom",
        legend.spacing = unit(7,"cm"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 10),
        legend.direction = 'horizontal',
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14, hjust = 0.5,
                                    margin = unit(c(0, 0, 4, 0), "mm"))
  )


#### save plot
ggsave(paste0(outputDir,'water_treatment_energy_ratio_regions.png'), p.patch,
       height=10, width=10, units='in', dpi=300)

file.show(paste0(outputDir ,'water_treatment_energy_ratio_regions.png'))


#### save regional results
write.csv(regions.summarise.desal,
          paste0(inputDirData, 'desalination/2_energy/energy_regions_2015_desalination.csv'), 
          row.names = F)
write.csv(regions.summarise.wwt,
          paste0(inputDirData, 'wwt/1_energy/energy_regions_2015_wwt.csv'),
          row.names = F)
write.csv(regions.summarise.dwt,
          paste0(inputDirData, 'dwt/2_energy/energy_regions_2015_dwt.csv'),
          row.names = F)