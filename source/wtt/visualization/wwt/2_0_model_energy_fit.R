library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)

#### load ####
inputDir <- '../../../../input/wtt/wwt/'
outputDir <- '../../../../output/wtt/visualization/wwt/'

longo.data <- read.csv(paste0(inputDir, 'Longo2016_secondary_wwt.csv'))
colnames(longo.data)
tech.count <- longo.data %>% 
  group_by(Secondary.treatment..) %>% 
  summarise(tech=n())


#### multiple linear regression ####
#fit mlr using log
mlr.model.ro <- lm(log(Total.electricity.consumption..kWh.d.) ~ 
                     log(Average.flowrate..m3.d.) +  log(Influent..COD..g.d),
                     # log(Influent..COD..g.d) + log(Effluent..COD..g.d),
                     # log(Influent..COD..g.d) + COD..removed..ratio,
                     # COD..removed..g.d,
                   data=longo.data)
summary(mlr.model.ro)$coef

#predict values
predict.wwt.energy <- as_tibble(predict(mlr.model.ro, longo.data, 
                                       interval = "confidence", level = 0.95)) 

df.energy <- cbind(longo.data, predict.wwt.energy)

#calculate r2 and p-value
rsq <- function (x, y) cor(x, y) ^ 2
rsq(df.energy$Total.electricity.consumption..kWh.d., exp(df.energy$fit))
cor.test(df.energy$Total.electricity.consumption..kWh.d., exp(df.energy$fit)) 

#### plot predicted vs. observed ####
plotData <- df.energy

#countries
fit.plot.countries <- ggplot(plotData,aes(x=Total.electricity.consumption..kWh.d.,
                                          y=exp(fit)))+
  geom_point(aes(color=Country), size=2.5) +
  geom_abline (slope=1, linetype = "dashed") +
  ylab('Predicted (kWh d<sup>-1</sup>)<br />') +
  xlab('<br />Observed (kWh d<sup>-1</sup>)<br />') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_color_manual(values=c("#000000", "#C0E442", "#56B4E9","#0072B2",
                              "#009E73", "#F0E442", "#E69F00",
                              "#D55E00", "#CC79A7","#9C79A7")) +
  theme_light() +
  guides(color=guide_legend(title="Country", ncol=3,
                            override.aes = list(size = 3))) +
  theme(axis.title = element_markdown(lineheight = 1.5, size=20),
        axis.text = element_text(size = 16))

#technologies
fit.plot.tech <- ggplot(plotData,aes(x=Total.electricity.consumption..kWh.d.,
                                     y=exp(fit)))+
  geom_point(aes(color=Secondary.treatment..), size=2.5) +
  geom_abline (slope=1, linetype = "dashed") +
  ylab('Predicted (kWh d<sup>-1</sup>)<br />') +
  xlab('<br />Observed (kWh d<sup>-1</sup>)<br />') +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  scale_color_manual(values=c("#000000", "#C0E442", "#56B4E9","#0072B2",
                              "#009E73", "#F0E442", "#E69F00",
                              "#D55E00", "#CC79A7","#9C79A7")) +
  
  theme_light() +
  theme(axis.title.x = element_markdown(lineheight = 1.5, size=20),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  guides(color=guide_legend(title="Technology", ncol=4,
                            override.aes = list(size = 3)))


#patch
combined <- ( fit.plot.countries + fit.plot.tech ) +
  plot_layout(guides='collect') &
  theme(legend.justification = 'center',
        legend.title = element_text(hjust=0.5, size=20),
        legend.text = element_text(size=18),
        legend.position = 'bottom',
        legend.direction = 'vertical',
        legend.spacing= unit(5.5, 'cm'),
        plot.title=element_text(hjust=0.5),
        legend.key.size = unit(2,"line"))

#save

ggsave(paste0(outputDir,'log_mlr_wwt.png'), combined,
       height=10, width=13, units='in', dpi=300)

file.show(paste0(outputDir,'log_mlr_wwt.png'))
