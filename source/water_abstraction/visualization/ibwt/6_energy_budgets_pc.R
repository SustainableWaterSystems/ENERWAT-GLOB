#### plot timeseries of transfers that have both pumping stations and hydropower plants

rm(list=ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(cowplot)
library(lubridate)

palette.budget <- c('#648FFF','#FE6100','#E3A43E')
# palette.budget <- c('#E56529', '#117733','#1E81B0')

inputDir <- '../../../../output/water_abstraction/model/ibwt/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

inputDirEnergy <- paste0(inputDir, '2_energy/')

outputDirViz <- '../../../../output/water_abstraction/visualization/ibwt/'

#### load ####
ibwt.budgets.monthly <- read.csv(paste0(inputDirEnergy, '4_energy_budget_monthly.csv')) %>% 
  mutate(datetime = as.Date(datetime))
ibwt.budgets.yearly <- read.csv(paste0(inputDirEnergy, '4_energy_budget_yearly.csv')) %>% 
  mutate(datetime = as.Date(datetime))

#### process ####
#### yearly
df.production.yearly <- ibwt.budgets.yearly %>% 
  filter(transfer.type == 'Energy mixed') %>% 
  select(datetime, transfer.name, twh.low.p, twh.mean.p, twh.high.p) %>% 
  rename(twh.low = twh.low.p,
         twh.mean = twh.mean.p,
         twh.high = twh.high.p) %>% 
  mutate(data.type = 'production')

df.consumption.yearly <- ibwt.budgets.yearly %>% 
  filter(transfer.type == 'Energy mixed') %>%
  select(datetime, transfer.name, twh.low.c, twh.mean.c, twh.high.c) %>% 
  rename(twh.low = twh.low.c,
         twh.mean = twh.mean.c,
         twh.high = twh.high.c) %>% 
  mutate(data.type = 'consumption')

df.net.yearly <- ibwt.budgets.yearly %>% 
  filter(transfer.type == 'Energy mixed') %>%
  select(datetime, transfer.name, twh.low.net, twh.mean.net, twh.high.net) %>% 
  rename(twh.low = twh.low.net,
         twh.mean = twh.mean.net,
         twh.high = twh.high.net) %>% 
  mutate(data.type = 'net.energy')

#### monthly
df.production.monthly <- ibwt.budgets.monthly %>% 
  filter(transfer.type == 'Energy mixed') %>%
  select(datetime, transfer.name, twh.low.p, twh.mean.p, twh.high.p) %>% 
  rename(twh.low = twh.low.p,
         twh.mean = twh.mean.p,
         twh.high = twh.high.p) %>% 
  mutate(data.type = 'production')

df.consumption.monthly <- ibwt.budgets.monthly %>% 
  filter(transfer.type == 'Energy mixed') %>%
  select(datetime, transfer.name, twh.low.c, twh.mean.c, twh.high.c) %>% 
  rename(twh.low = twh.low.c,
         twh.mean = twh.mean.c,
         twh.high = twh.high.c) %>% 
  mutate(data.type = 'consumption')

df.net.monthly <- ibwt.budgets.monthly %>% 
  filter(transfer.type == 'Energy mixed') %>%
  select(datetime, transfer.name, twh.low.net, twh.mean.net, twh.high.net) %>% 
  rename(twh.low = twh.low.net,
         twh.mean = twh.mean.net,
         twh.high = twh.high.net) %>% 
  mutate(data.type = 'net.energy')


#### plot data ####
plot.data.yearly <- rbind(df.production.yearly,
                          df.consumption.yearly,
                          df.net.yearly) %>% 
  mutate(data.type = factor(data.type, levels=c('production',
                                      'consumption',
                                      'net.energy')))

plot.data.monthly <- rbind(df.production.monthly,
                          df.consumption.monthly,
                          df.net.monthly) %>% 
  mutate(data.type = factor(data.type, levels=c('production',
                                                'consumption',
                                                'net.energy')))

#### plot ####
budget.transfers <- unique(plot.data.yearly$transfer.name)

plots.monthly.list <- list()
plots.yearly.list <- list()


for(i in seq(length(budget.transfers))){
  
  budget.monthly.select <- plot.data.monthly %>% 
    filter(transfer.name == budget.transfers[i])
  
  budget.yearly.select <- budget.monthly.select %>% 
    mutate(datetime= floor_date(datetime, 'year')) %>% 
    group_by(datetime, data.type, transfer.name) %>% 
    summarise(twh.low = sum(twh.low),
              twh.mean = sum(twh.mean),
              twh.high = sum(twh.high))
  
  p.select.monthly <- ggplot(budget.monthly.select, aes(x=datetime, y=twh.mean, color=data.type)) +
    geom_line(linewidth=0.7) +
    geom_ribbon(aes(x=datetime, 
                    ymin=twh.low, 
                    ymax=twh.high,
                    color=data.type),
                alpha=0.2, 
                inherit.aes=F, show.legend=F) +
    ggtitle(budget.transfers[i]) +
    xlab('Year') +
    ylab('TWh month<sup>-1</sup>') +
    scale_color_manual(values = palette.budget) +
    theme_bw() +
    theme(axis.title.y = element_markdown(),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none',
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank()
    )
  
  p.select.yearly <- ggplot(budget.yearly.select, aes(x=datetime, y=twh.mean, color=data.type)) +
    geom_line(linewidth=0.7) +
    geom_ribbon(aes(x=datetime, 
                    ymin=twh.low, 
                    ymax=twh.high,
                    color=data.type),
                alpha=0.2, 
                inherit.aes=F, show.legend=F) +
    ggtitle(budget.transfers[i]) +
    xlab('Year') +
    ylab('TWh y<sup>-1</sup>') +
    scale_y_continuous(position = "right") +
    scale_color_manual(values = palette.budget) +
    theme_bw() +
    theme(axis.title.y = element_markdown(),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none',
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank()
    )
  
  plots.monthly.list[[i]] <- p.select.monthly
  plots.yearly.list[[i]] <- p.select.yearly
  
}

plot.segura.monthly <- ggplot(plot.data.monthly %>% 
                        filter(transfer.name == budget.transfers[5]), 
                      aes(x=datetime, y=twh.mean, color=data.type)) +
  geom_line(linewidth=0.7) +
  geom_ribbon(aes(x=datetime, 
                  ymin=twh.low, 
                  ymax=twh.high,
                  color=data.type),
              alpha=0.2, 
              inherit.aes=F, show.legend=F) +
  ggtitle(budget.transfers[5]) +
  xlab('Year') +
  ylab('TWh month<sup>-1</sup>') +
  scale_color_manual(values = palette.budget,
                     labels = c('Energy production','Energy consumption','Net energy')) +
  # scale_y_continuous(position = "right") +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank()
  ) +
  guides(color=guide_legend(override.aes = list(size = 3, shape = 22, linewidth = 5)))

plot.segura.yearly <- ggplot(plot.data.monthly %>% 
                                filter(transfer.name == budget.transfers[5])%>% 
                               mutate(datetime= floor_date(datetime, 'year')) %>% 
                               group_by(datetime, data.type, transfer.name) %>% 
                               summarise(twh.low = sum(twh.low),
                                         twh.mean = sum(twh.mean),
                                         twh.high = sum(twh.high)),
                             
                              aes(x=datetime, y=twh.mean, color=data.type)) +
  geom_line(linewidth=0.7) +
  geom_ribbon(aes(x=datetime, 
                  ymin=twh.low, 
                  ymax=twh.high,
                  color=data.type),
              alpha=0.2, 
              inherit.aes=F, show.legend=F) +
  ggtitle(budget.transfers[5]) +
  xlab('Year') +
  ylab('TWh y<sup>-1</sup>') +
  scale_y_continuous(position = "right") +
  scale_color_manual(values = palette.budget,
                     labels = c('Energy production','Energy consumption','Net energy')) +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank()
  ) +
  guides(color=guide_legend(override.aes = list(size = 3, shape = 22, linewidth = 5)))

p.patch <- (plots.monthly.list[[1]] + plots.yearly.list[[1]]) /
  (plots.monthly.list[[2]] + plots.yearly.list[[2]]) /
  (plots.monthly.list[[3]] + plots.yearly.list[[3]]) /
  (plots.monthly.list[[4]] + plots.yearly.list[[4]]) /
  ((plot.segura.monthly + plot.segura.yearly)  + 
     plot_layout(guides = "collect") &
     theme(legend.position = 'bottom',
           legend.text = element_markdown(size=14))) &
  theme(axis.title = element_markdown(size=14))
 
ggsave(paste0(outputDirViz, 'energy_budget_net.png'), p.patch,
       height=14, width=10, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz, 'energy_budget_net.png'))


#### summaries ####
swp <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[1])

laa <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[2])

sms1 <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[3])

sms2 <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[4])

tst <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[5])

transfer.summary.yearly <- plot.data.yearly %>% 
  group_by(transfer.name, data.type) %>% 
  summarise(
    pos = sum(twh.mean > 0),
    neg = sum(twh.mean < 0 ),
    twh.low = min(twh.low),
            twh.mean = min(twh.mean),
            twh.high = min(twh.high))
  
transfer.summary.monthly <- plot.data.monthly %>% 
  group_by(transfer.name, data.type) %>% 
  summarise(
    pos = sum(twh.mean > 0),
    neg = sum(twh.mean < 0 ),
    twh.low = mean(twh.low),
    twh.mean = mean(twh.mean),
    twh.high = mean(twh.high))



tagus.segura.check <- plot.data.monthly %>% 
  filter(transfer.name == budget.transfers[5]) %>% 
  mutate(datetime=floor_date(datetime, unit='year')) %>% 
  group_by(datetime, data.type) %>% 
  summarise(twh.mean = sum(twh.mean))

ggplot(tagus.segura.check, aes(x=datetime, y=twh.mean, color=data.type))+
  geom_line()
