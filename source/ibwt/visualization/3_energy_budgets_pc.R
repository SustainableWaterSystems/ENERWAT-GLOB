#### plot timeseries of transfers that have both pumping stations and hydropower plants

rm(list=ls())

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(cowplot)
library(lubridate)

palette.budget <- c('#648FFF','#FE6100','#E3A43E')

inputDir <- '../../../output/ibwt/model/'

inputDirDischarge <- paste0(inputDir, '1_discharge_timeseries/')

inputDirEnergy <- paste0(inputDir, '2_energy/')

outputDirViz <- '../../../output/ibwt/visualization/'

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

transfer.title <- c('California State Water Project',
                    'Snowy Mountains Scheme (Murray)',
                    'Tagus-Segura Transfer')

# shared pieces so the low-high band looks the same in every panel:
# - fill matches the series colour (instead of default grey)
# - color = NA switches off the ribbon outline, which was drawing two extra
#   lines (ymin and ymax) per series and made the bands look like error bars
ribbon.band <- geom_ribbon(aes(ymin = twh.low, ymax = twh.high, fill = data.type),
                           alpha = 0.2, color = NA, show.legend = FALSE)

scale.color <- scale_color_manual(values = palette.budget)
scale.fill <- scale_fill_manual(values = palette.budget, guide = 'none')

scale.color.labelled <- scale_color_manual(values = palette.budget,
                                           labels = c('Energy production',
                                                      'Energy consumption',
                                                      'Net energy budget'))

legend.override <- guides(color = guide_legend(override.aes = list(size = 3, shape = 22, linewidth = 5)))

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
    ribbon.band +
    ggtitle(transfer.title[i]) +
    xlab('Year') +
    ylab('TWh month<sup>-1</sup>') +
    scale.color +
    scale.fill +
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
    ribbon.band +
    ggtitle(transfer.title[i]) +
    xlab('Year') +
    ylab('TWh y<sup>-1</sup>') +
    scale_y_continuous(position = "right") +
    scale.color +
    scale.fill +
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
                                filter(transfer.name == budget.transfers[3]), 
                              aes(x=datetime, y=twh.mean, color=data.type)) +
  ribbon.band +
  geom_line(linewidth=0.7) +
  ggtitle(transfer.title[3]) +
  xlab('Year') +
  ylab('TWh month<sup>-1</sup>') +
  scale.color.labelled +
  scale.fill +
  # scale_y_continuous(position = "right") +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank()
  ) +
  legend.override

plot.segura.yearly <- ggplot(plot.data.monthly %>% 
                               filter(transfer.name == budget.transfers[3])%>% 
                               mutate(datetime= floor_date(datetime, 'year')) %>% 
                               group_by(datetime, data.type, transfer.name) %>% 
                               summarise(twh.low = sum(twh.low),
                                         twh.mean = sum(twh.mean),
                                         twh.high = sum(twh.high)),
                             
                             aes(x=datetime, y=twh.mean, color=data.type)) +
  ribbon.band +
  geom_line(linewidth=0.7) +
  ggtitle(transfer.title[3]) +
  xlab('Year') +
  ylab('TWh y<sup>-1</sup>') +
  scale_y_continuous(position = "right") +
  scale.color.labelled +
  scale.fill +
  theme_bw() +
  theme(axis.title.y = element_markdown(),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank()
  ) +
  legend.override

p.patch <- (plots.monthly.list[[1]] + plots.yearly.list[[1]]) /
  (plots.monthly.list[[2]] + plots.yearly.list[[2]]) /
  (plot.segura.monthly + plot.segura.yearly  + 
     plot_layout(guides = "collect") &
     theme(legend.position = 'bottom',
           legend.text = element_markdown(size=14))) &
  theme(axis.title = element_markdown(size=14))

ggsave(paste0(outputDirViz, 'energy_budget_net.png'), p.patch,
       height=10, width=10, units='in', dpi=300, bg='white')

file.show(paste0(outputDirViz, 'energy_budget_net.png'))


#### summaries ####
swp <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[1]) %>% 
  filter(data.type == 'net.energy')

# laa <- plot.data.yearly %>% 
#   filter(transfer.name == budget.transfers[2]) %>% 
#   filter(data.type == 'net.energy')

sms1 <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[2]) %>% 
  filter(data.type == 'net.energy')

# sms2 <- plot.data.yearly %>% 
#   filter(transfer.name == budget.transfers[4])%>% 
#   filter(data.type == 'net.energy')

tst <- plot.data.yearly %>% 
  filter(transfer.name == budget.transfers[3]) %>% 
  filter(data.type == 'net.energy')

#### 
mean(swp$twh.low)
mean(swp$twh.mean)
mean(swp$twh.high)


mean(sms1$twh.low)
mean(sms1$twh.mean)
mean(sms1$twh.high)


mean(tst$twh.low)
mean(tst$twh.mean)
mean(tst$twh.high)

####
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