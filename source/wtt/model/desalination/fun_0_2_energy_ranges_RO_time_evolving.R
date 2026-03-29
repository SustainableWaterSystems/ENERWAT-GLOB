#### salinities ####

TDS.pure.low <- 0
TDS.river.low <- 0.5
TDS.brackish.low <- 3
TDS.seawater.low <- 20
TDS.brine.low <- 50
TDS.wastewater.low <- ((TDS.river.low) + (TDS.brackish.low)) / 2 

TDS.pure.high <- 0.5
TDS.river.high <- 3
TDS.brackish.high <- 20
TDS.seawater.high <- 50
TDS.brine.high <- 262 #max brine concentration is 26.2% at 25 C
TDS.wastewater.high <- ((TDS.river.high) + (TDS.brackish.high)) / 2

TDS.pure.mean <- (TDS.pure.low + TDS.pure.high) / 2
TDS.river.mean <- (TDS.river.low + TDS.river.high) / 2 
TDS.brackish.mean <- (TDS.brackish.low + TDS.brackish.high) / 2
TDS.seawater.mean <- (TDS.seawater.low + TDS.seawater.high) / 2
TDS.brine.mean <- (TDS.brine.low + TDS.brine.high) / 2
TDS.wastewater.mean <- (TDS.wastewater.low + TDS.wastewater.high) / 2


#### read the data ####

#ro data to get minimum year
RO.year.min <- min(RO$Online.date)
RO.year.max <- max(RO$Online.date)
RO.years.all <- seq(as.Date(RO.year.min), as.Date(RO.year.max), by="years")


#### set vector of indexes to fit
year.index = seq(1, length(RO.years.all))
year.vect <- cbind.data.frame(year.index, RO.years.all)
colnames(year.vect) <- c('year.index','Year')

#points to fit
points.literature <- read.csv(paste0(
  inputDir, 'RO_time_evolving_points_literature.csv')) %>% 
  mutate(Year=as.Date(paste(Year, 1, 1, sep = "-"))) %>% 
  inner_join(., year.vect) %>% 
  mutate(Source = factor(
    Source, levels=c(" Elimelech & Philip (2011) - SWRO (Fit)",  
                     " Liu et al. (2016) - SWRO",
                     " Liu et al. (2016) - BWRO")))

points.elimelech <- points.literature %>% 
  filter(Source == " Elimelech & Philip (2011) - SWRO (Fit)")

####-------fit exponential decay model-------####
# exponential decay with lower asymptote = 1.06 ? or 2?
exp.decay.3 <- drm(Energy ~ year.index, data = points.elimelech,
                   fct=EXD.3(fixed = c(1.06, NA, NA), names = c("c","d", "e")))
summary(exp.decay.3)


####----calculate new values----####
predict.seawater <- as_tibble(predict(exp.decay.3, year.vect,
                           interval = "confidence", level = 0.95)) %>%
  mutate(Year = year.vect$Year,
         Feedwater = 'Seawater') 

predict.pure <- predict.seawater %>% 
  mutate(Prediction = Prediction * TDS.pure.mean / TDS.seawater.mean) %>% 
  mutate(Lower = Lower * TDS.pure.low / TDS.seawater.low) %>% 
  mutate(Upper = Upper * TDS.pure.high / TDS.seawater.high) %>% 
  mutate(Feedwater = 'Pure')

predict.river <- predict.seawater %>% 
  mutate(Prediction = Prediction * TDS.river.mean / TDS.seawater.mean) %>% 
  mutate(Lower = Lower * TDS.river.low / TDS.seawater.low) %>% 
  mutate(Upper = Upper * TDS.river.high / TDS.seawater.high) %>% 
  mutate(Feedwater = 'River')

predict.wastewater <- predict.seawater %>% 
  mutate(Prediction = Prediction * TDS.wastewater.mean / TDS.seawater.mean) %>% 
  mutate(Lower = Lower * TDS.wastewater.low / TDS.seawater.low) %>% 
  mutate(Upper = Upper * TDS.wastewater.high / TDS.seawater.high) %>% 
  mutate(Feedwater = 'Wastewater')

predict.brackish <- predict.seawater %>% 
  mutate(Prediction = Prediction * TDS.brackish.mean / TDS.seawater.mean) %>% 
  mutate(Lower = Lower * TDS.brackish.low / TDS.seawater.low) %>% 
  mutate(Upper = Upper * TDS.brackish.high / TDS.seawater.high) %>% 
  mutate(Feedwater = 'Brackish')

predict.brine <- predict.seawater %>% 
  mutate(Prediction = Prediction * TDS.brine.mean / TDS.seawater.mean) %>% 
  mutate(Lower = Lower * TDS.brine.low / TDS.seawater.low) %>% 
  mutate(Upper = Upper * TDS.brine.high / TDS.seawater.high) %>% 
  mutate(Feedwater = 'Brine')


#### add plant-wide uncertainties #### 
energy.data <- rbind(predict.pure,
                     predict.river,
                     predict.wastewater,
                     predict.brackish,
                     predict.seawater,
                     predict.brine) %>%
  mutate(Feedwater = factor(Feedwater, levels=c('Pure', 'River', 'Wastewater',
                                                'Brackish', 'Seawater', 'Brine'))) 

energy.data$Lower[energy.data$Feedwater == 'Pure'] <-
  energy.data$Lower[energy.data$Feedwater == 'Pure'] + 0.6
energy.data$Prediction[energy.data$Feedwater == 'Pure'] <-
  energy.data$Prediction[energy.data$Feedwater == 'Pure'] + 1.3
energy.data$Upper[energy.data$Feedwater == 'Pure'] <-
  energy.data$Upper[energy.data$Feedwater == 'Pure'] + 2.0

energy.data$Lower[energy.data$Feedwater == 'River'] <-
  energy.data$Lower[energy.data$Feedwater == 'River'] + 0.6
energy.data$Prediction[energy.data$Feedwater == 'River'] <-
  energy.data$Prediction[energy.data$Feedwater == 'River'] + 1.3
energy.data$Upper[energy.data$Feedwater == 'River'] <-
  energy.data$Upper[energy.data$Feedwater == 'River'] + 2.0

energy.data$Lower[energy.data$Feedwater == 'Wastewater'] <-
  energy.data$Lower[energy.data$Feedwater == 'Wastewater'] + 0.6
energy.data$Prediction[energy.data$Feedwater == 'Wastewater'] <-
  energy.data$Prediction[energy.data$Feedwater == 'Wastewater'] + 1.3
energy.data$Upper[energy.data$Feedwater == 'Wastewater'] <-
  energy.data$Upper[energy.data$Feedwater == 'Wastewater'] + 2.0

energy.data$Lower[energy.data$Feedwater == 'Brackish'] <-
  energy.data$Lower[energy.data$Feedwater == 'Brackish'] + 0.6
energy.data$Prediction[energy.data$Feedwater == 'Brackish'] <-
  energy.data$Prediction[energy.data$Feedwater == 'Brackish'] + 1.3
energy.data$Upper[energy.data$Feedwater == 'Brackish'] <-
  energy.data$Upper[energy.data$Feedwater == 'Brackish'] + 2.0

energy.data$Lower[energy.data$Feedwater == 'Seawater'] <-
  energy.data$Lower[energy.data$Feedwater == 'Seawater'] + 1.3
energy.data$Prediction[energy.data$Feedwater == 'Seawater'] <-
  energy.data$Prediction[energy.data$Feedwater == 'Seawater'] + 4.0
energy.data$Upper[energy.data$Feedwater == 'Seawater'] <-
  energy.data$Upper[energy.data$Feedwater == 'Seawater'] + 6.7

energy.data$Lower[energy.data$Feedwater == 'Brine'] <- 
  energy.data$Lower[energy.data$Feedwater == 'Brine'] + 1.3
energy.data$Prediction[energy.data$Feedwater == 'Brine'] <-
  energy.data$Prediction[energy.data$Feedwater == 'Brine'] + 4.0
energy.data$Upper[energy.data$Feedwater == 'Brine'] <-
  energy.data$Upper[energy.data$Feedwater == 'Brine'] + 6.7