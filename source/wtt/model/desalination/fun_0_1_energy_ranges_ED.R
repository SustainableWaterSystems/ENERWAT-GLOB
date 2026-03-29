#-----------script that calculates energy ranges for electrodialysis-----------#
#### define salinity boundaries in DesalData ####
#wastewater assumed in between river and brackish

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

#### reference energy use for ed
# ED_low_TDS_low <- 0.7 # 0 ppm ?
# ED_low_TDS_high <- 2.5 # < 2500 ppm
# ED_high_TDS_low <- 2.64 # 2500 ppm
# ED_high_TDS_high <- 5.5 # 5000 ppm

# fit line on three points: 0, 2.5 and 5 g/L
salinity.fit <- c(0, 2.5, 5)
energy.ed.fit <- c(0.7, 2.64, 5.5)

fit.data <- as.data.frame(cbind(salinity.fit, energy.ed.fit))
colnames(fit.data) <- c('salinity', 'energy')

lin.fit.ed <- lm(energy ~ salinity, data = fit.data) 
lin.fit.ed

#extrapolate energy use on desaldata salinity boundaries
# salinity <- as.data.frame(c(0, 0.5, 3, 20, 50, 260))
salinity <- as.data.frame(unique(c(
  
  TDS.pure.low,
  TDS.river.low, 
  TDS.brackish.low,
  TDS.seawater.low,
  TDS.brine.low ,
  TDS.wastewater.low,
  
  TDS.pure.high,
  TDS.river.high,
  TDS.brackish.high,
  TDS.seawater.high,
  TDS.brine.high,
  TDS.wastewater.high,
  
  TDS.pure.mean ,
  TDS.river.mean ,
  TDS.brackish.mean, 
  TDS.seawater.mean ,
  TDS.brine.mean ,
  TDS.wastewater.mean 
  
)))
colnames(salinity) <- c('salinity')

predict.ed.energy <- as_tibble(round2(
  predict(lin.fit.ed, salinity, interval = "confidence", level = 0.95), 3))

predict.ed.energy.salinities <- cbind(salinity, predict.ed.energy)

#### extrapolate linearly on DesalData boundaries
pure.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.pure.low]
pure.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.pure.high]
pure.mean <-  predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.pure.mean]

river.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.river.low]
river.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.river.high]
river.mean <-  predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.river.mean]

brackish.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brackish.low]
brackish.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brackish.high]
brackish.mean <-  predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brackish.mean]

seawater.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.seawater.low]
seawater.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.seawater.high]
seawater.mean <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.seawater.mean]

brine.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brine.low]
brine.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brine.high]
brine.mean <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.brine.mean]

wastewater.low <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.wastewater.low]
wastewater.high <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.wastewater.high]
wastewater.mean <- predict.ed.energy.salinities$fit[
  predict.ed.energy.salinities$salinity == TDS.wastewater.mean]