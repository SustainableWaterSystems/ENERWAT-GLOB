rm(list=ls())

library(dplyr)
library(RANN)
library(geosphere)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

inputDir <- '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
inputDirElevation <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/1_segments/'
inputDirHeads <- '../../../../input/global_data/ibwt/Shah2025_GloHydroRes/'

outputDir <- paste0(inputDir, '2_infrastructure/')
dir.create(outputDir, recursive = T, showWarnings = F)

#### load 
shah2025 <- read.csv(paste0(inputDirHeads, 'GloHydroRes_vs1.csv'))

data.sections <- read.csv(paste0(inputDir,'0_timeseries_raw/1_0_bias_correction_sections.csv'))
data.segments <- read.csv(paste0(inputDir,'0_timeseries_raw/1_1_bias_correction_segments.csv'))

power.intakes <- data.segments %>% 
  filter(intake.type == 'Power station')

# write.csv(power.intakes, paste0(outputDir, '1_information_hydropower.csv'), row.names = F)


#### process ####
#get coordinates from Jignesh data
shah2025.coordinates <- shah2025 %>%
  select(plant_lon, plant_lat) %>%
  filter(!is.na(plant_lon))

nearest.list <- list()

for(i in seq(nrow(power.intakes))){

  #selecy hydropower to match from ibwts
  hydropower.select <- power.intakes[i,]

  #get coordinates of selected hydropower
  point.coordinates <- data.frame(hydropower.select$intake.lon,
                                  hydropower.select$intake.lat)
  colnames(point.coordinates) <- colnames(shah2025.coordinates)

  #make dataframe
  matching.df <- rbind(point.coordinates, shah2025.coordinates)

  #find closest point
  rann.matches <- data.frame(nn2(matching.df, k=10)[[1]])

  match.id <- rann.matches$X2[1]

  shah.matched <- shah2025[match.id,] %>%
    select(ID:plant_type, year, dam_height_m, head_m)

  #### calculate distance between matched points
  # model.distance <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  model.distance <- distm(c(shah.matched$plant_lon, shah.matched$plant_lat),
                          c(point.coordinates$plant_lon, point.coordinates$plant_lat),
                          fun = distHaversine)

  hydropower.select.head <- hydropower.select %>%
    mutate(id.shah2025 = shah.matched$ID,
           plant.type = shah.matched$plant_type,
           capacity.mw = shah.matched$capacity_mw,
           dam.height = shah.matched$dam_height_m,
           head.m = shah.matched$head_m,

           # match.distance = model.distance,
           country.shah = shah.matched$country)

  nearest.list[[i]] <- hydropower.select.head

}

hydropower.heads <- do.call(rbind, nearest.list)

####  if head does is not reported use dam height ####
heads.shah <- hydropower.heads %>%
  filter(!is.na(dam.height) | !is.na(head.m)) %>%
  mutate(head.available = 1)

heads.shah$head.m[is.na(heads.shah$head.m)] <- heads.shah$dam.height[is.na(heads.shah$head.m)]

#### if bot head and dam height dont exist -> use elevation profile ####
heads.nas <- hydropower.heads %>%
  filter(is.na(dam.height) & is.na(head.m))  %>%
  mutate(head.available = 0)

n.countries <- unique(heads.nas$Country)

list.countries <- list()

for(i in seq(length(n.countries))){

  country <- n.countries[i]

  n.transfers.df <- heads.nas %>%
    filter(Country == country)

  n.transfers <- unique(n.transfers.df$transfer.name)

  list.transfers <- list()

  #### loop per transfer
  for(j in seq(length(n.transfers))){

    transfer.name.j <- n.transfers[j]

    #### get elevation data of all transfer segments ####
    segments.folder <- paste0(inputDirElevation, country, '/',
                              transfer.name.j, '/')

    n.segments.df <- n.transfers.df %>%
      filter(transfer.name == transfer.name.j)

    n.segments <- n.segments.df$segment.full

    list.segments <- list()

    for(k in seq(length(n.segments))){

      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name.j, ' - ',
                            'Segment: ', k, '/', length(n.segments)))

      #### segment info
      segment.info <- n.segments.df[k,]

      #### get elevation lift and bind
      segment.elevation.data <- read.csv(paste0(segments.folder, 'segment_',
                                                n.segments[k], '.csv'))

      intake.elevation <- segment.elevation.data$elevation.aster.m[
        which(!is.na(segment.elevation.data$Subject))
      ]

      # elevation.max <- max(segment.elevation.data$elevation.aster.m)
      elevation.min <- min(segment.elevation.data$elevation.aster.m)

      hydropower.head <- intake.elevation - elevation.min

      list.segments[[k]] <- hydropower.head

    }

    segments.pumping.df <- do.call(rbind, list.segments)

    list.transfers[[j]] <- segments.pumping.df

  }

  country.transfers.df <- do.call(rbind, list.transfers)

  list.countries[[i]] <- country.transfers.df

}

heads.elevation.values <- data.frame(do.call(rbind, list.countries))
colnames(heads.elevation.values) <- 'head.m'

#### add heads from elevation to heads dataframe
heads.nas$head.m <- heads.elevation.values$head.m

#### bind dataframes
hydropower.heads.complete <- rbind(heads.shah, heads.nas)
write.csv(hydropower.heads.complete, paste0(outputDir, '0_information_hydropower.csv'), row.names = F)
