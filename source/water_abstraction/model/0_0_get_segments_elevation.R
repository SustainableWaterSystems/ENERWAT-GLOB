rm(list=ls())

#### this script uses terra to extract the elevation of a dem along a vector line
library(readxl)
library(dplyr)
library(terra)
library(sf)
library(geosphere)
library(tidyr)
library(stringr)

replaceMessage <- function(x, width = 80)
{
  message("\r", rep(" ", times = width - length(x)), "\r", appendLF = F)
  message(paste0(x, "                                  "), appendLF = F)
}

#set directories
inputDir <- '../../../../input/water_abstraction/ibwt/'

inputDirDEM <- paste0(inputDir, '0_elevation_profiles/ASTER_dem/')
inputDirIBWT <- paste0(inputDir, '0_elevation_profiles/Transfer_segments/')

outputDir <- '../../../../output/water_abstraction/model/ibwt/0_elevation_profiles/0_elevation/'
dir.create(outputDir, showWarnings = F, recursive = T)
 
#### load ####
#file containing name of transfer dems
dem.names <- read_excel(paste0(inputDir, 'dem_names_per_transfer.xlsx'))

ibwts.compiled <- dem.names %>%
  filter(data.complete == 1)

#### loop by country ####
n.countries <- list.dirs(inputDirIBWT, full.names = F, recursive = F)

for(i in seq(length(n.countries))){
  
  country <- n.countries[i]
  
  #create output directory for the country
  outputDirCountry <- paste0(outputDir, country, '/')
  dir.create(outputDirCountry, showWarnings = F, recursive = T)
  
  #### loop by transfers (can be multiple in the same country)
  country.transfers <- list.dirs(paste0(inputDirIBWT, country),
                                 full.names = F, recursive = F)
  
  #### loop per transfer
  for(j in seq(length(country.transfers))){
    
    # j=5
    transfer.name <- country.transfers[j]
    
    #create output directory for the transfer
    outputDirTransfer <- paste0(outputDirCountry, transfer.name, '/')
    dir.create(outputDirTransfer, showWarnings = F, recursive = T)
    
    #find dem name associated with specific transfer
    dem.name.transfer <- dem.names$dem.name[
      dem.names$transfer.name == transfer.name]
    
    #load transfer dem
    dem.raster <- rast(paste0(inputDirDEM, dem.name.transfer, '.tif'))
    
    #### get elevation data of all segments ####
    segments.folder <- paste0(inputDirIBWT, country, '/', 
                              transfer.name, '/', 'segments/')
    
    segments.n <- list.dirs(segments.folder, full.names = F)[-1]
    
    for(k in seq(length(segments.n))){
      
      # k=1
      replaceMessage(paste0('Country: ', country, ' - ',
                            'Transfer: ', transfer.name, ' - ',
                            'Segment: ', k, '/', length(segments.n)))
      
      segment <- segments.n[k]
      
      ibwt.vector.segment <- vect(paste0(
        inputDirIBWT, country, '/', transfer.name, '/',
        'segments/', segment, '/', segment, '.shp'))
      
      #### get elevation data ####
      elevation.data <- terra::extractAlong(dem.raster, ibwt.vector.segment, xy=T)
      colnames(elevation.data)[4] <- 'Country'

      ##### extract other table variables
      #section number
      section.number <- 
        as.numeric(regmatches(segment, gregexpr("[0-9]+", segment)))
      
      section.segment <- str_extract(segment, "[a-z]+")
      
      #length of transfer (m) -> add to table to calculate kwh/km
      segment.length <- perim(ibwt.vector.segment)
      
      #length of section piece (dem resolution, m)
      unit.length <- segment.length / nrow(elevation.data)
      
      #make dummy variable for indexes of section pieces
      id.path <- seq_len(nrow(elevation.data))
      
      #calculate cumulative distance of segment
      distance.cumulative <- data.frame(id.path) %>%
        mutate(unit.distance = unit.length) %>%
        mutate(distance = cumsum(unit.distance))
      
      #tidy dataframe
      profile.df <- data.frame(
        section.id = section.number, #id of transfer's section (reservoirs)
        segment.id = section.segment, #letter of transfer segment (pumping stations)
        segment.full = segment, #full segment name
        point.id = id.path,  #sequential : point id
        lon = elevation.data$x, #coordinates
        lat = elevation.data$y, #coordinates
        elevation.aster.m = elevation.data$Country,  #elevation profile
        distance.km.unit = unit.length / 1000, #unit distance for total cumulative of all segments
        distance.km.total = distance.cumulative$distance / 1000, #cumulative distance of water transfer
        segment.length.km = segment.length / 1000
      )
      
      write.csv(profile.df,  paste0(outputDirTransfer, 'segment_', segment, '.csv'), row.names = F)
      
   
      }
    }
  }
