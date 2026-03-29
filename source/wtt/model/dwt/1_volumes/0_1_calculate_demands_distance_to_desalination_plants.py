import pandas as pd
import os

import warnings
warnings.filterwarnings("ignore")

################################################ HAVERSINE FUNCTION
from numpy import cos, sin, arcsin, sqrt
from math import radians

def haversine(row, lon_plant, lat_plant):
    lon1 = lon_plant
    lat1 = lat_plant
    lon2 = row['lon']
    lat2 = row['lat']
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * arcsin(sqrt(a)) 
    km = 6371 * c
    return km
##################################################

############ PROGRESS BAR ###########
def progress_bar(current, total, bar_length=20):
    fraction = current / total

    arrow = int(fraction * bar_length - 1) * '-' + '>'
    padding = int(bar_length - len(arrow)) * ' '

    ending = '\n' if current == total else '\r'

    print(f'Progress: [{arrow}{padding}] {int(fraction*100)}%', end=ending)
##################################################

inputDirWorld = '../../../../../input/global_data/'
inputDirDWT = '../../../../../input/wtt/dwt/'
inputDirTreatment = '../../../../../output/wtt/model/dwt/0_population/3_output_clean/'
inputDirDesal = '../../../../../output/wtt/model/dwt/1_volumes/0_drinking_desalination/'

#### files ####
countries_5arcmin = pd.read_csv(
  inputDirWorld + 'general/countries_raster_5arcmin.csv', low_memory=False)

population_treatment_5arcmin_2015 = pd.read_csv(
  inputDirTreatment + 'population_treatment_5arcmin_SAFE.csv'
)

water_demands_2015 = pd.read_csv(
  inputDirDWT + 'domestic_demands_2015_tot_m3.csv'
)

desalination_5arcmin_2015 = pd.read_csv(
  inputDirDesal + 'desaldata_drinking_5arcmin_2015.csv'
)


#### filtering inputs ####
#### get demands for countries with drinking water treatment
treatment_countries = sorted(set(population_treatment_5arcmin_2015['Country']))

treatment_demands = water_demands_2015.loc[water_demands_2015['demands.m3.2015'] != 0]

treatment_demands_countries_info = pd.merge(treatment_demands, countries_5arcmin)

treatment_demands_input = treatment_demands_countries_info.loc[treatment_demands_countries_info['Country'].isin(treatment_countries)]

#### get desalination gridcells
desalination_nonnull = desalination_5arcmin_2015.loc[desalination_5arcmin_2015['drinking.desalination.m3.y'] != 0]

desalination_gridcells = pd.merge(desalination_nonnull, countries_5arcmin)

desalination_gridcells_input = desalination_gridcells.loc[desalination_gridcells['Country'].isin(treatment_countries)]

desalination_gridcells_input = desalination_gridcells_input.reset_index(drop = True)

#### processing distances ####
desalination_countries = sorted(set(desalination_gridcells_input['Country']))

for country in range(0, len(desalination_countries)):
    
    country_select = desalination_countries[country]

    outputDirCreate = inputDirDesal + '0_desalination_distances/' + country_select + '/'

    if not os.path.exists(outputDirCreate):
      os.makedirs(outputDirCreate)


#### loop per plant and filter by country everytime
# get coordinates of selected plant

for idx in range(0, len(desalination_gridcells_input)):
  
  print('\rCalculating distance from plant... ' + str(idx), '/', str(len(desalination_gridcells_input)))

  plant_cell = desalination_gridcells_input.loc[idx]

  #get demands of the country in which the plant is
  country_demands = treatment_demands_input.loc[treatment_demands_input['Country'] == plant_cell['Country']]
  # print(country_demands)

  #calculate distance
  country_demands['distance'] = country_demands.apply(lambda row: haversine(row, plant_cell['lon'], plant_cell['lat']), axis=1)

  #set outputdirectory of country and save dataframe using plant name
  outputDir = inputDirDesal + '0_desalination_distances/' + plant_cell['Country'] + '/'
  country_demands.to_csv(outputDir + 'distance_from_plant_' + str(plant_cell['cell_ID']) + '.csv', index=False)