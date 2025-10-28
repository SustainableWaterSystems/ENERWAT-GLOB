#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 19 14:13:29 2020

@author: jessicaruijsch

Updated on 11 June 2022 
@mikmagni: vectorized functions for parallel

Updated on 19 March 2023 
@mikmagni: other netcdf implementations
"""
#========================================================================
#
# * This script extracts values from all netCDF files in a folder and 
#   outputs the values as a csv file. 
# * The values at different locations are saved in different csv files 
#   in the output file path, with location names indicated at the end of the file names.
#
#========================================================================


import os 
import xarray as xr
import pandas as pd
import numpy as np
from os import listdir
from datetime import date


########################################################################################
def near(array,value):
	
    idx=(np.abs(array-value)).argmin()
    return idx	


def get_latlon():  
	

	#get coordinates of selected point from its pandas dataframe indexes
	xin = df.axes[1].values
	yin = df.axes[0].values
	
	lon = discharge_nc['lon'][:]  	#netcdf lon    
	lat = discharge_nc['lat'][:]	#netcdf lat

	lon_np = lon.to_numpy()
	lat_np = lat.to_numpy()

	get_latlon.ix = near(lon_np, xin)
	get_latlon.iy = near(lat_np, yin)


def get_discharge():

	#get discharge at selected pixel
	discharge_selected = discharge_nc['discharge'][:, get_latlon.iy, get_latlon.ix]
	# print(discharge_selected)

	# PCR-GLOBWB was run for 1979-2023 (select dates in pd.date_range)	
	discharge_df = pd.DataFrame(discharge_selected,columns=['discharge']) #latitude comes before longitude in PCR-GLOBWB netCDF file)
	discharge_df['datetime'] = pd.date_range(start='1/1/1979', end='12/31/2023', freq='MS') #monthly

	# print(discharge_df)

	#put datetime at beginning
	datetime = discharge_df['datetime']
	discharge_df.drop(labels=['datetime'], axis=1,inplace = True)
	discharge_df.insert(0, 'datetime', datetime)

	reservoir_ID = int(j)

	
	discharge_df.to_csv(outputDirTS + 'm3_s_outlet_'+ str(reservoir_ID) + '.csv', index=False, float_format='%.3f')
###################################################################


inputDir = '../../../../input/water_abstraction/ibwt/1_discharge_timeseries/'
inputDirDischarge = inputDir + 'discharge_Steyaert2025/'
outputDir = '../../../../output/water_abstraction/model/ibwt/1_discharge_timeseries/'
outputDirTS = outputDir + '/timeseries/'

if not os.path.exists(outputDirTS):
    os.makedirs(outputDirTS)

#open gridded reservoir IDs to find coordinates of outlets
reservoirs_5arcmin = xr.open_dataset(inputDir + 'outlet_map_geodar_lakes_dams_unique.nc')

#add time dimension for debug 
reservoirs_5arcmin = reservoirs_5arcmin.expand_dims(time=1)

#open dataset containing reservoirs ID related to inter-basin transfers
loc = pd.read_csv(outputDir + 'ibwt_zones.csv', encoding= 'latin')

#get zones of reservoirs (avoid loading discharge .nc everytime)
pcr_zones = loc['zone_ID'].unique()


for i in pcr_zones:

	print(i)

	#open discharge file (depending on zone of inter-basin transfer)
	filename = 'M' + str(i) + '_discharge_monthly_avg_output.nc' #file containing discharge
	discharge_nc = xr.open_dataset(inputDirDischarge + filename)

	#filter dataframe to get reservoirs in selected zone
	loc_filtered = loc.where(loc['zone_ID'] == i).dropna(axis=0, how='all')
	reservoirs_filtered = loc_filtered['reservoir_ID'].unique()


	#loop over reservoirs to find their gridcell and therefore coordinates
	for j in reservoirs_filtered:
		
		print(int(j))

		#get pixel with selected reservoir_ID
		reservoir_pixel = reservoirs_5arcmin['water_body_outlet'].where(reservoirs_5arcmin['water_body_outlet'] == int(j))

		#make dataframe with coordinates as indexes
		df = pd.DataFrame(reservoir_pixel.values[0,:], index=reservoirs_5arcmin.latitude.values, columns = reservoirs_5arcmin.longitude.values)

		#drop nas to only have reservoir_ID and its coordinates
		df = df.dropna(axis=0, how="all").dropna(axis=1, how="all")

		get_latlon()
		get_discharge()








