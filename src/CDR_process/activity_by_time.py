import sys
sys.path.append('../python/')

import pandas as pd
import imp
import cdr
import json
import os.path
from scipy.spatial.distance import cosine

imp.reload(cdr)

print ("Libraries loaded")

dfs = {}

# Read all the files in Milano
for month in {"11", "12"}:
    for day in range(1, 32):
        to_read = '../../data/CDR/sms-call-internet-mi-2013-' + month + '-' +\
                    str(day).zfill(2) + '.txt'
        
        if os.path.isfile(to_read):
            dfs[month + str(day).zfill(2)] = pd.read_csv(to_read, delimiter='\t', header=None) 
            print ("loaded ", to_read)

print ("Milano files loaded")

file = '../../data/GeoJSON/milano-grid.geojson'
with open(file) as f:
    grid = pd.read_json(f, typ='Series')

print ("Grid file loaded")


m_weekd, d_weekd, e_weekd, m_weeke, d_weeke, e_weeke = cdr.join_cdr_grid_by_time(dfs['1101'], grid)

for key, value in dfs.items():	
	if key != '1101':
		m_weekday, d_weekday, e_weekday, m_weekend, d_weekend, e_weekend = cdr.join_cdr_grid_by_time(value, grid)

		m_weekday.fillna(0, inplace=True)
		d_weekday.fillna(0, inplace=True)
		e_weekday.fillna(0, inplace=True)
		m_weekend.fillna(0, inplace=True)
		d_weekend.fillna(0, inplace=True)
		e_weekend.fillna(0, inplace=True)

		m_weekd = pd.concat([m_weekd, m_weekday])
		d_weekd = pd.concat([m_weekd, d_weekday])
		e_weekd = pd.concat([e_weekd, e_weekday])
		m_weeke = pd.concat([m_weeke, m_weekend])
		d_weeke = pd.concat([d_weeke, d_weekend])
		e_weeke = pd.concat([e_weeke, e_weekend])

		m_weekd= m_weekd.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',
		                        'geometry.coordinates': 'first',
				                'geometry.type': 'first',
				                'id': 'first',
				                'cellId': 'first',
				                'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		d_weekd= d_weekd.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',
		                        'geometry.coordinates': 'first',
		                        'geometry.type': 'first',
		                        'id': 'first',
		                        'cellId': 'first',
		                        'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		e_weekd= e_weekd.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',
		                        'geometry.coordinates': 'first',
			                    'geometry.type': 'first',
			                    'id': 'first',
			                    'cellId': 'first',
			                    'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		m_weeke= m_weeke.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',                        
		                        'geometry.coordinates': 'first',
		                        'geometry.type': 'first',
		                        'id': 'first',
		                        'cellId': 'first',
		                        'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		d_weeke= d_weeke.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',
		                        'geometry.coordinates': 'first',
		                        'geometry.type': 'first',
		                        'id': 'first',
		                        'cellId': 'first',
		                        'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		e_weeke= e_weeke.groupby('properties.cellId').agg({
		                        'properties.cellId': 'first',
		                        'geometry.coordinates': 'first',
		                        'geometry.type': 'first',
		                        'id': 'first',
		                        'cellId': 'first',
		                        'type': 'first',
		                        'smsIn': 'sum',
		                        'smsOut': 'sum',
		                        'callIn': 'sum',
		                        'callOut': 'sum',
		                        'internet': 'sum'})
		
		print("processed", key)

cols = ['cellId','smsIn','smsOut','callIn','callOut','internet']

print ("Join Completed...")


# reorganizing them  to GeoJSON    
m_wd = cdr.df_to_geojson(m_weekd, cols)
d_wd = cdr.df_to_geojson(d_weekd, cols)
e_wd = cdr.df_to_geojson(e_weekd, cols)
m_we = cdr.df_to_geojson(m_weeke, cols)
d_we = cdr.df_to_geojson(d_weeke, cols)
e_we = cdr.df_to_geojson(e_weeke, cols)

# writing the file
output_filename = '../../data/CDR/generated/CDR_morning_weekday.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(m_wd, output_file, indent=2)

output_filename = '../../data/CDR/generated/CDR_day_weekday.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(d_wd, output_file, indent=2)

output_filename = '../../data/CDR/generated/CDR_evening_weekday.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(e_wd, output_file, indent=2)

output_filename = '../../data/CDR/generated/CDR_morning_weekend.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(m_we, output_file, indent=2)

output_filename = '../../data/CDR/generated/CDR_day_weekend.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(d_we, output_file, indent=2)

output_filename = '../../data/CDR/generated/CDR_evening_weekend.geojson'
with open(output_filename, 'w') as output_file:
   json.dump(e_we, output_file, indent=2)

print ("file generated.")


