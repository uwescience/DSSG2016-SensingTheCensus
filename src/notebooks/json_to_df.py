
# coding: utf-8
import pandas as pd
import json
import numpy
import math
from pandas.io.json import json_normalize


# a function that changes Pandas dataFrame to Geojson
def df_to_geojson(df, properties):
    geojson = {'crs': {'type': 'name', 'properties': {"name": "urn:ogc:def:crs:EPSG::4326"}}, 'type': 'FeatureCollection', 'features': []}
    for _, row in df.iterrows():
        feature = {'type': 'Feature', 'properties': {}, 
            'id': row['id'],
            'geometry': {'type': 'Polygon', 'coordinates': []}}
        feature['geometry']['coordinates'] = row['geometry.coordinates']
        for prop in properties:
            if math.isnan(row[prop]):
                feature['properties'][prop] = 0
            else:
                feature['properties'][prop] = row[prop]
        geojson['features'].append(feature)
    return geojson


df = pd.read_csv('../../data/CDR/sms-call-internet-mi-2013-11-01.txt', delimiter='\t', header=None) 
file = '../CDR_process/milano-grid.geojson'
with open(file) as f:
    grid = pd.read_json(f, typ='Series')

print ("Data loaded...")

df.columns = ["cellId", "time", "countryCode", "smsIn", "smsOut", "callIn", "callOut", "internet"]
cols = ['cellId','smsIn','smsOut','callIn','callOut','internet']

gg = json_normalize(grid['features'])

agg_df = df[df['countryCode'] != 0].groupby('cellId').agg({        
        'cellId': 'first',
        'time': 'first',
        'smsIn': 'sum',
        'smsOut': 'sum',
        'callIn': 'sum',
        'callOut': 'sum',
        'internet': 'sum'
    })


merged = pd.merge(left=gg, right=agg_df, left_on='properties.cellId', right_on='cellId')
geojson = df_to_geojson(merged, cols)

# writing the file
output_filename = 'joined.geojson'
with open(output_filename, 'w') as output_file:
    json.dump(geojson, output_file, indent=2)

print ("file generated.")