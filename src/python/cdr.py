
# coding: utf-8
import pandas as pd
import json
import numpy
import math
from pandas.io.json import json_normalize


def df_to_geojson(df, properties):
    """
    A function that changes Pandas dataFrame to Geojson.
    
    Parameters
    ----------
    df : DataFrame
       This is a joined dataframe between CDR data and a grid data.
    properties : list
       Column names of CDR properties.

    Returns
    -------
    geojson : dict
    """
    geojson = {'crs': {'type': 'name', 
                       'properties': {"name": "urn:ogc:def:crs:EPSG::4326"}}, 
               'type': 'FeatureCollection', 'features': []}
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


def join_cdr_grid(cdr, grid):
    """
    A function that outputs a joined table (dict) from CDR and GRID data.
    
    Parameters
    ----------
    cdr : pandas DataFrame
       CDR data loaded in a pandas DataFrame.
    grid : pandas Series
       A pandas Series data of a city grid.

    Returns
    -------
    geojson : dict
    """
    

    cdr.columns = ["cellId", "time", "countryCode", "smsIn", "smsOut",
                   "callIn", "callOut", "internet"]
    norm_grid = json_normalize(grid['features'])

    agg_df = cdr[cdr['countryCode'] != 0].groupby('cellId').agg({
            'cellId': 'first',
            'time': 'first',
            'smsIn': 'sum',
            'smsOut': 'sum',
            'callIn': 'sum',
            'callOut': 'sum',
            'internet': 'sum'
        })


    merged = pd.merge(left=norm_grid, right=agg_df, left_on='properties.cellId', right_on='cellId')

    cols = ['cellId','smsIn','smsOut','callIn','callOut','internet']
    geojson = df_to_geojson(merged, cols)
    return geojson
