
# coding: utf-8
import pandas as pd
import json
import numpy
import math
from pandas.io.json import json_normalize
from scipy.spatial.distance import cosine


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
    A function that outputs a joined table (DataFrame) from CDR and GRID data.
    
    Parameters
    ----------
    cdr : pandas DataFrame
       CDR data loaded in a pandas DataFrame.
    grid : pandas Series
       A pandas Series data of a city grid.

    Returns
    -------
    joined_df : pandas DataFrame
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


    joined_df = pd.merge(left=norm_grid, right=agg_df, how='left',
                        left_on='properties.cellId', right_on='cellId')
    
    return joined_df


def calculate_cosine_similarity(grid, dfs, reference_day='1101'):
    """
    A function that calculate consine similarity values by day.
    
    Parameters
    ----------
    grid : pandas Series
        A pandas Series data of a city grid.
    dfs : a dict of pandas Series
        Each element of a dict has a key for a reference day (e.g., '1101')
        and a value for a DataFrame of joined table for that day.

    Returns
    -------
    smsIn, smsOut, callIn, callOut, internet : lists 
        Contain {day : consine_similarity} values, sorted by date
    """
    smsIn = {}
    smsOut = {}
    callIn = {}
    callOut = {}
    internet = {}

    reference = join_cdr_grid(dfs[reference_day], grid)
    reference.fillna(0, inplace=True)

    for key, value in dfs.items():
        if key != '1101':
            joined = join_cdr_grid(value, grid)
            joined.fillna(0, inplace=True)
            try:
                smsIn[key] = 1 - cosine(reference["smsIn"], joined["smsIn"])
            except:
                print (key)
                continue
            try:
                smsOut[key] = 1 - cosine(reference["smsOut"], joined["smsOut"])
            except:
                print (key)
                continue
            try:
                callIn[key] = 1 - cosine(reference["callIn"], joined["callIn"])
            except:
                print (key)
                continue
            try:
                callOut[key] = 1 - cosine(reference["callOut"], joined["callOut"])
            except:
                print (key)
                continue
            try:
                internet[key] = 1 - cosine(reference["internet"], joined["internet"])
            except:
                print (key)
                continue
        print("processed", key)

    #sorting
    smsIn = sorted(smsIn.items(), key=lambda s: s[0])
    smsOut = sorted(smsOut.items(), key=lambda s: s[0])
    callIn = sorted(callIn.items(), key=lambda s: s[0])
    callOut = sorted(callOut.items(), key=lambda s: s[0])
    internet = sorted(internet.items(), key=lambda s: s[0])

    return smsIn, smsOut, callIn, callOut, internet
