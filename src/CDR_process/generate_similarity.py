import sys
sys.path.append('../python/')

import pandas as pd
import imp
import cdr
import json
import os.path
from scipy.spatial.distance import cosine

# import matplotlib.pyplot as plt

# matplotlib.style.use('ggplot')
#%matplotlib inline

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

smsIn, smsOut, callIn, callOut, internet = cdr.calculate_cosine_similarity(grid, dfs, '1101')

print ("cosine similarity caculated")


# generating a DataFrame from dicts
df = pd.DataFrame(smsIn,columns=['Date', 'smsIn'])
df.set_index(df['Date'], inplace=True)
df = pd.merge(left=df, right=pd.DataFrame(smsOut, columns=['Date', 'smsOut']), how='left', on="Date")
df = pd.merge(left=df, right=pd.DataFrame(callIn, columns=['Date', 'callIn']), how='left', on="Date")
df = pd.merge(left=df, right=pd.DataFrame(callOut, columns=['Date', 'callOut']), how='left', on="Date")
df = pd.merge(left=df, right=pd.DataFrame(internet, columns=['Date', 'internet']), how='left', on="Date")

output_filename = '../../data/CDR/generated/similarity.csv'
df.to_csv(output_filename, encoding='utf-8', index=False)


#unit testing
# cdrr = pd.read_csv('../../data/CDR/sms-call-internet-mi-2013-12-01.txt', delimiter='\t', header=None)
# file = '../../data/GeoJSON/milano-grid.geojson'

# joined = cdr.join_cdr_grid(cdrr, grid)
# joined.fillna(0,inplace=True)

# for i in range(1,10000):    
#     if i not in joined.id.values:
#         print (i)

