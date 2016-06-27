
# coding: utf-8

# In[9]:

import pandas as pd
import json


# In[27]:

df = pd.read_csv('../../data/CDR/sms-call-internet-mi-2013-11-01.txt', delimiter='\t',
                 header=None) 


# In[20]:

file = '../CDR_process/milano-grid.geojson'
with open(file) as f:
    grid = pd.read_json(f, typ='DataFrame')


# In[29]:

df.columns = ["cellID", "time", "countryCode", "smsIn", "smsOut", "callIn", "callOut", "internet"]


# In[47]:

for cell in grid["features"]:
    if 'sms_in' not in cell['properties']:
        cell['properties']['sms_in'] = df[(df["cellID"] == cell['properties']['cellId']) & (df["countryCode"] != 0)]['smsIn'].sum()
    if 'sms_out' not in cell['properties']:
        cell['properties']['sms_out'] = df[(df["cellID"] == cell['properties']['cellId']) & (df["countryCode"] != 0)]['smsOut'].sum()
    if 'call_in' not in cell['properties']:
        cell['properties']['call_in'] = df[(df["cellID"] == cell['properties']['cellId']) & (df["countryCode"] != 0)]['callIn'].sum()
    if 'call_out' not in cell['properties']:
        cell['properties']['call_out'] = df[(df["cellID"] == cell['properties']['cellId']) & (df["countryCode"] != 0)]['callOut'].sum()
    if 'internet' not in cell['properties']:
        cell['properties']['internet'] = df[(df["cellID"] == cell['properties']['cellId']) & (df["countryCode"] != 0)]['internet'].sum()    


# In[ ]:

grid.head()


# In[31]:

df.head()


# In[ ]:



