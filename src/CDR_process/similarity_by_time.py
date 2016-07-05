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
# for month in {"11", "12"}:
#     for day in range(1, 32):
for month in {"11"}:
    for day in range(1, 3):
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

m_wd_smsIn = {}
m_wd_smsOut = {}
m_wd_callIn = {}
m_wd_callOut = {}
m_wd_internet = {}
d_wd_smsIn = {}
d_wd_smsOut = {}
d_wd_callIn = {}
d_wd_callOut = {}
d_wd_internet = {}
e_wd_smsIn = {}
e_wd_smsOut = {}
e_wd_callIn = {}
e_wd_callOut = {}
e_wd_internet = {}
m_we_smsIn = {}
m_we_smsOut = {}
m_we_callIn = {}
m_we_callOut = {}
m_we_internet = {}
d_we_smsIn = {}
d_we_smsOut = {}
d_we_callIn = {}
d_we_callOut = {}
d_we_internet = {}
e_we_smsIn = {}
e_we_smsOut = {}
e_we_callIn = {}
e_we_callOut = {}
e_we_internet = {}

m_weekd, d_weekd, e_weekd, m_weeke, d_weeke, e_weeke = cdr.join_cdr_grid_by_time(dfs['1101'], grid)

for key, value in dfs.items():	

	m_weekday, d_weekday, e_weekday, m_weekend, d_weekend, e_weekend = cdr.join_cdr_grid_by_time(value, grid)

	try:
		m_wd_smsIn[key] = 1 - cosine(m_weekd["smsIn"], m_weekday["smsIn"])
		m_wd_smsOut[key] = 1 - cosine(m_weekd["smsOut"], m_weekday["smsOut"])
		m_wd_callIn[key] = 1 - cosine(m_weekd["callIn"], m_weekday["callIn"])
		m_wd_callOut[key] = 1 - cosine(m_weekd["callOut"], m_weekday["callOut"])
		m_wd_internet[key] = 1 - cosine(m_weekd["internet"], m_weekday["internet"])
	except:
		print("weekday morning has exception...")
		raise

	try:
		d_wd_smsIn[key] = 1 - cosine(d_weekd["smsIn"], d_weekday["smsIn"])
		d_wd_smsOut[key] = 1 - cosine(d_weekd["smsOut"], d_weekday["smsOut"])
		d_wd_callIn[key] = 1 - cosine(d_weekd["callIn"], d_weekday["callIn"])
		d_wd_callOut[key] = 1 - cosine(d_weekd["callOut"], d_weekday["callOut"])
		d_wd_internet[key] = 1 - cosine(d_weekd["internet"], d_weekday["internet"])
	except:
		print("weekday afternoon has exception...")

	try:
		e_wd_smsIn[key] = 1 - cosine(e_weekd["smsIn"], e_weekday["smsIn"])
		e_wd_smsOut[key] = 1 - cosine(e_weekd["smsOut"], e_weekday["smsOut"])
		e_wd_callIn[key] = 1 - cosine(e_weekd["callIn"], e_weekday["callIn"])
		e_wd_callOut[key] = 1 - cosine(e_weekd["callOut"], e_weekday["callOut"])
		e_wd_internet[key] = 1 - cosine(e_weekd["internet"], e_weekday["internet"])
	except:
		print("weekday evening has exception...")

	try:
		m_we_smsIn[key] = 1 - cosine(m_weeke["smsIn"], m_weekend["smsIn"])
		m_we_smsOut[key] = 1 - cosine(m_weeke["smsOut"], m_weekend["smsOut"])
		m_we_callIn[key] = 1 - cosine(m_weeke["callIn"], m_weekend["callIn"])
		m_we_callOut[key] = 1 - cosine(m_weeke["callOut"], m_weekend["callOut"])
		m_we_internet[key] = 1 - cosine(m_weeke["internet"], m_weekend["internet"])
	except:
		print("weekend morning has exception...")

	try:
		d_we_smsIn[key] = 1 - cosine(d_weeke["smsIn"], d_weekend["smsIn"])
		d_we_smsOut[key] = 1 - cosine(d_weeke["smsOut"], d_weekend["smsOut"])
		d_we_callIn[key] = 1 - cosine(d_weeke["callIn"], d_weekend["callIn"])
		d_we_callOut[key] = 1 - cosine(d_weeke["callOut"], d_weekend["callOut"])
		d_we_internet[key] = 1 - cosine(d_weeke["internet"], d_weekend["internet"])
	except:
		print("weekend afternoon has exception...")

	try:
		e_we_smsIn[key] = 1 - cosine(e_weeke["smsIn"], e_weekend["smsIn"])
		e_we_smsOut[key] = 1 - cosine(e_weeke["smsOut"], e_weekend["smsOut"])
		e_we_callIn[key] = 1 - cosine(e_weeke["callIn"], e_weekend["callIn"])
		e_we_callOut[key] = 1 - cosine(e_weeke["callOut"], e_weekend["callOut"])
		e_we_internet[key] = 1 - cosine(e_weeke["internet"], e_weekend["internet"])
	except:
		print("weekend evening has exception...")

	print("processed", key)

print ("Join Completed...")


def generate_df_similarity(smsIn, smsOut, callIn, callOut, internet):
	# generating a DataFrame from dicts
	df = pd.DataFrame(smsIn,columns=['Date', 'smsIn'])
	df.set_index(df['Date'], inplace=True)
	df = pd.merge(left=df, right=pd.DataFrame(smsOut, columns=['Date', 'smsOut']), how='left', on="Date")
	df = pd.merge(left=df, right=pd.DataFrame(callIn, columns=['Date', 'callIn']), how='left', on="Date")
	df = pd.merge(left=df, right=pd.DataFrame(callOut, columns=['Date', 'callOut']), how='left', on="Date")
	df = pd.merge(left=df, right=pd.DataFrame(internet, columns=['Date', 'internet']), how='left', on="Date")
	return df

m_wd_smsIn = sorted(m_wd_smsIn.items(), key=lambda s: s[0])
m_wd_smsOut = sorted(m_wd_smsOut.items(), key=lambda s: s[0])
m_wd_callIn = sorted(m_wd_callIn.items(), key=lambda s: s[0])
m_wd_callOut = sorted(m_wd_callOut.items(), key=lambda s: s[0])
m_wd_internet = sorted(m_wd_internet.items(), key=lambda s: s[0])

e_wd_smsIn = sorted(e_wd_smsIn.items(), key=lambda s: s[0])
e_wd_smsOut = sorted(e_wd_smsOut.items(), key=lambda s: s[0])
e_wd_callIn = sorted(e_wd_callIn.items(), key=lambda s: s[0])
e_wd_callOut = sorted(e_wd_callOut.items(), key=lambda s: s[0])
e_wd_internet = sorted(e_wd_internet.items(), key=lambda s: s[0])

d_wd_smsIn = sorted(d_wd_smsIn.items(), key=lambda s: s[0])
d_wd_smsOut = sorted(d_wd_smsOut.items(), key=lambda s: s[0])
d_wd_callIn = sorted(d_wd_callIn.items(), key=lambda s: s[0])
d_wd_callOut = sorted(d_wd_callOut.items(), key=lambda s: s[0])
d_wd_internet = sorted(d_wd_internet.items(), key=lambda s: s[0])

m_we_smsIn = sorted(m_we_smsIn.items(), key=lambda s: s[0])
m_we_smsOut = sorted(m_we_smsOut.items(), key=lambda s: s[0])
m_we_callIn = sorted(m_we_callIn.items(), key=lambda s: s[0])
m_we_callOut = sorted(m_we_callOut.items(), key=lambda s: s[0])
m_we_internet = sorted(m_we_internet.items(), key=lambda s: s[0])

d_we_smsIn = sorted(d_we_smsIn.items(), key=lambda s: s[0])
d_we_smsOut = sorted(d_we_smsOut.items(), key=lambda s: s[0])
d_we_callIn = sorted(d_we_callIn.items(), key=lambda s: s[0])
d_we_callOut = sorted(d_we_callOut.items(), key=lambda s: s[0])
d_we_internet = sorted(d_we_internet.items(), key=lambda s: s[0])

e_we_smsIn = sorted(e_we_smsIn.items(), key=lambda s: s[0])
e_we_smsOut = sorted(e_we_smsOut.items(), key=lambda s: s[0])
e_we_callIn = sorted(e_we_callIn.items(), key=lambda s: s[0])
e_we_callOut = sorted(e_we_callOut.items(), key=lambda s: s[0])
e_we_internet = sorted(e_we_internet.items(), key=lambda s: s[0])

df = generate_df_similarity(m_wd_smsIn, m_wd_smsOut, m_wd_callIn, m_wd_callOut, m_wd_internet)
df.to_csv('../../data/CDR/generated/m_wd_similarity.csv', encoding='utf-8', index=False)
df = generate_df_similarity(d_wd_smsIn, d_wd_smsOut, d_wd_callIn, d_wd_callOut, d_wd_internet)
df.to_csv('../../data/CDR/generated/d_wd_similarity.csv', encoding='utf-8', index=False)
df = generate_df_similarity(e_wd_smsIn, e_wd_smsOut, e_wd_callIn, e_wd_callOut, e_wd_internet)
df.to_csv('../../data/CDR/generated/e_wd_similarity.csv', encoding='utf-8', index=False)
df = generate_df_similarity(m_we_smsIn, m_we_smsOut, m_we_callIn, m_we_callOut, m_we_internet)
df.to_csv('../../data/CDR/generated/m_we_similarity.csv', encoding='utf-8', index=False)
df = generate_df_similarity(d_we_smsIn, d_we_smsOut, d_we_callIn, d_we_callOut, d_we_internet)
df.to_csv('../../data/CDR/generated/d_we_similarity.csv', encoding='utf-8', index=False)
df = generate_df_similarity(e_we_smsIn, e_we_smsOut, e_we_callIn, e_we_callOut, e_we_internet)
df.to_csv('../../data/CDR/generated/e_we_similarity.csv', encoding='utf-8', index=False)

print ("files written.")
