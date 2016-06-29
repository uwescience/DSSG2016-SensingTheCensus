
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import csv

file = '../../data/CDR/generated/similarity.csv'
with open(file) as f:
    cosines = pd.read_csv(f)

print('hi')

cosines['Date'] = pd.to_datetime(cosines['Date'], format='%m%d')
print (cosines['Date'])
ax = cosines.plot(x='Date', grid=True, figsize=(12, 8))
ax.xaxis.grid(True, which='minor')
ax.yaxis.grid(True, which='minor')
print('hi2')
plt.show()
