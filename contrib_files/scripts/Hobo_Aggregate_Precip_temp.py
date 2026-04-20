# -*- coding: utf-8 -*-
"""
Created on Fri Nov 11 09:25:52 2022

@author: jacob
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Change name of datafile here
df = pd.read_csv('contrib_files/data/UpperQ_RG2.csv') #'siesmic_16-11-2025.csv'

# Convert Date column to correct format
df['Date'] = pd.to_datetime(df['Date'])

# Replace every recorded event with 1 (this way it doesn't matter if the precip was recorded as 'Event' or in mm when we deployed the HOBO)
df['Event'].values[df['Event'].values > 0 ] = 1

# Choose aggregation interval and sum over that interval H = hour, D = day - you can look up other options for intervals
aggregate_precip = 0.2*(df.resample('h', on='Date').Event.sum())

aggregate_temp = df.resample('h', on='Date').Temp_C.sum()


# Convert Series to Datafrane to plot


temp_frame = aggregate_temp.to_frame()

precip_frame = aggregate_precip.to_frame() #+ temp_frame['T_DegC']

full_data_frame = pd.DataFrame()

#full_data_frame['Date'] = precip_frame.index.values

full_data_frame['Temp_C'] = temp_frame['Temp_C']

full_data_frame['Precip (mm)'] = precip_frame['Event']


# Double check the sum is correct (Should match final value in original datafile)
total_sum = (precip_frame["Event"].sum()) / 0.2


#### CHANGE THIS EACH TIME SO YOU DON'T OVERWRITE YOUR FILES - use an informative file name (Met station name and date)
# full_data_frame.to_csv("UpperQ_RG2_HOURLY.csv", index=True)

# Plot! Change the title depending on which Met station 
fig, ax = plt.subplots(figsize=(15,10))
ax.set_title("Hourly Precipitation, UpperQ", fontsize=30)
ax.set_xlabel('Date', fontsize=30)
ax.set_ylabel('mm', fontsize=30)

plt.xticks(fontsize=15)
plt.yticks(fontsize=15)

plt.plot(full_data_frame.index.values, full_data_frame['Precip (mm)'])

fig, ax = plt.subplots(figsize=(15,10))

###Change Title of plot!
ax.set_title("Hourly Temperature, UpperQ (RG)", fontsize=30)
ax.set_xlabel('Date', fontsize=30)
ax.set_ylabel('T (degC)', fontsize=30)

plt.xticks(fontsize=15)
plt.yticks(fontsize=15)
plt.plot(full_data_frame.index.values, full_data_frame['Temp_C'])