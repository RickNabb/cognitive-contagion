import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from itertools import groupby

'''
Create a matplotlib histogram, show it, and save it.

:param data: The array of data to make into a histogram.
:param title: The title of the plot.
:param path: The path to save the plot in.
:param bar_tfm_fn: A function to transform bar height in the plot if needed. Set as the identity function as default.
'''
def plot_and_save_histogram(data, title, path, bar_tfm_fn=lambda x: x):
  sorted_rounded_data = sorted(np.round(data, 3))
  data_vals = list(set(sorted_rounded_data))
  freq = [ bar_tfm_fn(len(list(group))) for key, group in groupby(sorted_rounded_data) ]
  freq_series = pd.Series(freq)

  plt.figure(figsize=(8,8))
  ax = freq_series.plot(kind='bar')
  ax.set_title(title)
  ax.set_xticklabels(sorted(data_vals), fontsize=6)

  plt.savefig(path)
  plt.show()

'''
Create a matplotlib histogram, show it, and save it.

:param data: The array of data to make into a histogram.
:param title: The title of the plot.
:param path: The path to save the plot in.
'''
def plot_and_save_series(data, title, path, kind):
  freq_series = pd.Series(data)

  plt.figure(figsize=(8,6))
  ax = freq_series.plot(kind=kind)
  ax.set_title(title)

  plt.savefig(path)
  plt.show()

