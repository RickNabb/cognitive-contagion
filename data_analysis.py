from datetime import date, timedelta
import ast
from enum import Enum
from random import *
from utils import *
from statistics import mean, variance, mode
from copy import deepcopy
from plotting import *
from nlogo_colors import *
import itertools
import pandas as pd
import os
from os.path import exists
import numpy as np
from scipy.stats import chi2_contingency, truncnorm, pearsonr
from sklearn.linear_model import LinearRegression
import math
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from nlogo_io import *
from nlogo_graphs import nlogo_saved_graph_to_nx
from messaging import dist_to_agent_brain, believe_message
# import statsmodels.formula.api as smf

SIM_RAW_DATA_DIR = 'D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data'
SIM_DF_DATA_DIR = './data'
ANALYSIS_DATA_DIR = './data/analyses'

# Pulled from https://stackoverflow.com/questions/1060279/iterating-through-a-range-of-dates-in-python
def daterange(start_date, end_date):
  for n in range(int((end_date - start_date).days)):
    yield start_date + timedelta(n)

"""
STATS STUFF
"""

'''
Return a tuple of summary statistical measures given a list of values.

:param l: The list to calculate stats for.
'''
def summary_statistics(l):
  if len(l) >= 2:
    return (mean(l), variance(l), mode(l))
  elif len(l) >= 1:
    return (mean(l), -1, mode(l))
  else:
    return (-1, -1, -1)

"""
Sample a distribution given a specific attribute. This distribution may
also depend on another, and if so, the function recursively calls
itself to return the needed dependency.

:param attr: An attribute from the Attribues enumeration to sample
its approriate distribution in the empirical data.
"""
def random_dist_sample(attr, resolution, given=None):
    return AttributeValues[attr.name]['vals'](resolution)

"""
Sample an attribute with an equal distribution over the values.

:param attr: The attribute to sample - e.g. Attributes.I.
"""
def random_sample(attr):
  rand = int(math.floor(random() * len(list(attr.value))))
  val = list(attr.value)[rand]
  return val.value

def test_random_sample():
  print(random_sample(Attributes.VG))

"""
ANALYSIS FUNCTIONS
"""

def process_multiple_sim_data(path):
  for file in os.listdir(path):
    data = process_sim_data(f'{path}/{file}')
    stats = citizen_message_statistics(data[0], data[1])

def process_nlogo_world_data(path):
  '''
  Read in and process the data that NetLogo produces from BehaviorSpace
  for the state of the model world, which includes global variables at their
  final values at the end of the simulation, the final state of each agent,
  the state of each patch, link, plots, etc.

  For now, we just use this to return global variables, but it can be
  extended to include more.

  :param path: The filepath including filename for data to read in.
  :returns: A dictionary of global variables and their values.
  '''
  f = open(path)
  raw = f.read()
  f.close()
  chunks = raw.split('\n\n')

  global_lines = chunks[2].split('\n')
  global_keys = [ el.replace('"','') for el in global_lines[1].split('","') ]
  global_vals = [ el.replace('"','') for el in global_lines[2].split('","') ]

  global_vars = { global_keys[i]: global_vals[i] for i in range(len(global_keys)) }

  agent_lines = chunks[3].split('\n')
  agent_keys = [ el.replace('"','') for el in agent_lines[1].split(',') ]
  agents_vals = [ [ el.replace('"','') for el in agent_lines[2+i].split(',') ] for i in range(len(agent_lines)-2) ]

  agent_vars = [ { agent_keys[j]: agent_vals[j] for j in range(len(agent_keys)) } for agent_vals in agents_vals ]

  return (global_vars,agent_vars)

'''
Parse a NetLogo chart export .csv file. This requires a single chart export file
and should not be run on an entire world export. This will return a dictionary
of plot points in a data frame, keyed by the pen name.

:param path: The path to the chart .csv file.
'''
def process_chart_data(path):
  f = open(path)
  raw = f.read()
  f.close()
  chunks = raw.split('\n\n')

  model_lines = chunks[1].replace('"','').split('\n')
  model_keys = model_lines[1].split(',')
  model_vals = model_lines[2].split(',')
  model_props = { model_keys[i]: model_vals[i] for i in range(len(model_keys)) }

  prop_lines = chunks[2].replace('"','').split('\n')
  chart_props = {}
  chart_props['color'] = {}
  keys = prop_lines[1].split(',')
  vals = prop_lines[2].split(',')
  for i in range(0, len(keys)):
    chart_props[keys[i]] = vals[i]

  data_sets = {}
  chart_lines = chunks[4].split('\n')
  
  data_set_splits = []
  split_line = chart_lines[0].split(',')
  for i in range(0, len(split_line)):
    el = split_line[i].replace('"','')
    if el != '':
      data_set_splits.append((el, i))
  for split in data_set_splits:
    data_sets[split[0]] = []

  for i in range(1, len(chart_lines)):
    line = chart_lines[i].replace('"','')
    if line == '': continue

    els = line.split(',')
    for j in range(0, len(data_set_splits)):
      split = data_set_splits[j]
      if j+1 == len(data_set_splits):
        data_sets[split[0]].append(els[split[1]:])
      else:
        data_sets[split[0]].append(els[split[1]:data_set_splits[j+1][1]])

  dfs = {}
  for split in data_set_splits:
    # df = pd.DataFrame(data=data_sets[split[0]][1:], columns=data_sets[split[0]][0])
    df = pd.DataFrame(data=data_sets[split[0]][1:], columns=data_sets[split[0]][0])
    del df['pen down?']
    chart_props['color'][split[0]] = df['color'].iloc[0] if len(df['color']) > 0 else 0
    del df['color']
    # del df['x']
    dfs[split[0]] = df

  return (model_props, chart_props, dfs)

'''
Read multiple NetLogo chart export files and plot all of them on a single
Matplotlib plot.

:param in_path: The directory to search for files in.
:param in_filename: A piece of filename that indicates which files to parse
in the process. This should usually be the name of the chart in the NetLogo file.
'''
def process_multi_chart_data(in_path, in_filename='percent-agent-beliefs'):
  props = []
  multi_data = []
  model_params = {}
  file_order = []
  global_vars = []
  globals_to_keep = ['behavior-rand']
  print(f'process_multi_chart_data for {in_path}/{in_filename}')

  if not os.path.isdir(in_path):
    print(f'ERROR: Path not found {in_path}')
    return (-1, -1, -1,-1)
  if len(os.listdir(in_path)) == 0:
    print(f'ERROR: Path contains no files')
    return (-1,-1,-1,-1)

  for file in os.listdir(in_path):
    if in_filename in file and '.swp' not in file and '.swo' not in file:
      data = process_chart_data(f'{in_path}/{file}')
      global_data,agent_data = process_nlogo_world_data(f'{in_path}/{file[0:file.index("_")]}_world.csv')
      globals_reduced = { key: val for key,val in global_data.items() if key in globals_to_keep }
      model_params = data[0]
      props.append(data[1])
      multi_data.append(data[2])
      file_order.append(file)
      global_vars.append(globals_reduced)

  full_data_size = int(model_params['tick-end']) + 1
  means = { pen_name: [] for pen_name in multi_data[0].keys() }
  i = -1
  for data in multi_data:
    i += 1
    for pen_name in data.keys():
      data_vector = np.array(data[pen_name]['y']).astype('float32')

      if len(data_vector) != full_data_size:
        if len(data_vector) > full_data_size:
          data_vector = data_vector[:full_data_size]
        # TODO: This is janky code
        # elif abs(len(data_vector) - full_data_size) <= 5:
        #   data_vector = np.append(data_vector, [ data_vector[-1] for i in range(abs(len(data_vector) - full_data_size)) ])
        else:
          print(f'ERROR parsing multi chart data for pen "{pen_name}" -- data length {len(data_vector)} did not equal number of ticks {full_data_size}')
          continue

      if means[pen_name] == []:
        means[pen_name] = np.array([data_vector])
      else:
        means[pen_name] = np.vstack([means[pen_name], data_vector])

  final_props = props[0]
  props_y_max = np.array([ float(prop['y max']) for prop in props ])
  final_props['y max'] = props_y_max.max()
  return (means, final_props, model_params, global_vars)

def parse_beliefs_over_time_from_global(global_data):
  messages_believed_nlogo = global_data['beliefs-over-time']

def process_message_data(in_path, rand_id):
  '''
  Process message data from the simulation runs: messages heard by agents,
  believed by agents, agent belief data over time, and messages sent by
  institutional agents.

  As of now, the analysis calculates a timeseries of the mean differences
  between agent beliefs at each tick and the messages they hear, and believe
  (two separate timeseries).

  :param in_path: The path to a single param combo run's output
  :param rand_id: The random seed used for filenames to process.

  :return belief_data: A dictionary keyed by tick, where each entry is
   a list of agent beliefs as dictionaries keyed by proposition, values
   as belief value.
  :return messages_heard: A list where each entry is an agent's dictionary
  of messages heard, keyed by tick, valued with a list of message ids.
  :return messages_believed: A list where each entry is an agent's dictionary
  of messages heard, keyed by tick, valued with a list of message ids.
  :return all_messages: A dictionary keyed by message id with the value
  as a dictionary of proposition & belief value encoded into the message.
  '''
  print(f'processing message data for {in_path}/{rand_id}_FILE.json')
  citizen_beliefs_file = open(f'{in_path}/{rand_id}_bel_over_time.json', 'r')
  messages_believed_file = open(f'{in_path}/{rand_id}_messages_believed.json', 'r')
  messages_heard_file = open(f'{in_path}/{rand_id}_messages_heard.json', 'r')
  messages_sent_file = open(f'{in_path}/{rand_id}_messages_sent.json', 'r')

  '''
  Format of citizen_beliefs: [ {'tick': [{'A': 1}}, {'1': {'A': 2}] } ... where each key is a tick, and each array entry in a tick is a citizen ]

  Format of messages_believed: [ [{'tick': [believed], 'tick': [believed], ...}] where each array is a citizen ]
  '''

  belief_data = json.load(citizen_beliefs_file)
  bel_data = {}
  for tick in belief_data:
    bel_data.update(tick)
  belief_data = bel_data

  messages_heard_data = json.load(messages_heard_file)
  messages_heards_data = []
  for cit in messages_heard_data:
    messages_heard = {}
    for entry in cit:
      messages_heard.update(entry)
    messages_heards_data.append(messages_heard)
  messages_heard_data = messages_heards_data

  messages_bel_data = json.load(messages_believed_file)
  messages_belief_data = []
  for cit in messages_bel_data:
    messages_believed = {}
    for entry in cit:
      messages_believed.update(entry)
    messages_belief_data.append(messages_believed)
  messages_bel_data = messages_belief_data

  messages_sent_data = json.load(messages_sent_file)
  # messages_sent = { }
  # for media_messages in messages_sent_data:
  #   messages_sent.update(media_messages)

  citizen_beliefs_file.close()
  messages_believed_file.close()
  messages_heard_file.close()
  messages_sent_file.close()
  return belief_data, messages_heard_data, messages_bel_data, messages_sent_data

def combine_all_messages_no_media(messages_sent):
  all_messages = {}
  for media_name,messages in messages_sent.items():
    all_messages.update(messages)
  return all_messages

def message_exposure_by_belief_multi_analysis(message_multidata):
  '''
  Combines several data structures in the same format as what is returned
  by message_belief_by_belief_analysis into one data structure of the same
  shape.

  :param message_multidata: A list of 4-tuples containing the results
  from running process_message_data on each rand_id in a directory:
  (beliefs, messages_heard, messages_believed, messages_sent)
  '''
  combined_exposure_by_belief = { }
  for message_data in message_multidata:
    beliefs, messages_heard, _, messages_sent = message_data
    exposure_by_belief = message_exposure_by_belief_analysis(beliefs, messages_heard, combine_all_messages_no_media(messages_sent))
    for proposition, agent_belief in exposure_by_belief.items():
      if proposition not in combined_exposure_by_belief:
        combined_exposure_by_belief[proposition] = {}
      beliefs_for_proposition = combined_exposure_by_belief[proposition]
      for belief_value, belief_list in agent_belief.items():
        if belief_value not in beliefs_for_proposition:
          beliefs_for_proposition[belief_value] = []
        beliefs_for_proposition[belief_value].append(belief_list)
  return combined_exposure_by_belief

def message_exposure_by_belief_analysis(beliefs, messages_heard, all_messages):
  '''
  Generates a dictionary of all belief values heard by each belief value
  from 0-6, keyed on belief value 0-6, with values as lists containing
  message belief values. The dictionary is keyed by proposition, so the
  construction has a copy for each proposition.

  :param beliefs: The belief_data object returned from process_message_data
  :param messages_heard: The messages_heard_data object returned from process_message_data
  :param all_messages: A dictionary of messages keyed by id with contents inside as
  another dictionary (the result of combine_all_messages_no_media)
  '''
  # exposure_by_belief = { proposition: { agent_b: { message_b: [] for message_b in range(7) } for agent_b in range(7) } for proposition in beliefs['0'][0] }
  exposure_by_belief = { proposition: { agent_b: [] for agent_b in range(7) } for proposition in beliefs['0'][0] }
  for tick in beliefs:
    beliefs_at_tick = beliefs[tick]
    for agent_id in range(len(beliefs_at_tick)):
      if tick not in messages_heard[agent_id]:
        continue
      agent_beliefs = beliefs_at_tick[agent_id]
      messages_at_tick_for_agent = messages_heard[agent_id][tick]
      messages = [ all_messages[str(message_id)] for message_id in messages_at_tick_for_agent ]
      for proposition,belief_value in agent_beliefs.items():
        for message in messages:
          message_value = message[proposition]
          if int(belief_value) in exposure_by_belief[proposition]:
            exposure_by_belief[proposition][int(belief_value)].append(int(message_value))
          # else:
            # Shrug
  return exposure_by_belief

def message_belief_by_belief_multi_analysis(message_multidata):
  '''
  Combines several data structures in the same format as what is returned
  by message_belief_by_belief_analysis into one data structure of the same
  shape.

  :param message_multidata: A list of 4-tuples containing the results
  from running process_message_data on each rand_id in a directory:
  (beliefs, messages_heard, messages_believed, messages_sent)
  '''
  combined_exposure_by_belief = { }
  for message_data in message_multidata:
    beliefs, _, messages_believed, messages_sent = message_data
    exposure_by_belief = message_belief_by_belief_analysis(beliefs, messages_believed, combine_all_messages_no_media(messages_sent))
    for proposition, agent_belief in exposure_by_belief.items():
      if proposition not in combined_exposure_by_belief:
        combined_exposure_by_belief[proposition] = {}
      beliefs_for_proposition = combined_exposure_by_belief[proposition]
      for belief_value, belief_list in agent_belief.items():
        if belief_value not in beliefs_for_proposition:
          beliefs_for_proposition[belief_value] = []
        beliefs_for_proposition[belief_value].append(belief_list)
  return combined_exposure_by_belief

def message_belief_by_belief_analysis(beliefs, messages_believed, all_messages):
  '''
  Generates a dictionary of all belief values believed by each belief value
  from 0-6, keyed on belief value 0-6, with values as lists containing
  message belief values. The dictionary is keyed by proposition, so the
  construction has a copy for each proposition.

  :param beliefs: The belief_data object returned from process_message_data
  :param messages_believed: The messages_bel_data object returned from process_message_data
  :param all_messages: A dictionary of messages keyed by id with contents inside as
  another dictionary (the result of combine_all_messages_no_media)
  '''
  # exposure_by_belief = { proposition: { agent_b: { message_b: [] for message_b in range(7) } for agent_b in range(7) } for proposition in beliefs['0'][0] }
  exposure_by_belief = { proposition: { agent_b: [] for agent_b in range(7) } for proposition in beliefs['0'][0] }
  for tick in beliefs:
    beliefs_at_tick = beliefs[tick]
    for agent_id in range(len(beliefs_at_tick)):
      if tick not in messages_believed[agent_id]:
        continue
      agent_beliefs = beliefs_at_tick[agent_id]
      messages_at_tick_for_agent = messages_believed[agent_id][tick]
      messages = [ all_messages[str(message_id)] for message_id in messages_at_tick_for_agent ]
      for proposition,belief_value in agent_beliefs.items():
        for message in messages:
          message_value = message[proposition]
          if int(belief_value) in exposure_by_belief[proposition]:
            exposure_by_belief[proposition][int(belief_value)].append(int(message_value))
  return exposure_by_belief

def message_distance_analysis(belief_data, messages_heard_data, messages_bel_data, all_messages):
  ticks = len(belief_data)
  belief_diffs = []
  heard_diffs = []
  for tick in range(ticks):
    belief_diffs_at_tick = np.array([])
    heard_diffs_at_tick = np.array([])
    # print(belief_data[tick])
    belief_data_for_tick = belief_data[str(tick)]
    # heard_data_for_tick = messages_heard_data[tick]
    for cit_id in range(len(belief_data_for_tick)):
      agent_brain = belief_data_for_tick[cit_id]
      agent_malleables = [ bel for bel in agent_brain.keys() ]
      agent_brain['malleable'] = agent_malleables
      messages_believed = []
      messages_heard = []
      
      if str(tick) in messages_bel_data[cit_id].keys():
        messages_believed = messages_bel_data[cit_id][str(tick)]
      if str(tick) in messages_heard_data[cit_id].keys():
        messages_heard = messages_heard_data[cit_id][str(tick)]

      cur_agent_belief = agent_brain
      for message_id in sorted(messages_believed + messages_heard):
        message = all_messages[f'{message_id}']
        diff = dist_to_agent_brain(cur_agent_belief,message) 
        if message_id in messages_heard:
          heard_diffs_at_tick = np.append(heard_diffs_at_tick, diff)
        if message_id in messages_believed:
          belief_diffs_at_tick = np.append(belief_diffs_at_tick, diff)
          cur_agent_belief = believe_message(agent_brain, message, '', 'discrete')
    belief_diffs.append((belief_diffs_at_tick.mean(), belief_diffs_at_tick.var()))
    heard_diffs.append((heard_diffs_at_tick.mean(), heard_diffs_at_tick.var()))
  return { 'believed': np.array(belief_diffs), 'heard': np.array(heard_diffs) }

def message_exposure_by_group_multi_analysis(data_path, graph_path):
  '''
  Gather data about message exposure grouped by media consumer group
  and by media producer who sent the message, for all runs inside a given
  data path.

  :param data_path: The path to the raw simulation input data to gather,
  which should contain message files.
  :param graph_path: The path to the corresponding graph for that simulation
  parameter combination.
  '''
  (citizens, cit_social, media_arr, media_sub_arr) = read_graph(graph_path)

  groups = set([ cit[2] for cit in citizens ])
  media_names = set([ media[1] for media in media_arr ])
  combined_exposure_by_group = { 'A': {
    group: {
      media_name: [] for media_name in media_names
    } for group in groups }
  }

  proxy_filename = 'world.csv'
  file_ids = []
  if os.path.isdir(data_path):
    for file in os.listdir(data_path):
      if proxy_filename in file:
        file_ids.append(file.replace('_world.csv',''))

    for file_id in file_ids:
      data = message_exposure_by_group_analysis(data_path, file_id, graph_path)
      for proposition, group_data in data.items():
        for group, messages_by_media in group_data.items():
          for media_name, messages in messages_by_media.items():
            combined_exposure_by_group[proposition][group][media_name].append(np.array(messages))
    return combined_exposure_by_group
  else:
    print(f'ERROR: Path not found {data_path}')
    return -1

def message_belief_by_group_multi_analysis(data_path, graph_path):
  '''
  Gather data about message belief grouped by media consumer group
  and by media producer who sent the message, for all runs inside a given
  data path.

  :param data_path: The path to the raw simulation input data to gather,
  which should contain message files.
  :param graph_path: The path to the corresponding graph for that simulation
  parameter combination.
  '''
  (citizens, cit_social, media_arr, media_sub_arr) = read_graph(graph_path)

  groups = set([ cit[2] for cit in citizens ])
  media_names = set([ media[1] for media in media_arr ])
  combined_belief_by_group = { 'A': {
    group: {
      media_name: [] for media_name in media_names
    } for group in groups }
  }

  proxy_filename = 'world.csv'
  file_ids = []
  if os.path.isdir(data_path):
    for file in os.listdir(data_path):
      if proxy_filename in file:
        file_ids.append(file.replace('_world.csv',''))

    for file_id in file_ids:
      data = message_belief_by_group_analysis(data_path, file_id, graph_path)
      for proposition, group_data in data.items():
        for group, messages_by_media in group_data.items():
          for media_name, messages in messages_by_media.items():
            combined_belief_by_group[proposition][group][media_name].append(np.array(messages))
    return combined_belief_by_group
  else:
    print(f'ERROR: Path not found {data_path}')
    return -1

def message_exposure_by_group_analysis(data_path, rand_id, graph_path):
  '''
  For a given simulation run data, analyze which consumer agents were
  exposed to which messages and from which media producers.

  :param data_path: Path to a specific set of runs including the graph
  number (so only repetitions from BehaviorSpace).
  :param rand_id: The ID of the run to analyze results for.
  :param graph_path: Path to the graph that generated results for this
  run.

  :return: A dictionary keyed by proposition, with values as dictionaries
  keyed by group name, and those values as dictionaries keyed by media name
  with values as a list of belief values of messages exposed. See the
  exposure_by_group variable.
  '''
  (citizens, cit_social, media_arr, media_sub_arr) = read_graph(graph_path)
  graph = nlogo_saved_graph_to_nx(citizens, cit_social, media_arr, media_sub_arr)

  groups = set([ cit[2] for cit in citizens ])
  media_names = set([ media[1] for media in media_arr ])

  beliefs, messages_heard, messages_believed, messages_sent = process_message_data(data_path, rand_id)
  all_messages = combine_all_messages_no_media(messages_sent)
  message_id_to_name = {}
  for media_name, messages in messages_sent.items():
    for message in messages:
      message_id_to_name[message] = media_name

  exposure_by_group = { proposition: { 
    group: { 
      media_name: [] for media_name in media_names
      } for group in groups
    } for proposition in beliefs['0'][0]
  }
  for tick in beliefs:
    beliefs_at_tick = beliefs[tick]
    for agent_id in range(len(beliefs_at_tick)):
      if tick not in messages_heard[agent_id]:
        continue
      agent_beliefs = beliefs_at_tick[agent_id]
      agent_group = graph.nodes[agent_id]['groups']
      messages_at_tick_for_agent = messages_heard[agent_id][tick]
      # messages = [ all_messages[str(message_id)] for message_id in messages_at_tick_for_agent ]
      for proposition,belief_value in agent_beliefs.items():
        for message_id in messages_at_tick_for_agent:
          message = all_messages[str(message_id)]
          message_value = message[proposition]
          message_sender = message_id_to_name[str(message_id)]
          exposure_by_group[proposition][agent_group][message_sender].append(int(message_value))
  return exposure_by_group

def message_belief_by_group_analysis(data_path, rand_id, graph_path):
  '''
  For a given simulation run data, analyze which consumer agents 
  believed which messages and from which media producers.

  :param data_path: Path to a specific set of runs including the graph
  number (so only repetitions from BehaviorSpace).
  :param rand_id: The ID of the run to analyze results for.
  :param graph_path: Path to the graph that generated results for this
  run.

  :return: A dictionary keyed by proposition, with values as dictionaries
  keyed by group name, and those values as dictionaries keyed by media name
  with values as a list of belief values of messages exposed. See the
  belief_by_group variable.
  '''
  (citizens, cit_social, media_arr, media_sub_arr) = read_graph(graph_path)
  graph = nlogo_saved_graph_to_nx(citizens, cit_social, media_arr, media_sub_arr)

  groups = set([ cit[2] for cit in citizens ])
  media_names = set([ media[1] for media in media_arr ])

  beliefs, _, messages_believed, messages_sent = process_message_data(data_path, rand_id)
  all_messages = combine_all_messages_no_media(messages_sent)
  message_id_to_name = {}
  for media_name, messages in messages_sent.items():
    for message in messages:
      message_id_to_name[message] = media_name

  belief_by_group = { proposition: { 
    group: { 
      media_name: [] for media_name in media_names
      } for group in groups
    } for proposition in beliefs['0'][0]
  }
  for tick in beliefs:
    beliefs_at_tick = beliefs[tick]
    for agent_id in range(len(beliefs_at_tick)):
      if tick not in messages_believed[agent_id]:
        continue
      agent_beliefs = beliefs_at_tick[agent_id]
      agent_group = graph.nodes[agent_id]['groups']
      messages_at_tick_for_agent = messages_believed[agent_id][tick]
      # messages = [ all_messages[str(message_id)] for message_id in messages_at_tick_for_agent ]
      for proposition,belief_value in agent_beliefs.items():
        for message_id in messages_at_tick_for_agent:
          message = all_messages[str(message_id)]
          message_value = message[proposition]
          message_sender = message_id_to_name[str(message_id)]
          belief_by_group[proposition][agent_group][message_sender].append(int(message_value))
  return belief_by_group

def process_multi_message_data(in_path):
  '''
  Aggregate all the message-related data analysis for a given experiment
  output path.

  :param in_path: The directory path to a given experiment directory
  containing messaging data files.
  '''
  proxy_filename = 'world.csv'
  file_ids = []
  message_multi_data = []
  if os.path.isdir(in_path):
    for file in os.listdir(in_path):
      if proxy_filename in file:
        file_ids.append(file.replace('_world.csv',''))

    for file_id in file_ids:
      data = process_message_data(in_path, file_id)
      message_multi_data.append(data)
    return message_multi_data
  else:
    print(f'ERROR: Path not found {in_path}')
    return -1

def plot_multi_message_data(multi_data_entry, out_path, show_plot=False):
  line_color = lambda key: '#000000'

  multi_data_has_multiple = lambda multi_data_entry: type(multi_data_entry[0]) == type(np.array(0)) and len(multi_data_entry) > 1

  measures = ['believed','heard']
  param_combo = multi_data_entry[0]
  multi_data = multi_data_entry[1]

  # The case where a path was not found
  if multi_data == -1:
    print(f'ERROR: No data for entry {param_combo}')
    return

  for measure in measures:
    # Check to make sure data can be stacked
    data_lengths = []
    for repetition in multi_data:
      data_lengths.append(len(repetition[measure]))
    if len(set(data_lengths)) > 1:
      print(f'ERROR: Data lengths unequal between repetitions for param combo {param_combo} measure {measure}: {data_lengths}')
      continue

    fig, (ax) = plt.subplots(1, figsize=(8,6))
    y_min = 0
    y_max = 6
    x_min = 0
    x_max = len(multi_data[0]['believed'])
    ax.set_ylim([y_min, y_max])
    plt.yticks(np.arange(y_min, y_max, step=1))
    plt.xticks(np.arange(x_min, x_max*1.1, step=5))
    ax.set_ylabel("Mean distance")
    ax.set_xlabel("Time step")

    mean_vec = np.array([ [ val[0] for val in repetition[measure] ] for repetition in multi_data ])
    mean_vec = mean_vec.mean(0) if multi_data_has_multiple(multi_data) else mean_vec[0]

    var_vec = np.array([ [ val[1] for val in repetition[measure] ] for repetition in multi_data ])
    var_vec = var_vec.var(0) if multi_data_has_multiple(multi_data) else var_vec[0]

    ax.plot(mean_vec, c=line_color(param_combo))
    ax.fill_between(range(x_min, len(mean_vec)), mean_vec-var_vec, mean_vec+var_vec, facecolor=f'{line_color(param_combo)}44')

    plt.savefig(f'{out_path}/{"-".join(param_combo)}_messages_{measure}_dist.png')
    if show_plot: plt.show()
    plt.close()

'''
Given some multi-chart data, plot it and save the plot.

:param multi_data: Data with means and std deviations for each point.
:param props: Properties object for the plotting.
:param out_path: A path to save the results in.
:param out_filename: A filename to save results as, defaults to 'aggregate-chart'
:param show_plot: Whether or not to display the plot before saving.
'''
def plot_multi_chart_data(types, multi_data, props, out_path, out_filename='aggregate-chart', show_plot=False):
  if PLOT_TYPES.LINE in types:
    plot = plot_nlogo_multi_chart_line(props, multi_data)
    plt.savefig(f'{out_path}/{out_filename}_line.png')
    if show_plot: plt.show()
    plt.close()

  if PLOT_TYPES.STACK in types:
    plot = plot_nlogo_multi_chart_stacked(props, multi_data)
    plt.savefig(f'{out_path}/{out_filename}_stacked.png')
    if show_plot: plt.show()
    plt.close()

  if PLOT_TYPES.HISTOGRAM in types:
    plot = plot_nlogo_multi_chart_histogram(props, multi_data)
    plt.savefig(f'{out_path}/{out_filename}_histogram.png')
    if show_plot: plt.show()
    plt.close()

def plot_chart_data(types, chart_data, props, out_path, out_filename='aggregate-chart', show_plot=False):
  if PLOT_TYPES.LINE in types:
    plot = plot_nlogo_chart_line(props, chart_data)
    plt.savefig(f'{out_path}/{out_filename}_line.png')
    if show_plot: plt.show()
    plt.close()

  if PLOT_TYPES.STACK in types:
    print('UNSUPPORTED CHART TYPE')
    # plot = plot_nlogo_chart_stacked(props, chart_data)
    # plt.savefig(f'{out_path}/{out_filename}_stacked.png')
    # if show_plot: plt.show()
    # plt.close()

  if PLOT_TYPES.HISTOGRAM in types:
    print('UNSUPPORTED CHART TYPE')
    # plot = plot_nlogo_chart_histogram(props, chart_data)
    # plt.savefig(f'{out_path}/{out_filename}_histogram.png')
    # if show_plot: plt.show()
    # plt.close()

'''
Plot multiple NetLogo chart data sets on a single plot. 

:param props: The properties dictionary read in from reading the chart file. This
describes pen colors, x and y min and max, etc.
:param multi_data: A dictionary (keyed by line) of matrices where each row is one simulation's worth of data points.
'''
def plot_nlogo_multi_chart_stacked(props, multi_data):
  init_dist_width = 10

  # series = pd.Series(data)
  fig, (ax) = plt.subplots(1, figsize=(8,6))
  # ax, ax2 = fig.add_subplot(2)
  ax.set_ylim([0, 1])
  y_min = int(round(float(props['y min'])))
  y_max = int(round(float(props['y max'])))
  x_min = int(round(float(props['x min'])))
  x_max = int(round(float(props['x max'])))
  plt.yticks(np.arange(y_min, y_max+0.2, step=0.2))
  plt.xticks(np.arange(x_min, x_max+10, step=10))
  ax.set_ylabel("Portion of agents who believe b")
  ax.set_xlabel("Time Step")


  multi_data_keys_int = list(map(lambda el: int(el), multi_data.keys()))

  # To use Netlogo colors
  # line_color = lambda key: f"#{rgb_to_hex(NLOGO_COLORS[int(round(float(props['color'][key])))])}"

  # To use higher resolution colors
  resolution = int(max(multi_data_keys_int))+1
  line_color = lambda key: f"#{rgb_to_hex([ 255 - round((255/(resolution-1))*int(key)), 0, round((255/(resolution-1)) * int(key)) ])}"
  
  mean_vecs = []
  var_vecs = []
  rev_keys_int = sorted(multi_data_keys_int, reverse=True)
  rev_keys = list(map(lambda el: f'{el}', rev_keys_int))
  multi_data_has_multiple = lambda multi_data_entry: type(multi_data_entry[0]) == type(np.array(0)) and len(multi_data_entry) > 1
  for key in rev_keys:
    mean_vec = multi_data[key].mean(0) if multi_data_has_multiple(multi_data[key])  else multi_data[key]
    var_vec = multi_data[key].var(0) if multi_data_has_multiple(multi_data[key]) else np.zeros(len(mean_vec))

    # Add padding for the initial values so those are displayed in the graph
    mean_vec = np.insert(mean_vec, 0, [ mean_vec[0] for i in range(init_dist_width) ])

    mean_vecs.append(mean_vec)
    var_vecs.append(var_vec)
  
  ax.set_xlim([x_min-init_dist_width,len(mean_vecs[0])-init_dist_width])
  plt.stackplot(range(x_min-init_dist_width, len(mean_vecs[0])-init_dist_width), mean_vecs, colors=[ f'{line_color(c)}' for c in rev_keys ], labels=[ f'b = {b}' for b in rev_keys ])

'''
Plot multiple NetLogo chart data sets on a single plot. This will scatterplot
each data set and then draw a line of the means at each point through the
entire figure.

:param props: The properties dictionary read in from reading the chart file. This
describes pen colors, x and y min and max, etc.
:param multi_data: A list of dataframes that contain chart data.
'''
def plot_nlogo_multi_chart_line(props, multi_data):
  # series = pd.Series(data)
  # print(multi_data)
  fig, (ax) = plt.subplots(1, figsize=(8,6))
  # ax, ax2 = fig.add_subplot(2)
  y_min = int(round(float(props['y min'])))
  y_max = int(round(float(props['y max'])))
  y_step = float(props['y step'])
  x_min = int(round(float(props['x min'])))
  x_max = int(round(float(props['x max'])))
  ax.set_ylim([0, y_max])
  plt.yticks(np.arange(y_min, y_max, step=y_step))
  # plt.yticks(np.arange(y_min, y_max*1.1, step=y_max/10))
  plt.xticks(np.arange(x_min, x_max*1.1, step=5))
  ax.set_ylabel("% of agents who believe b")
  ax.set_xlabel("Time Step")

  line_color = lambda key: '#000000'
  line_names_to_color = {
    'dem': '#0000ff',
    'mod': '#ff00ff',
    'rep': '#ff0000'
  }

  if 'dem' in list(multi_data.keys()):
    line_color = lambda key: line_names_to_color[key]
  elif list(multi_data.keys())[0] != 'default':
    # This is specific code to set the colors for belief resolutions
    multi_data_keys_int = list(map(lambda el: int(el), multi_data.keys()))
    resolution = int(max(multi_data_keys_int))+1
    line_color = lambda key: f"#{rgb_to_hex([ 255 - round((255/max(resolution-1,1))*int(key)), 0, round((255/max(resolution-1,1)) * int(key)) ])}"
 
  multi_data_has_multiple = lambda multi_data_entry: type(multi_data_entry[0]) == type(np.array(0)) and len(multi_data_entry) > 1
 
  for key in multi_data:
    mean_vec = multi_data[key].mean(0) if multi_data_has_multiple(multi_data[key]) else multi_data[key][0]
    var_vec = multi_data[key].var(0) if multi_data_has_multiple(multi_data[key]) else np.zeros(len(mean_vec))
    # print(multi_data[key])
    # print(var_vec)
    ax.plot(mean_vec, c=line_color(key))
    ax.fill_between(range(x_min, len(mean_vec)), mean_vec-var_vec, mean_vec+var_vec, facecolor=f'{line_color(key)}44')
  
  return multi_data

def plot_nlogo_chart_line(props, chart_data):
  print(props)
  fig, (ax) = plt.subplots(1, figsize=(8,6))
  # ax, ax2 = fig.add_subplot(2)
  y_min = int(round(float(props['y min'])))
  y_max = int(round(float(props['y max'])))
  y_step = float(props['y step'])
  x_min = int(round(float(props['x min'])))
  x_max = int(round(float(props['x max'])))
  ax.set_ylim([0, y_max])
  plt.yticks(np.arange(y_min, y_max, step=y_step))
  # plt.yticks(np.arange(y_min, y_max*1.1, step=y_max/10))
  plt.xticks(np.arange(x_min, x_max*1.1, step=5))
  ax.set_ylabel("% of agents who believe b")
  ax.set_xlabel("Time Step")

  line_color = lambda key: '#000000'
  line_names_to_color = {
    'dem': '#0000ff',
    'mod': '#ff00ff',
    'rep': '#ff0000'
  }

  if 'dem' in list(chart_data.keys()):
    line_color = lambda key: line_names_to_color[key]
  elif list(chart_data.keys())[0] != 'default':
    # This is specific code to set the colors for belief resolutions
    multi_data_keys_int = list(map(lambda el: int(el), chart_data.keys()))
    resolution = int(max(multi_data_keys_int))+1
    line_color = lambda key: f"#{rgb_to_hex([ 255 - round((255/max(resolution-1,1))*int(key)), 0, round((255/max(resolution-1,1)) * int(key)) ])}"
 
  for key in chart_data:
    data_vec = chart_data[key]
    # print(multi_data[key])
    # print(var_vec)
    ax.plot(data_vec['y'], c=line_color(key))
  
  return chart_data

'''
Plot multiple NetLogo chart data sets on a single plot. This will scatterplot
each data set and then draw a line of the means at each point through the
entire figure.

:param props: The properties dictionary read in from reading the chart file. This
describes pen colors, x and y min and max, etc.
:param multi_data: A list of dataframes that contain chart data.
'''
def plot_nlogo_histogram(props, multi_data):
  # series = pd.Series(data)
  # print(multi_data)
  fig, (ax) = plt.subplots(1, figsize=(8,6))
  # ax, ax2 = fig.add_subplot(2)
  ax.set_ylim([0, 1.1])
  y_min = int(round(float(props['y min'])))
  y_max = int(round(float(props['y max'])))
  x_min = int(round(float(props['x min'])))
  x_max = int(round(float(props['x max'])))
  plt.yticks(np.arange(y_min, y_max+0.2, step=0.2))
  plt.xticks(np.arange(x_min, x_max+10, step=10))
  ax.set_ylabel("# of agents who believe b")
  ax.set_xlabel("Time Step")

  line_color = lambda key: '#000000'

  if list(multi_data.keys())[0] != 'default':
    # This is specific code to set the colors for belief resolutions
    multi_data_keys_int = list(map(lambda el: int(el), multi_data.keys()))
    resolution = int(max(multi_data_keys_int))+1
    bar_color = lambda key: f"#{rgb_to_hex([ 255 - round((255/max(resolution-1,1))*int(key)), 0, round((255/max(resolution-1,1)) * int(key)) ])}"

  multi_data_has_multiple = lambda multi_data_entry: type(multi_data_entry[0]) == type(np.array(0)) and len(multi_data_entry) > 1
 
  for key in multi_data:
    mean_vec = multi_data[key].mean(0) if multi_data_has_multiple(multi_data[key])  else multi_data[key]
    var_vec = multi_data[key].var(0) if multi_data_has_multiple(multi_data[key]) else np.zeros(len(mean_vec))
    # print(var_vec)
    ax.plot(mean_vec, c=bar_color(key))
    ax.fill_between(range(x_min, len(mean_vec)), mean_vec-var_vec, mean_vec+var_vec, facecolor=f'{bar_color(key)}44')
  
  return multi_data
      
'''
From a NetLogo world export file, read in the simulation data for citizens
and media entities. They are stored in Pandas dataframes for further processing.

:param path: The path to the file to read data in from.
'''
def process_sim_data(path):
  f = open(path)
  raw = f.read()
  f.close()
  lines = raw.split('\n')
  turtle_data = []
  for i in range(0, len(lines)):
    line = lines[i]
    if line == '"TURTLES"':
      while line.strip() != '':
        i += 1
        line = lines[i]
        turtle_data.append(line.replace('""','"').split(','))

  turtle_data[0] = list(map(lambda el: el.replace('"',''), turtle_data[0]))
  turtle_df = pd.DataFrame(data=turtle_data[1:], columns=turtle_data[0])

  unneeded_cols = ['color', 'heading', 'xcor', 'ycor', 'label', 'label-color', 'shape', 'pen-size', 'pen-mode', 'size','hidden?']
  citizen_delete = ['media-attrs','messages-sent']
  media_delete = ['messages-heard','brain','messages-believed']

  for col in unneeded_cols:
    del turtle_df[col]

  citizen_df = turtle_df[turtle_df['breed'] == '"{breed citizens}"']
  media_df = turtle_df[turtle_df['breed'] == '"{breed medias}"']

  for col in citizen_delete:
    del citizen_df[col]
  for col in media_delete:
    del media_df[col]

  return (citizen_df, media_df)

'''
Get a relevant set of statistics about the citizens' ending state
after the simulation was run: which messages they heard

:param citizen_df: A dataframe containing citizen data.
'''
def citizen_message_statistics(citizen_df, media_df):
  messages = {}
  # Generate a data frame for media messages
  for m in media_df.iterrows():
    m_sent = nlogo_mixed_list_to_dict(m[1]['messages-sent'])
    messages.update(m_sent)
  for m_id, val in messages.items():
    val['id'] = int(m_id.strip())

  message_vals = list(messages.values())
  messages_df = pd.DataFrame(data=message_vals, columns=list(message_vals[0].keys()))

  # Generate citizen data frames relevant for statistics
  heard_dfs = {}
  for citizen in citizen_df.iterrows():
    parsed = nlogo_mixed_list_to_dict(citizen[1]['messages-heard'])
    flat_heard = []
    for timestep,message_ids in parsed.items():
      flat_heard.extend([ { 'tick': int(timestep), 'message_id': m_id  } for m_id in message_ids ] )
    df = pd.DataFrame(flat_heard)
    heard_dfs[int(citizen[1]['who'].replace('"',''))] = df
  
  believed_dfs = {}
  for citizen in citizen_df.iterrows():
    parsed = nlogo_mixed_list_to_dict(citizen[1]['messages-believed'])
    flat_believed = []
    if not type(parsed) == list:
      for timestep,message_ids in parsed.items():
        flat_believed.extend([ { 'tick': int(timestep), 'message_id': m_id  } for m_id in message_ids ] )
      df = pd.DataFrame(flat_believed)
      believed_dfs[int(citizen[1]['who'].replace('"',''))] = df
    else:
      believed_dfs[int(citizen[1]['who'].replace('"',''))] = pd.DataFrame()
  
  # Analyze the data frames for some statistical measures (per citizen)
  # - Total heard
  # - Total believed
  # - Ratio of believed/heard
  # - Totals heard broken down by partisanship & ideology
  # - Totals believed broken down by partisanship & ideology
  # - Totals heard broken down by virus belief
  # - Totals believed broken down by virus belief
  # - Somehow get at beliefs over time?

  per_cit_stats = {}
  for row in citizen_df.iterrows():
    citizen = row[1]
    cit_id = int(citizen['who'].replace('"',''))
    per_cit_stats[cit_id] = per_citizen_stats(cit_id, messages_df, heard_dfs[cit_id], believed_dfs[cit_id])

  # Analyze some group-level measures
  # - Aggregate by citizen's partisan/ideology pair
  # - 

  aggregate_stats = citizens_stats(citizen_df, per_cit_stats)

  return (messages_df, heard_dfs, believed_dfs, per_cit_stats, aggregate_stats)

'''
Generate some statistical measures based on the aggregate view of the citizenry.

:param cit_df: A dataframe containing data for each citizen.
:param per_cit_stats: A dictionary of statistical measures calculated
for each citizen. This is generated from the `citizen_stats()` function.
'''
def citizens_stats(cit_df, per_cit_stats):
  partisanships = list(attrs_as_array(Attributes.P))
  ideologies = list(attrs_as_array(Attributes.I))
  virus_believe_vals = [ -1, 0, 1 ]

  pi_keyed_dict = { (prod[0], prod[1]): 0 for prod in itertools.product(partisanships, ideologies) }
  virus_belief_keyed_dict = { (prod[0], prod[1], prod[2]): 0 for prod in itertools.product(virus_believe_vals, repeat=3) }

  total_by_p = { p: 0 for p in partisanships }
  total_by_i = { i: 0 for i in ideologies }
  total_by_p_i = pi_keyed_dict.copy()

  heard_by_cit_p_i = { (prod[0], prod[1]): [] for prod in itertools.product(partisanships, ideologies) }
  believed_by_cit_p_i = deepcopy(heard_by_cit_p_i)
  # This will be a dict of { (citizen_p, citizen_i): { (message_p, message_i):
  # [ list of (p,i) message heard ] } } - i.e. a dictionary of message types
  # heard by political group
  heard_by_pi_given_cit_pi = { (prod[0], prod[1]): deepcopy(heard_by_cit_p_i) for prod in itertools.product(partisanships, ideologies) }
  believed_by_pi_given_cit_pi = { (prod[0], prod[1]): deepcopy(heard_by_cit_p_i) for prod in itertools.product(partisanships, ideologies) }

  virus_bel_counts = virus_belief_keyed_dict.copy()
  virus_bel_totals = { (prod[0], prod[1], prod[2]): [] for prod in itertools.product(virus_believe_vals, repeat=3) }

  ending_beliefs_by_p_i = { (prod[0], prod[1]): virus_belief_keyed_dict.copy() for prod in itertools.product(partisanships, ideologies) }
  # Similarly to above, this will be a dict of { (cit_p, cit_i): {
  # (virus_beliefs...): [ list of per-citizen (vg,vs,vd) messages heard ] } }
  heard_by_virus_bel_given_pi = { (prod[0], prod[1]): deepcopy(virus_bel_totals) for prod in itertools.product(partisanships, ideologies) }
  believed_by_virus_bel_given_pi = { (prod[0], prod[1]): deepcopy(virus_bel_totals) for prod in itertools.product(partisanships, ideologies) }

  for cit in cit_df.iterrows():
    citizen = cit[1]
    cit_id = int(citizen['who'].replace('"',''))
    brain = nlogo_mixed_list_to_dict(citizen['brain'])
    stats = per_cit_stats[cit_id]

    pi_tup = pi_tuple(brain)
    virus_tup = virus_tuple(brain)

    total_by_i[int(brain['I'])] += 1
    total_by_p[int(brain['P'])] += 1
    total_by_p_i[pi_tup] += 1
    heard_by_cit_p_i[pi_tup].append(stats['total_heard'])
    believed_by_cit_p_i[pi_tup].append(stats['total_believed'])
    for message_pi_tup in stats['heard_by_p_i'].keys():
      heard_by_pi_given_cit_pi[pi_tup][message_pi_tup].append(stats['heard_by_p_i'][message_pi_tup])
      believed_by_pi_given_cit_pi[pi_tup][message_pi_tup].append(stats['believed_by_p_i'][message_pi_tup])

    virus_bel_counts[virus_tup] += 1
    ending_beliefs_by_p_i[pi_tup][virus_tup] += 1
    for message_virus_tup in stats['heard_by_virus_bel'].keys():
      heard_by_virus_bel_given_pi[pi_tup][message_virus_tup].append(stats['heard_by_virus_bel'][message_virus_tup])
      believed_by_virus_bel_given_pi[pi_tup][message_virus_tup].append(stats['believed_by_virus_bel'][message_virus_tup])
  
  heard_sum_by_p_i = { pi: summary_statistics(heard_by_cit_p_i[pi]) for pi in heard_by_cit_p_i.keys() }
  believed_sum_by_p_i = { pi: summary_statistics(believed_by_cit_p_i[pi]) for pi in believed_by_cit_p_i.keys() }

  heard_sum_by_pi_given_pi = pi_keyed_dict.copy()
  for pi in heard_by_pi_given_cit_pi.keys():
    entry = heard_by_pi_given_cit_pi[pi]
    heard_sum_by_pi_given_pi[pi] = { cit_pi: summary_statistics(entry[cit_pi]) for cit_pi in entry.keys() }

  believed_sum_by_pi_given_pi = pi_keyed_dict.copy()
  for pi in believed_by_pi_given_cit_pi.keys():
    entry = believed_by_pi_given_cit_pi[pi]
    believed_sum_by_pi_given_pi[pi] = { cit_pi: summary_statistics(entry[cit_pi]) for cit_pi in entry.keys() }

  heard_sum_by_virus_given_pi = pi_keyed_dict.copy()
  for pi in heard_by_virus_bel_given_pi.keys():
    entry = heard_by_virus_bel_given_pi[pi]
    heard_sum_by_virus_given_pi[pi] = { virus_bel: summary_statistics(entry[virus_bel]) for virus_bel in entry.keys() }

  believed_sum_by_virus_given_pi = pi_keyed_dict.copy()
  for pi in believed_by_virus_bel_given_pi.keys():
    entry = believed_by_virus_bel_given_pi[pi]
    believed_sum_by_virus_given_pi[pi] = { virus_bel: summary_statistics(entry[virus_bel]) for virus_bel in entry.keys() }
  
  stats_given_pi = pi_keyed_dict.copy()
  for pi in stats_given_pi.keys():
    stats_given_pi[pi] = {}
    stats_given_pi[pi]['n'] = total_by_p_i[pi]
    stats_given_pi[pi]['total_heard'] = heard_sum_by_p_i[pi]
    stats_given_pi[pi]['total_believed'] = believed_sum_by_p_i[pi]
    stats_given_pi[pi]['ending_beliefs'] = ending_beliefs_by_p_i[pi]
    stats_given_pi[pi]['heard_stats_by_pi'] = heard_sum_by_pi_given_pi[pi]
    stats_given_pi[pi]['believed_stats_by_pi'] = believed_sum_by_pi_given_pi[pi]
    stats_given_pi[pi]['heard_stats_by_virus'] = heard_sum_by_virus_given_pi[pi]
    stats_given_pi[pi]['believed_stats_by_virus'] = believed_sum_by_virus_given_pi[pi]

  return stats_given_pi

'''
Generate some statistics for each citizen in the simulation report data.
Measures that are reported:
- Total messages heard and believed
- Believed/heard ratio
- Messages heard & believed by (partisan,ideology) pair
- Messages heard & believed by virus-belief combination
'''
def per_citizen_stats(cit_id, messages_df, heard_df, believed_df):
  cit_stats = {}
  cit_stats['total_heard'] = len(heard_df)
  cit_stats['total_believed'] = len(believed_df)
  cit_stats['bel_heard_ratio'] = cit_stats['total_believed']/cit_stats['total_heard']

  partisanships = list(attrs_as_array(Attributes.P))
  ideologies = list(attrs_as_array(Attributes.I))
  heard_by_p_i = { (prod[0], prod[1]): 0 for prod in itertools.product(partisanships, ideologies) }

  for row in heard_df.iterrows():
    heard = row[1]
    m_id = int(heard['message_id'])
    message = messages_df[messages_df['id'] == m_id]
    heard_by_p_i[pi_tuple(message)] += 1

  # (P, I) tuples
  believed_by_p_i = { (prod[0], prod[1]): 0 for prod in itertools.product(partisanships, ideologies) }
  for row in believed_df.iterrows():
    believed = row[1]
    m_id = int(believed['message_id'])
    message = messages_df[messages_df['id'] == m_id]
    believed_by_p_i[pi_tuple(message)] += 1
  cit_stats['heard_by_p_i'] = heard_by_p_i
  cit_stats['believed_by_p_i'] = believed_by_p_i

  # (VG, VG, VD) tuples
  virus_believe_vals = [ -1, 0, 1 ]
  heard_by_virus_bel = { (prod[0], prod[1], prod[2]): 0 for prod in itertools.product(virus_believe_vals, repeat=3) }
  for row in heard_df.iterrows():
    heard = row[1]
    m_id = int(heard['message_id'])
    message = messages_df[messages_df['id'] == m_id]
    heard_by_virus_bel[virus_tuple(message)] += 1
  believed_by_virus_bel = { (prod[0], prod[1], prod[2]): 0 for prod in itertools.product(virus_believe_vals, repeat=3) }
  for row in believed_df.iterrows():
    believed = row[1]
    m_id = int(believed['message_id'])
    message = messages_df[messages_df['id'] == m_id]
    believed_by_virus_bel[virus_tuple(message)] += 1
  cit_stats['heard_by_virus_bel'] = heard_by_virus_bel
  cit_stats['believed_by_virus_bel'] = believed_by_virus_bel

  return cit_stats

def group_stats_by_attr(group_stats, attr):
  return { pi: val[attr] for pi,val in group_stats.items() }

'''
Return a tuple of partisanship and ideology attributes from a given object.

:param obj: Some object to fetch parameters from.
'''
def pi_tuple(obj): return (int(obj['P']),int(obj['I']))

'''
Return a tuple of virus-related beliefs from a given object.

:param obj: Some object to fetch parameters from.
'''
def virus_tuple(obj): return (int(obj['VG']),int(obj['VS']),int(obj['VD']))

def plot_stats_means(stats_data, title, path):
  plot_and_save_series({ key: val[0] for key,val in stats_data.items() }, title, path, 'bar')

def pi_data_charts(stats_data, attr, replace, title_w_replace, path_w_replace):
  partisanships = list(attrs_as_array(Attributes.P))
  ideologies = list(attrs_as_array(Attributes.I))
  pi_keys = { (prod[0], prod[1]): 0 for prod in itertools.product(partisanships, ideologies) }
  for key in pi_keys:
    plot_stats_means(stats_data[key][attr], title_w_replace.replace(replace, str(key)), path_w_replace.replace(replace, f'{key[0]}-{key[1]}'))

def corr_multi_data(multi_data_1, multi_data_2, method='pearson'):
  '''
  Calculate correlations between two sets of multi data.

  :param multi_data_1: A first set of data over multiple simulation runs, keyed by agent belief value.
  :param multi_data_2: A second set of data over multiple simulation runs, keyed by agent belief value.

  :return: Correlation values per belief value.
  '''
  m1_means = { key: multi_data_1[key].mean(0) for key in multi_data_1 }
  m2_means = { key: multi_data_2[key].mean(0) for key in multi_data_2 }

  rs = {}
  for key in multi_data_1:
    df = pd.DataFrame({ 'data1': m1_means[key], 'data2': m2_means[key] })
    # Uncomment if you need to investigate the df
    # rs[key] = {}
    # rs[key]['df'] = df
    # rs[key]['corr'] = df.corr(method=method).iloc[0,1]
    rs[key] = df.corr(method=method).iloc[0,1]
  return rs

def aggregate_corr(corr_by_bel):
  '''
  Generate an average correlation across correlations by belief value.

  :param corr_by_bel: A dictionary keyed by belief value of correlation values.
  '''
  non_nan = np.array(list(corr_by_bel.values()))
  non_nan = non_nan[np.logical_not(np.isnan(non_nan))]
  return non_nan.sum() / len(non_nan)

def chi_sq_test_multi_data(multi_data_1, multi_data_2, N):
  '''
  Perform a chi squared test on two sets of multi data for each timestep in the simulation data. 

  NOTE: This converts agent population percentages to total numbers and pads by 1
  in order to circumnavigate sampling 0 agents.

  :param multi_data_1: A first set of data over multiple simulation runs, keyed by agent belief value.
  :param multi_data_2: A second set of data over multiple simulation runs, keyed by agent belief value.
  :param N: The number of agents in the simulation.

  :returns: Returns the chi2 timeseries data.
  '''

  m1_means = [ multi_data_1[key].mean(0) for key in multi_data_1 ]
  m2_means = [ multi_data_2[key].mean(0) for key in multi_data_2 ]

  data = []
  for timestep in range(len(m1_means[0])):
    data.append([])
    # Append on lists of the values for each belief at timestep t 
    data[timestep].append([ m1_means[bel][timestep] for bel in range(len(m1_means)) ])
    data[timestep].append([ m2_means[bel][timestep] for bel in range(len(m2_means)) ])
  
  for data_t in data:
    for i in range(len(data_t[0])):
      data_t[0][i] = round(N * data_t[0][i] + 1)
      data_t[1][i] = round(N * data_t[1][i] + 1)
  
  chi2_data = [ chi2_contingency(data_t) for data_t in data ]
  # TODO: CHANGE THIS BACK
  # return chi2_data
  return (data, chi2_data)

def chi_sq_global(chi2_data):
  '''
  Convert a timeseries of chi squared test data into a global measure of how many
  entries in the time series are statistically independent. Higher values indicate
  higher levels of independence.

  :param chi2_data: An array of timeseries chi squared data from the scipy test.
  '''
  data = np.array([ el[1] for el in chi2_data ])
  return (data <= 0.05).sum() / len(data)


def plot_chi_sq_data(chi2_data, props, title, out_path, out_filename):
  '''
  Plot a time series calculation of chi squared measures per timestep.

  :param chi2_data: The timeseries data from running chi squared stats on belief data.
  :param props: The simulation properties to pull time data from.
  :param title: Text to title the plot with.
  '''
  # series = pd.Series(data)
  fig, (ax) = plt.subplots(1, figsize=(8,6))
  # ax, ax2 = fig.add_subplot(2)
  ax.set_ylim([0, 1.0])
  y_min = 0
  y_max = 1.0
  x_min = int(props['x min'])
  x_max = int(props['x max'])
  plt.yticks(np.arange(y_min, y_max+0.2, step=0.05))
  plt.xticks(np.arange(x_min, x_max+10, step=10))
  ax.set_ylabel("p value")
  ax.set_xlabel("Time Step")
  ax.set_title(f'{title}')
 
  ax.plot([ data[1] for data in chi2_data ])
  plt.savefig(f'{out_path}/{out_filename}')
  plt.close()

def colors_for_belief(resolution):
  rgb_to_hex = lambda rgb: '%02x%02x%02x' % tuple(rgb)
  line_color = lambda key: f"#{rgb_to_hex([ 255 - round((255/(resolution-1))*int(key)), 0, round((255/(resolution-1)) * int(key)) ])}"
  return { str(bel): line_color(bel) for bel in range(resolution) }

def graph_messages_interaction_by_belief(title, message_interaction, interaction_str, out_path='', out_name=''):
  '''
  Create a plot of several subplots -- one per belief value -- graphing bar charts
  of some number of message interactions per message belief value. By interactions,
  this just means either exposure, belief, sharing, etc.

  :param title: The graph title.
  :param message_interaction: A dictionary keyed on proposition, with values as
  dictionaries keyed on belief value with values as lists of message values. The
  output from message_exposure_by_belief_analysis is an example of this data
  structure.
  :param interaction_str: A string to use in the y-axis to label the interaction
  as "Number of messages ___" (e.g., heard, believed, shared, etc.)
  '''
  # This is just a placeholder because we only did experiments
  # with one proposition at the time of this function creation
  proposition = 'A'
  belief_resolution = 7
  fig, axs = plt.subplots(1, belief_resolution, figsize=(2*belief_resolution,3), sharey=True, constrained_layout=True)
  colors = colors_for_belief(belief_resolution)
  for belief_value,messages_per_run in message_interaction[proposition].items():
    ax = axs[int(belief_value)]
    x = list(range(belief_resolution))
    num_beliefs_per_belief_per_run = np.array([
      [ len([ message for message in messages if message == bel_value ]) for bel_value in range(belief_resolution) ] for messages in messages_per_run
    ])
    y_means = num_beliefs_per_belief_per_run.mean(0)
    y_std = num_beliefs_per_belief_per_run.std(0)

    hatches = [ 'xxx' if i == belief_value else '' for i in range(belief_resolution) ]
    ax.bar(x, y_means, yerr=y_std, ecolor='black', capsize=2, color=colors.values(), hatch=hatches, edgecolor='#ffffffaa')
    ax.set_xticks(list(range(belief_resolution)))
    ax.set_xticklabels(list(range(belief_resolution)))
    ax.set_xlabel(f'b={belief_value}', fontsize=12)
  plt.title(title)
  fig.supxlabel('Belief Values', fontsize=14)
  fig.supylabel(f'Number of messages {interaction_str}', fontsize=14)
  if out_path != '' and out_name != '':
    plt.savefig(f'{out_path}/{out_name}')
    plt.close()
  else:
    plt.show()

def graph_message_interaction_by_media_by_group(title, message_interaction, interaction_str, out_path='', out_name=''):
  proposition = 'A'
  messages_by_group = message_interaction[proposition]
  num_groups = len(messages_by_group)
  group_to_color = { 'DEM': 'blue', 'MOD': 'purple', 'REP': 'red' }
  media_to_color = {
    'CARLSON': 'red',
    'INGRAHAM': 'red',
    'HANNITY': 'red',
    'FOX': 'purple',
    'BREITBART': 'red',
    'NYT': 'purple',
    'VOX': 'blue',
    'KOS': 'blue'
  }
  media_order = [ 'KOS', 'VOX', 'NYT', 'FOX', 'BREITBART', "HANNITY", 'INGRAHAM', 'CARLSON' ]
  fig, axs = plt.subplots(1, num_groups, figsize=(3*num_groups,2), sharey='row', constrained_layout=True)
  i = 0
  group_order = ['DEM','MOD','REP']
  group_to_label = { 'DEM': 'Democrat', 'MOD': 'Moderate', 'REP': 'Republican' }
  media_to_label = {
    'CARLSON': 'Carlson',
    'INGRAHAM': 'Ingraham',
    'HANNITY': 'Hannity',
    'FOX': 'Fox',
    'BREITBART': 'Breitbart',
    'NYT': 'NYT',
    'VOX': 'Vox',
    'KOS': 'Daily Kos'
  }
  for group in group_order:
    messages_by_media = messages_by_group[group]
    ordered_messages_by_media = { media: messages_by_media[media] for media in media_order }
    # For the upper axis, group messages by sender
    ax = axs[i]
    x = list(range(len(ordered_messages_by_media)))
    y = [ np.array([ len(run_messages) for run_messages in all_run_messages_from_media ]).mean().round() for all_run_messages_from_media in ordered_messages_by_media.values() ]
    y_err = [ np.array([ len(run_messages) for run_messages in all_run_messages_from_media ]).std() for all_run_messages_from_media in ordered_messages_by_media.values() ]
    colors = [ media_to_color[media_name] for media_name in ordered_messages_by_media.keys() ]

    bars = ax.bar(x, y, yerr=y_err, ecolor='black', capsize=2, color=colors)
    ax.bar_label(bars)
    ax.set_xlabel(group_to_label[group], fontsize=12)
    ax.set_xticks(range(len(ordered_messages_by_media)))
    ax.set_xticklabels([ media_to_label[media_name] for media_name in ordered_messages_by_media.keys() ], rotation=45, fontsize=8)
    ax.spines[['top','right']].set_visible(False)
    i += 1
  fig.supylabel(f'Number of\nmessages {interaction_str}', fontsize=14)
  fig.supxlabel(f'Group and message producer', fontsize=14)
  if out_path != '' and out_name != '':
    plt.savefig(f'{out_path}/{out_name}')
    plt.close()
  else:
    plt.show()

def graph_message_interaction_by_belief_by_group(title, message_interaction, interaction_str, out_path='', out_name=''):
  proposition = 'A'
  belief_resolution = 7
  belief_colors = colors_for_belief(belief_resolution)
  messages_by_group = message_interaction[proposition]
  num_groups = len(messages_by_group)
  group_to_color = { 'DEM': 'blue', 'MOD': 'purple', 'REP': 'red' }
  group_to_label = { 'DEM': 'Democrat', 'MOD': 'Moderate', 'REP': 'Republican' }
  fig, axs = plt.subplots(1, num_groups, figsize=(3*num_groups,2), sharey='row', constrained_layout=True)
  i = 0
  group_order = ['DEM','MOD','REP']
  for group in group_order:
    messages_by_media = messages_by_group[group]
    ax = axs[i]
    x = list(range(belief_resolution))
    all_messages_by_run = [ np.array([]) for j in range(NUM_RUNS) ]
    for media_name, run_messages in messages_by_media.items():
      for run in range(len(run_messages)):
        all_messages_by_run[run] = np.append(all_messages_by_run[run], run_messages[run])
    belief_message_counts_by_run = np.array([ [ len([ message for message in run_messages if message == bel_value ]) for bel_value in range(belief_resolution) ] for run_messages in all_messages_by_run ])
    y = belief_message_counts_by_run.mean(0).round()
    y_err = belief_message_counts_by_run.std(0)
    bars = ax.bar(x, y, yerr=y_err, ecolor='black', capsize=2, color=belief_colors.values())
    ax.bar_label(bars)
    ax.set_xlabel(group_to_label[group], fontsize=12)
    ax.set_xticks(range(belief_resolution))
    ax.set_xticklabels(list(range(belief_resolution)))
    ax.spines[['top','right']].set_visible(False)
    i += 1
  fig.supylabel(f'Number of\nmessages {interaction_str}', fontsize=14)
  fig.supxlabel(f'Group and belief value', fontsize=14)
  if out_path != '' and out_name != '':
    plt.savefig(f'{out_path}/{out_name}')
    plt.close()
  else:
    plt.show()

def graph_parameter_distribution(df, cascade_type, graph_type, title):
  '''
  Graph a distribution of parameters for a given dataframe and possible
  parameters for a cascade and graph type. This plots a histogram with
  blank bars for param values that have 0 count.

  :param df: The simulation data with rows containing data for each parameter.
  This is used to get the counts of how many rows contained certain parameters.
  :param cascade_type: The CASCADE_TYPE for the simulation.
  :param graph_type: The GRAPH_TYPE for the simulation.
  :param title: Optional title for the graph.
  '''
  param_values = {}
  cascade_param_values = CASCADE_PARAM_VALUES[cascade_type].copy()
  graph_param_values = GRAPH_PARAM_VALUES[graph_type].copy()
  param_value_to_label = {
    'simple_spread_chance': '$p_{SC}$',
    'complex_spread_ratio': '$\eta$',
    'cognitive_exponent': '$\\alpha$',
    'cognitive_translate': '$\gamma$',
    'ba_m': '$m_{BA}$',
    'group_homophily': '$h_G$'
  }
  param_values.update(cascade_param_values)
  param_values.update(graph_param_values)
  fig, axs = plt.subplots(nrows=1, ncols=len(param_values), figsize=(3*len(param_values),1.5), constrained_layout=True, sharey=True)
  plt.rcParams['text.usetex'] = True
  for i in range(len(param_values)):
    param = list(param_values.keys())[i]
    possible_values = param_values[param]
    subax = axs[i]
    param_counts = { key: val for key, val in dict(df[param].value_counts()).items() }
    x = list(range(len(possible_values)))
    y = [ param_counts[value] if (value in param_counts) else 0 for value in possible_values ]
    subax.bar(x,y)
    subax.set_xticks(x)
    subax.set_xticklabels(possible_values)
    subax.set_xlabel(param_value_to_label[param], fontsize=14)
    subax.spines[['top','right']].set_visible(False)
  # fig.text(0.5, 0.04, 'Parameter', ha='center', va='center')
  # fig.text(0.06, 0.5, 'Number of runs with\nparameter as value', ha='center', va='center', rotation='vertical')
  axs[0].set_ylabel('Number of runs', fontsize=10)
  # fig.set_constrained_layout(True)
  # fig.supylabel('Parameter')
  plt.show()

def plot_opinion_timeseries_against_polling(all_run_data_df, polling_data, output_path='', output_filename=''):
  line_names = ['dem','mod','rep'] 
  line_name_to_label = {
    'dem': 'Democrat',
    'mod': 'Moderate',
    'rep': 'Republican'
  }
  line_colors = { 'dem': '#0000ff', 'mod': '#aa00aa', 'rep': '#ff0000' }
  run_data = {
    line_name: all_run_data_df[all_run_data_df['pen_name'] == line_name]['data'] for line_name in line_names
  }
  start_date = date(2020, 4, 6)
  end_date = date(2020, 6, 9)
  dates = list(daterange(start_date, end_date))
  x = np.arange(len(dates))

  fig, ax = plt.subplots(figsize=(8, 2), constrained_layout=True)
  for line_name in line_names:
    ax.plot(x, polling_data[line_name], color=line_colors[line_name], label=f'Empirical {line_name_to_label[line_name]}')
    mean_sim_data = run_data[line_name].mean(0) 
    std_sim_data = np.array(run_data[line_name]).std(0) 
    ax.plot(x, mean_sim_data, color=line_colors[line_name], linestyle='dashed', label=f'Simulated {line_name_to_label[line_name]}')
    ax.fill_between(x, mean_sim_data-std_sim_data, mean_sim_data+std_sim_data, facecolor=f'{line_colors[line_name]}22')

  ax.set_xticks(np.arange(len(dates), step=7))
  ax.set_xticklabels([f'{dates[i].month}-{dates[i].day}' for i in range(len(dates)) if i % 7 == 0], rotation=-45, ha='left', fontsize=10)
  ax.set_xlabel('Date')
  ax.set_ylabel('Percent supportive\nof mask wearing')
  # fig.legend(loc='upper center', ncol=6, mode='expand')
  # ax.legend(ncol=3, loc='lower center', fontsize=10)
  # plt.gca().xaxis.set_major_locator(mdates.DayLocator(interval=7))
  # plt.gcf().autofmt_xdate()

  if output_path != '':
    plt.savefig(f'{output_path}/{output_filename}')
    plt.close()
  else:
    plt.show()

def plot_initial_belief_dist_by_group(out_path='', out_file=''):
  proposition = ''
  belief_resolution = 7
  belief_colors = colors_for_belief(belief_resolution)
  distribution_data = json.load(open('./gallup-cit-init-dist.json', 'r'))
  distributions = { group: { belief: [] for belief in data['beliefs'].keys() } for group, data in distribution_data.items() }
  for group, specs in distribution_data.items():
    for belief_name, params in specs['beliefs'].items():
      proposition = belief_name
      for i in range(50):
        dist = params[0]
        if dist == 'normal':
          mean = params[1]
          std = params[2]
          distributions[group][belief_name].append(normal_dist(belief_resolution-1, mean, std, specs['n']))
        elif dist == 'matched_normal':
          mean = params[1]
          std = params[2]
          target_val = params[3]
          target_count = params[4]
          threshold = params[5]
          dist = matched_normal_dist(belief_resolution-1, mean, std, specs['n'], target_val, target_count, threshold)
          bel_vals_in_dist = [ (dist==i).sum() for i in range(belief_resolution) ]
          distributions[group][belief_name].append(bel_vals_in_dist)
  fig,axs = plt.subplots(nrows=1, ncols=3, constrained_layout=True, figsize=(9,2), sharey=True)
  group_order = ['DEM','MOD','REP']
  group_to_label = {
    'DEM': 'Democrat',
    'MOD': 'Moderate',
    'REP': 'Republican'
  }
  for i in range(len(group_order)):
    group = group_order[i]
    ax = axs[i]
    dists = distributions[group][proposition]
    y = np.array(dists).mean(0)
    y_err = np.array(dists).var(0)
    x = np.arange(belief_resolution)
    bars = ax.bar(x, y, yerr=y_err, ecolor='black', capsize=2, color=belief_colors.values())
    ax.set_xticks(x)
    ax.set_xticklabels(map(str, range(belief_resolution)))
    ax.set_xlabel(group_to_label[group])
  axs[0].set_ylabel('Number of agents with\ninitial belief (N=300)')
  if out_path != '' and out_file != '':
    plt.savefig(f'{out_path}/{out_file}')
    plt.close()
  else:
    plt.show()

def plot_messages_sent_by_media_by_group_aggregated(out_path='', out_file=''):
  media_connections = json.load(open('./gallup-media-connections.json', 'r'))
  media_names = list(media_connections.keys())
  messages_sent = json.load(open('./gallup-media-messages.json', 'r'))
  messages_sent_by_media = { media_name: [] for media_name in media_names }
  for tick, messages in messages_sent.items():
    if tick != 'start' and tick != 'stop':
      for media_name, media_messages in messages.items():
        messages_sent_by_media[media_name] += media_messages
  groups = ['DEM','MOD','REP']
  media_diets = { group: [ media_name for media_name,groups in media_connections.items() if group in groups ] for group in groups }
  messages_by_group = { group: [] for group in groups }
  proposition = 'A'
  for group, diet in media_diets.items():
    for media_name, messages in messages_sent_by_media.items():
      if media_name in diet:
        messages_by_group[group] += [ message[proposition] for message in messages ]
  belief_resolution = 7
  belief_colors = colors_for_belief(belief_resolution)
  fig,axs = plt.subplots(nrows=1, ncols=3, constrained_layout=True, figsize=(9,2), sharey=True)
  group_order = ['DEM','MOD','REP']
  group_to_label = {
    'DEM': 'Democrat',
    'MOD': 'Moderate',
    'REP': 'Republican'
  }
  for i in range(len(group_order)):
    group = group_order[i]
    ax = axs[i]
    y = [ (np.array(messages_by_group[group]) == i).sum() for i in range(belief_resolution) ]
    x = np.arange(belief_resolution)
    bars = ax.bar(x, y, color=belief_colors.values())
    ax.set_xticks(x)
    ax.set_xticklabels(map(str, range(belief_resolution)))
    ax.set_xlabel(group_to_label[group])
  axs[0].set_ylabel('Number of messages\nof each belief sent')
  if out_path != '' and out_file != '':
    plt.savefig(f'{out_path}/{out_file}')
    plt.close()
  else:
    plt.show()

def plot_group_belief_distributions_over_time(data_path, graph_path, out_path='', out_file=''):
  proposition = 'A'
  belief_resolution = 7
  belief_colors = colors_for_belief(belief_resolution)
  ticks_to_sample = ['0', '17', '35', '62']
  message_multidata = process_multi_message_data(data_path)
  (citizens, _, _, _) = read_graph(graph_path)
  groups = set([ cit[2] for cit in citizens ])

  citizens_per_group = { group: [ cit[0] for cit in citizens if cit[2] == group ] for group in groups }

  group_order = ['DEM','MOD','REP']

  beliefs_per_group_at_tick_per_run = { group: { tick: [] for tick in ticks_to_sample } for group in groups }
  for message_data in message_multidata:
    beliefs, _, _, _ = message_data
    for group in groups:
      for tick in ticks_to_sample:
        beliefs_at_tick = beliefs[tick]
        belief_values_in_group_at_tick = np.array(list(map(lambda el: el[proposition], [ beliefs_at_tick[i] for i in range(len(beliefs_at_tick)) if str(i) in citizens_per_group[group] ])))
        beliefs_per_group_at_tick_per_run[group][tick].append([ (belief_values_in_group_at_tick==i).sum() for i in range(belief_resolution) ])

  fig, axs = plt.subplots(nrows=len(groups), ncols=len(ticks_to_sample), sharey=True, constrained_layout=True, figsize=(9,3))
  for col in range(len(ticks_to_sample)):
    tick = ticks_to_sample[col]
    for row in range(len(groups)):
      group = group_order[row]
      ax = axs[(row,col)]
      data = beliefs_per_group_at_tick_per_run[group]
      x = np.arange(belief_resolution)
      y = np.array(data[tick]).mean(0)
      y_err = np.array(data[tick]).std(0)
      ax.bar(x, y, yerr=y_err, ecolor='black', capsize=2, color=belief_colors.values())
      ax.spines[['top','right']].set_visible(False)
      ax.set_xticks(x)
      ax.set_xticklabels(x)
      if col == 0:
        ax.set_ylabel(group)
    axs[(2, col)].set_xlabel(f't={tick}')
  
  if out_path != '' and out_file != '':
    plt.savefig(f'{out_path}/{out_file}')
    plt.close()
  else:
    plt.show()

"""
##################
EXPERIMENT-SPECIFIC
ANALYSIS
##################
"""

class PLOT_TYPES(Enum):
  LINE = 0
  STACK = 1
  HISTOGRAM = 2

def process_exp_outputs(param_combos, plots, path):
  '''
  Process the output of a NetLogo experiment, aggregating all results
  over simulation runs and generating plots for them according to
  all the parameter combinations denoted in param_combos.
  
  :param param_combos: A list of parameters where their values are
  lists (e.g. [ ['simple','complex'], ['default', 'gradual'] ])
  :param plots: A list of dictionaries keyed by the name of the NetLogo
  plot to process, with value of a list of PLOT_TYPE
  (e.g. { 'polarization': [PLOT_TYPES.LINE], 'agent-beliefs': [...] })
  :param path: The root path to begin processing in.
  '''
  combos = []
  for combo in itertools.product(*param_combos):
    combos.append(combo)

  if not os.path.isdir(f'{path}/results'):
    os.mkdir(f'{path}/results')

  for combo in combos:
    for (plot_name, plot_types) in plots.items():
      # print(plot_name, plot_types)
      (multi_data, props, model_params) = process_multi_chart_data(f'{path}/{"/".join(combo)}', plot_name)
      # If there was no error processing the data
      if multi_data != -1:
        plot_multi_chart_data(plot_types, multi_data, props, f'{path}/results', f'{"-".join(combo)}_{plot_name}-agg-chart')

def process_select_exp_outputs_single(param_combos, rand_params, plots, path, results_dir):
  '''
  Process some of the output of a NetLogo experiment, aggregating specified 
  results over simulation runs and generating plots for them according to
  all the parameter combinations denoted in param_combos.
  
  :param param_combos: A list of selected parameter values as lists
  :param plots: A list of dictionaries keyed by the name of the NetLogo
  plot to process, with value of a list of PLOT_TYPE
  (e.g. { 'polarization': [PLOT_TYPES.LINE], 'agent-beliefs': [...] })
  :param path: The root path to begin processing in.
  '''
  if not os.path.isdir(f'{path}/{results_dir}'):
    os.mkdir(f'{path}/{results_dir}')

  for i in range(len(param_combos)):
    combo = param_combos[i]
    rand_id = rand_params[i]
    for (plot_name, plot_types) in plots.items():
      # print(plot_name, plot_types)
      chart_data = process_chart_data(f'{path}/{"/".join(combo)}/{rand_id}_{plot_name}.csv')
      model_params = chart_data[0]
      props = chart_data[1]
      if plot_name == 'opinion-timeseries':
        props['y min'] = 0
        props['y max'] = 100
        props['y step'] = 5
      if plot_name == 'percent-agent-beliefs':
        props['y min'] = 0
        props['y max'] = 1
        props['y step'] = 0.1
      data = chart_data[2]
      plot_chart_data(plot_types, data, props, f'{path}/{results_dir}', f'{"-".join(combo)}_{plot_name}')

def process_select_exp_outputs_mean(param_combos, plots, path, results_dir):
  '''
  Process some of the output of a NetLogo experiment, aggregating specified 
  results over simulation runs and generating plots for them according to
  all the parameter combinations denoted in param_combos.
  
  :param param_combos: A list of selected parameter values as lists
  :param plots: A list of dictionaries keyed by the name of the NetLogo
  plot to process, with value of a list of PLOT_TYPE
  (e.g. { 'polarization': [PLOT_TYPES.LINE], 'agent-beliefs': [...] })
  :param path: The root path to begin processing in.
  '''
  if not os.path.isdir(f'{path}/{results_dir}'):
    os.mkdir(f'{path}/{results_dir}')

  for combo in param_combos:
    for (plot_name, plot_types) in plots.items():
      # print(plot_name, plot_types)
      (multi_data, props, _, _) = process_multi_chart_data(f'{path}/{"/".join(combo)}', plot_name)
      print(plot_name)
      if plot_name == 'opinion-timeseries':
        props['y min'] = 0
        props['y max'] = 100
        props['y step'] = 5
      if plot_name == 'percent-agent-beliefs':
        props['y min'] = 0
        props['y max'] = 1
        props['y step'] = 0.1
      # If there was no error processing the data
      if multi_data != -1:
        plot_multi_chart_data(plot_types, multi_data, props, f'{path}/{results_dir}', f'{"-".join(combo)}_{plot_name}-agg-chart')

def get_all_message_multidata(param_combos, path):
  combos = []
  for combo in itertools.product(*param_combos):
    combos.append(combo)

  multi_datas = {}
  for combo in combos:
    multi_message_data = process_multi_message_data(f'{path}/{"/".join(combo)}')
    multi_datas[combo] = multi_message_data
  return multi_datas

def process_all_message_multidata(param_combos, path):
  combos = []
  for combo in itertools.product(*param_combos):
    combos.append(combo)

  multi_datas = {}
  for combo in combos:
    multi_message_data = process_multi_message_data(f'{path}/{"/".join(combo)}')
    plot_multi_message_data((combo, multi_message_data), f'{path}/results', False)
    multi_datas[combo] = multi_message_data
  return multi_datas

def get_all_multidata(param_combos, params, plots, path):
  combos = []
  for combo in itertools.product(*param_combos):
    combos.append(combo)

  multi_datas = {}
  for combo in combos:
    for (plot_name, plot_types) in plots.items():
      # print(plot_name, plot_types)
      (multi_data, props, model_params, global_vars) = process_multi_chart_data(f'{path}/{"/".join(combo)}', plot_name)
      multi_datas[(combo,plot_name)] = { 'data': multi_data, 'globals': global_vars }
      multi_datas['params'] = params
  return multi_datas

def missing_values_in_multidata(multidata):
  return { key: val for key,val in multidata.items() if val == -1 }

def all_dataframe_to_mean_dataframe(all_df):
  '''
  Convert a dataframe containing data for each run of a simulation
  parameter combination to a dataframe taking the mean across runs
  (but not across graphs, or the 'repetition' column).

  :param all_df: The dataframe containing all simulation result rows.
  '''
  param_columns = list_subtract(list(all_df.columns), ['measure','run','rand_id','data'])
  mean_df = pd.DataFrame(columns=param_columns + ['measure','data'])
  param_values = {
    param: all_df[param].unique() for param in param_columns
  }
  param_combos = itertools.product(*param_values.values())
  for combo in param_combos:
    param_to_value = { param_columns[i]: combo[i] for i in range(len(combo)) }
    query = ' and '.join([ f'{param} == "{value}"' if type(value) == str else f'{param} == {value}' for param, value in param_to_value.items() ])
    res = all_df.query(query)
    if len(res) > 0:
      all_runs_data = res['data']
      stacked_data = np.vstack(all_runs_data)
      mean_df.loc[len(mean_df)] = list(param_to_value.values()) + [res.iloc[0]['measure'], stacked_data.mean(0)]
    else:
      print(f'Missing rows for param combo: {combo}')
  return mean_df

def multidata_as_dataframe(multidata, columns):
  '''
  
  :param multidata: A dictionary keyed on a tuple of (param_combo, measure), where the values are a dictionary with two values: 'data' which contains
  a dictionary keyed on pen_name with values of stacked arrays of plot
  data for each run of a certain param combo
  '''
  df = pd.DataFrame(columns=(columns+['measure','pen_name','run','rand_id','data']))
  for (param_measure,data_globals) in multidata.items():
    if param_measure == 'params':
      continue
    data = data_globals['data']
    global_vars = data_globals['globals']
    # Something failed to read/didn't exist
    if data == -1:
      continue
    param_combo = param_measure[0]
    measure = param_measure[1]
    for (pen_name,runs_data) in data.items():
      for run in range(len(runs_data)):
        df.loc[len(df.index)] = list(param_combo) + [measure,pen_name,run,global_vars[run]['behavior-rand'],runs_data[run]]
  return df

def mean_multidata_as_dataframe(multidata, columns):
  '''
  
  :param multidata: A dictionary keyed on a tuple of (param_combo, measure), where the values are a dictionary with two values: 'data' which contains
  a dictionary keyed on pen_name with values of stacked arrays of plot
  data for each run of a certain param combo
  '''
  df = pd.DataFrame(columns=(columns+['measure','pen_name','data']))
  for (param_measure,data_globals) in multidata.items():
    if param_measure == 'params':
      continue
    data = data_globals['data']
    # Something failed to read/didn't exist
    if data == -1:
      continue
    param_combo = param_measure[0]
    measure = param_measure[1]
    for (pen_name,runs_data) in data.items():
      df.loc[len(df.index)] = list(param_combo) + [measure,pen_name,runs_data.mean(0)]
  return df

def dataframe_as_multidata(df):
  multidata = {}
  added_columns = ['measure','pen_name','run','data']
  for row in df.itertuples():
    param_columns = tuple([ col for col in df.columns if col not in added_columns ])
    param_col_values = tuple([ getattr(row,col) for col in param_columns ])
    measure = row.measure
    pen_name = row.pen_name
    key = (param_col_values,measure)
    if key not in multidata:
      multidata[key] = { }
    
    if pen_name not in multidata[key]:
      multidata[key][pen_name] = np.array([row.data])
    else:
      data = multidata[key][pen_name]
      multidata[key][pen_name] = np.vstack([data,row.data])
          # means[key] = np.vstack([means[key], data_vector])
  return multidata

def write_dataframe_with_simulation_data(df, path):
  write_safe_df = df.copy()
  for i in range(len(write_safe_df)):
    data = write_safe_df.iloc[i]['data']
    write_safe_df.at[i,'data'] = str(data.tolist()).replace(',',';')
  write_safe_df.to_csv(path)

def read_dataframe_with_simulation_data(path):
  df = pd.read_csv(path)
  df.drop(columns=['Unnamed: 0'], inplace=True)
  for i in range(len(df)):
    raw_data = df.iloc[i]['data']
    df.at[i,'data'] = np.array(ast.literal_eval(raw_data.replace(';',',')))
  return df

def process_top_exp_all_results(top_df, param_order, path, results_dir):
  params_no_rand_id = list_subtract(param_order, ['rand_id'])
  top_param_combos = [
    [ str(top_df[col].iloc[i]) for col in params_no_rand_id ]
    for i in range(len(top_df))
  ]
  rand_params = list(top_df['rand_id'])
  process_select_exp_outputs_single(
    top_param_combos,
    rand_params,
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path,
    results_dir)

def process_top_exp_mean_results(top_df, param_order, path, results_dir):
  top_param_combos = [
    [ str(top_df[col].iloc[i]) for col in param_order ]
    for i in range(len(top_df))
  ]
  process_select_exp_outputs_mean(
    top_param_combos,
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path,
    results_dir)

def process_simple_contagion_param_sweep_ER_top_mean(top_df, path, results_dir):
  param_order = ['er_p','simple_spread_chance','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_ER_top_mean(top_df, path, results_dir):
  param_order = ['er_p','cognitive_translate','cognitive_exponent','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_ER_top_mean(top_df, path, results_dir):
  param_order = ['er_p','complex_spread_ratio','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_simple_contagion_param_sweep_ER_top_all(top_df, path, results_dir):
  param_order = ['er_p','simple_spread_chance','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_ER_top_all(top_df, path, results_dir):
  param_order = ['er_p','cognitive_translate','cognitive_exponent','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_ER_top_all(top_df, path, results_dir):
  param_order = ['er_p','complex_spread_ratio','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_simple_contagion_param_sweep_ER_test(path):
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  er_p = ['0.05','0.1','0.25','0.5']
  repetition = list(map(str, range(4)))
  process_exp_outputs(
    [er_p,simple_spread_chance,repetition],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)

def get_simple_contagion_param_sweep_ER_test_multidata(path):
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  er_p = ['0.05','0.1','0.25','0.5']
  repetition = list(map(str, range(4)))
  measure_multidata = get_all_multidata(
    [er_p,simple_spread_chance,repetition],
    ['er_p','simple_spread_chance','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_simple_contagion_param_sweep_ER_multidata(path):
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  er_p = ['0.05','0.1','0.25','0.5']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [er_p,simple_spread_chance,repetition],
    ['er_p','simple_spread_chance','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_cognitive_contagion_param_sweep_ER_multidata(path):
  cognitive_exponent = ['1','2','3','4','5']
  cognitive_translate = ['0','1','2','3']
  er_p = ['0.05','0.1','0.25','0.5']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [er_p,cognitive_translate,cognitive_exponent,repetition],
    ['er_p','cognitive_translate','cognitive_exponent','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_complex_contagion_param_sweep_ER_multidata(path):
  complex_spread_ratio = ['0.05','0.1','0.25','0.5','0.75','0.95']
  er_p = ['0.05','0.1','0.25','0.5']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [er_p,complex_spread_ratio,repetition],
    ['er_p','complex_spread_ratio','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def process_simple_contagion_param_sweep_WS_top_mean(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','simple_spread_chance','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_simple_contagion_param_sweep_WS_top_all(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','simple_spread_chance','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_WS_top_mean(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','cognitive_translate','cognitive_exponent','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_WS_top_all(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','cognitive_translate','cognitive_exponent','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_WS_top_mean(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','complex_spread_ratio','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_WS_top_all(top_df, path, results_dir):
  param_order = ['ws_p','ws_k','complex_spread_ratio','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def get_simple_contagion_param_sweep_WS_multidata(path):
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  ws_p = ['0.1','0.25','0.5']
  ws_k = ['2','3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ws_p,ws_k,simple_spread_chance,repetition],
    ['ws_p','ws_k','simple_spread_chance','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_cognitive_contagion_param_sweep_WS_multidata(path):
  cognitive_exponent = ['1','2','3','4','5']
  cognitive_translate = ['0','1','2','3']
  ws_p = ['0.1','0.25','0.5']
  ws_k = ['2','3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ws_p,ws_k,cognitive_translate,cognitive_exponent,repetition],
    ['ws_p','ws_k','cognitive_translate','cognitive_exponent','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_complex_contagion_param_sweep_WS_multidata(path):
  complex_spread_ratio = ['0.05','0.1','0.25','0.5','0.75','0.9']
  ws_p = ['0.1','0.25','0.5']
  ws_k = ['2','3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ws_p,ws_k,complex_spread_ratio,repetition],
    ['ws_p','ws_k','complex_spread_ratio','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def process_simple_contagion_param_sweep_BA_top_all(top_df, path, results_dir):
  param_order = ['ba_m','simple_spread_chance','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_BA_top_all(top_df, path, results_dir):
  param_order = ['ba_m','cognitive_translate','cognitive_exponent','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_BA_top_all(top_df, path, results_dir):
  param_order = ['ba_m','complex_spread_ratio','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_simple_contagion_param_sweep_BA_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','simple_spread_chance','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_BA_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','cognitive_translate','cognitive_exponent','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_BA_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','complex_spread_ratio','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def get_param_sweep_multidata(cascade_type, graph_type, path):
  cascade_param_values = { param: str(value) for param, value in CASCADE_PARAM_VALUES[cascade_type].items() }
  graph_param_values = { param: str(value) for param, value in GRAPH_PARAM_VALUES[graph_type].items() }
  repetition = list(map(str, range(NUM_REPETITIONS)))
  combined_values = {}
  combined_values.update(cascade_param_values)
  combined_values.update(graph_param_values)
  combined_values.update({'repetition': repetition})
  param_order =SIM_PARAM_ORDERS[(cascade_type, graph_type)] 
  measure_multidata = get_all_multidata(
    [ combined_values[param] for param in param_order ],
    param_order,
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_simple_contagion_param_sweep_BA_multidata(path):
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,simple_spread_chance,repetition],
    ['ba_m','simple_spread_chance','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_cognitive_contagion_param_sweep_BA_multidata(path):
  cognitive_exponent = ['1','2','3','4','5']
  cognitive_translate = ['0','1','2','3']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,cognitive_translate,cognitive_exponent,repetition],
    ['ba_m','cognitive_translate','cognitive_exponent','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_complex_contagion_param_sweep_BA_multidata(path):
  complex_spread_ratio = ['0.05','0.1','0.25','0.5','0.75','0.9']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,complex_spread_ratio,repetition],
    ['ba_m','complex_spread_ratio','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def process_simple_contagion_param_sweep_BA_group_homophily_top_all(top_df, path, results_dir):
  param_order = ['ba_m','simple_spread_chance','group_homophily','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_BA_group_homophily_top_all(top_df, path, results_dir):
  param_order = ['ba_m','cognitive_translate','cognitive_exponent','group_homophily','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_BA_group_homophily_top_all(top_df, path, results_dir):
  param_order = ['ba_m','complex_spread_ratio','group_homophily','repetition','rand_id']
  process_top_exp_all_results(top_df, param_order, path, results_dir)

def process_simple_contagion_param_sweep_BA_group_homophily_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','simple_spread_chance','group_homophily','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_cognitive_contagion_param_sweep_BA_group_homophily_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','cognitive_translate','cognitive_exponent','group_homophily','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def process_complex_contagion_param_sweep_BA_group_homophily_top_mean(top_df, path, results_dir):
  param_order = ['ba_m','complex_spread_ratio','group_homophily','repetition']
  process_top_exp_mean_results(top_df, param_order, path, results_dir)

def get_simple_contagion_param_sweep_BA_group_homophily_multidata(path):
  group_homophily = ['0.1','0.25','0.5','0.75']
  simple_spread_chance = ['0.01','0.05','0.1','0.25','0.5','0.75']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,simple_spread_chance,group_homophily,repetition],
    ['ba_m','simple_spread_chance','group_homophily','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_cognitive_contagion_param_sweep_BA_group_homophily_multidata(path):
  group_homophily = ['0.1','0.25','0.5','0.75']
  cognitive_exponent = ['1','2','3','4','5']
  cognitive_translate = ['0','1','2','3']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,cognitive_translate,cognitive_exponent,group_homophily,repetition],
    ['ba_m','cognitive_translate','cognitive_exponent','group_homophily','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def get_complex_contagion_param_sweep_BA_group_homophily_multidata(path):
  group_homophily = ['0.1','0.25','0.5','0.75']
  complex_spread_ratio = ['0.05','0.1','0.25','0.5','0.75','0.9']
  ba_m = ['3','5','10','15']
  repetition = list(map(str, range(10)))
  measure_multidata = get_all_multidata(
    [ba_m,complex_spread_ratio,group_homophily,repetition],
    ['ba_m','complex_spread_ratio','group_homophily','repetition'],
    {'percent-agent-beliefs': [PLOT_TYPES.LINE, PLOT_TYPES.STACK],
    'opinion-timeseries': [PLOT_TYPES.LINE]},
    path)
  return measure_multidata

def read_gallup_data_into_dict(path):
  gallup_data = pd.read_csv(path)
  gallup_data.drop(columns=['Unnamed: 0'], inplace=True)
  gallup_dict = { col: np.array(gallup_data[col]) for col in gallup_data.columns }
  return gallup_dict

def metrics_for_simple_contagion_param_sweep_ER_test(path):
  gallup_dict = read_gallup_data_into_dict('../labeled-data/public/gallup-polling.csv')
  columns = ['er_p','simple_spread_chance','repetition']
  measure = 'opinion-timeseries'
  multidata = get_simple_contagion_param_sweep_ER_test_multidata(path)
  all_run_metrics = timeseries_similarity_for_all_runs(multidata, measure, columns, gallup_dict)
  mean_metrics = timeseries_similarity_for_mean_runs(multidata, measure, columns, gallup_dict)
  return all_run_metrics, mean_metrics

class GRAPH_TYPES(Enum):
  ER = 'er'
  WS = 'ws'
  BA = 'ba'
  BA_GROUP_H = 'ba-group-homophily'

class CASCADE_TYPES(Enum):
  SIMPLE = 'simple'
  COMPLEX = 'complex'
  COGNITIVE = 'cognitive'

CASCADE_PARAMETERS = {
  CASCADE_TYPES.SIMPLE: ['simple_spread_chance'],
  CASCADE_TYPES.COMPLEX: ['complex_spread_ratio'],
  CASCADE_TYPES.COGNITIVE: ['cognitive_translate','cognitive_exponent'],
}
GRAPH_PARAMETERS = {
  GRAPH_TYPES.ER: ['er_p'],
  GRAPH_TYPES.WS: ['ws_p', 'ws_k'],
  GRAPH_TYPES.BA: ['ba_m'],
  GRAPH_TYPES.BA_GROUP_H: ['ba_m','group_homophily']
}
CASCADE_PARAM_VALUES = {
  CASCADE_TYPES.SIMPLE: {
    'simple_spread_chance': [0.01,0.05,0.1,0.25,0.5,0.75]
  },
  CASCADE_TYPES.COMPLEX: {
    'complex_spread_ratio': [0.1,0.25,0.5,0.75]
  },
  CASCADE_TYPES.COGNITIVE: {
    'cognitive_exponent': [1,2,3,4,5],
    'cognitive_translate': [0,1,2]
  }
}
GRAPH_PARAM_VALUES = {
  GRAPH_TYPES.ER: {
    'er_p': [0.05,0.1,0.25,0.5]
  },
  GRAPH_TYPES.WS: {
    'ws_p': [0.1,0.25,0.5],
    'ws_k': [2,3,5,10,15]
  },
  GRAPH_TYPES.BA: {
    'ba_m': [5,10,15] 
  },
  GRAPH_TYPES.BA_GROUP_H: {
    'ba_m': [5,10,15],
    'group_homophily': [0.1,0.25,0.5,0.75]
  }
}
PARAM_TYPES = {
  'simple_spread_chance': float,
  'complex_spread_ratio': float,
  'cognitive_exponent': int,
  'cognitive_translate': int,
  'er_p': float,
  'ws_p': float,
  'ws_k': int,
  'ba_m': int,
  'group_homophily': float,
  'repetition': int,
  'run': int
}

GRAPH_TYPE_DIRECTORY_NAMES = {
  GRAPH_TYPES.ER: 'ER',
  GRAPH_TYPES.WS: 'WS',
  GRAPH_TYPES.BA: 'BA',
  GRAPH_TYPES.BA_GROUP_H: 'BA-group-homophily'
}
SIM_PARAM_ORDERS = {
  (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): ['ba_m','simple_spread_chance','repetition'],
  (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): [ 'ba_m','complex_spread_ratio', 'repetition'],
  (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): [ 'ba_m', 'cognitive_translate','cognitive_exponent', 'repetition'],
  (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): ['ba_m','simple_spread_chance', 'group_homophily', 'repetition'],
  (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): ['ba_m', 'complex_spread_ratio','group_homophily','repetition'],
  (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): ['ba_m','cognitive_translate','cognitive_exponent','group_homophily','repetition']
}
    
NUM_REPETITIONS = 10
NUM_RUNS = 30

def opinion_metrics_for_param_sweep_exp(path, cascade_type, graph_topology):
  gallup_dict = read_gallup_data_into_dict('../labeled-data/public/gallup-polling.csv')
  cascade_type_str = cascade_type.value
  graph_topology_str = graph_topology.value
  multidata_functions = {
    (CASCADE_TYPES.SIMPLE,GRAPH_TYPES.ER): get_simple_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.COMPLEX,GRAPH_TYPES.ER): get_complex_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.COGNITIVE,GRAPH_TYPES.ER): get_cognitive_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.SIMPLE,GRAPH_TYPES.WS): get_simple_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.COMPLEX,GRAPH_TYPES.WS): get_complex_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.COGNITIVE,GRAPH_TYPES.WS): get_cognitive_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.SIMPLE,GRAPH_TYPES.BA): get_simple_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.COMPLEX,GRAPH_TYPES.BA): get_complex_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.COGNITIVE,GRAPH_TYPES.BA): get_cognitive_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.SIMPLE,GRAPH_TYPES.BA_GROUP_H): get_simple_contagion_param_sweep_BA_group_homophily_multidata,
    (CASCADE_TYPES.COMPLEX,GRAPH_TYPES.BA_GROUP_H): get_complex_contagion_param_sweep_BA_group_homophily_multidata,
    (CASCADE_TYPES.COGNITIVE,GRAPH_TYPES.BA_GROUP_H): get_cognitive_contagion_param_sweep_BA_group_homophily_multidata,
  }
  columns = CASCADE_PARAMETERS[cascade_type] + GRAPH_PARAMETERS[graph_topology] + ['repetition']
  measure = 'opinion-timeseries'

  data_path = SIM_DF_DATA_DIR
  all_data_file = f'{data_path}/{cascade_type_str}-{graph_topology_str}-all.csv'
  mean_data_file = f'{data_path}/{cascade_type_str}-{graph_topology_str}-mean.csv'
  all_run_metrics = None
  mean_metrics = None
  if exists(all_data_file) and exists(mean_data_file):
    print(f'Reading in existing data for {cascade_type_str},{graph_topology_str}')
    all_df = read_dataframe_with_simulation_data(all_data_file)
    mean_df = read_dataframe_with_simulation_data(mean_data_file)
    all_run_metrics = timeseries_similarity_scores_for_simulations(all_df, gallup_dict)
    mean_metrics = timeseries_similarity_scores_for_simulations(mean_df, gallup_dict)
  else:
    multidata = multidata_functions[(cascade_type,graph_topology)](path)
    all_run_metrics = timeseries_similarity_for_all_runs(multidata, measure, columns, gallup_dict)
    mean_metrics = timeseries_similarity_for_mean_runs(multidata, measure, columns, gallup_dict)
  return all_run_metrics, mean_metrics

def metrics_for_simple_contagion_param_sweep_ER(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER)

def metrics_for_simple_contagion_param_sweep_WS(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS)

def metrics_for_simple_contagion_param_sweep_BA(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA)

def metrics_for_simple_contagion_param_sweep_BA_group_homophily(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H)

def metrics_for_cognitive_contagion_param_sweep_ER(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER)

def metrics_for_cognitive_contagion_param_sweep_WS(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS)

def metrics_for_cognitive_contagion_param_sweep_BA(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA)

def metrics_for_cognitive_contagion_param_sweep_BA_group_homophily(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H)

def metrics_for_complex_contagion_param_sweep_ER(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER)

def metrics_for_complex_contagion_param_sweep_WS(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS)

def metrics_for_complex_contagion_param_sweep_BA(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA)

def metrics_for_complex_contagion_param_sweep_BA_group_homophily(path):
  return opinion_metrics_for_param_sweep_exp(path, CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H)

def top_matches_for_metrics(metrics_df):
  metrics_df['mape*pearson'] = (1 - metrics_df['mape']) * metrics_df['pearson']
  ranked = metrics_df.sort_values(by=['mape'], ascending=True)
  # ranked = metrics_df.sort_values(by=['pearson','mape'], ascending=[False,True])
  return ranked.head(10)

def mean_multidata(multidata):
  multi_data_has_multiple = lambda multi_data_entry: type(multi_data_entry[0]) == type(np.array(0)) and len(multi_data_entry) > 1
  # for param_measure in multidata.keys():
  #   if param_measure != 'params':
  #     if multidata[param_measure] == -1:
  #       print(f'Found -1 at {param_measure}')
  #     else:
  #       print(f'Keys: {multidata[param_measure].keys()}')
  mean_multidata = {
    # param_measure[0] is the parameter combo tuple
    param_measure: {
      'data': {
        pen_name: (multidata[param_measure]['data'][pen_name].mean(0) if multi_data_has_multiple(multidata[param_measure]['data'][pen_name]) else multidata[param_measure]['data'][pen_name]) for pen_name in multidata[param_measure]['data'].keys() 
      },
      'globals': multidata[param_measure]['globals']
    } for param_measure in multidata.keys() if (param_measure != 'params' and multidata[param_measure]['data'] != -1)
  }
  return mean_multidata

def multidata_means_as_df(multidata, columns):
  mean_data = mean_multidata(multidata)
  df_mean = mean_multidata_as_dataframe(mean_data, columns)
  return df_mean

def timeseries_similarity_for_all_runs(multidata, measure, columns, target_data):
  '''
  
  :param multidata: A dictionary keyed on a tuple of (param_combo, measure), where the values are a dictionary with two values: 'data' which contains
  a dictionary keyed on pen_name with values of stacked arrays of plot
  data for each run of a certain param combo
  '''
  multidata_measure = { key: val for key, val in multidata.items() if (key[1] == measure) }
  df = multidata_as_dataframe(multidata_measure, columns)
  columns = columns.copy() + ['run','rand_id']
  return timeseries_similarity_scores_for_simulations(df, target_data)

def timeseries_similarity_for_mean_runs(multidata, measure, columns, target_data):
  multidata_measure = { key: val for key, val in multidata.items() if key[1] == measure }
  df = multidata_means_as_df(multidata_measure, columns)
  return timeseries_similarity_scores_for_simulations(df, target_data)

def timeseries_similarity_scores_for_simulations(df, target_data):
  metrics = { 
    'pearson': lambda simulated, empirical: np.corrcoef(simulated, empirical)[0,1],
    'euclidean': lambda simulated, empirical: np.sqrt(np.sum((empirical - simulated) ** 2)),
    'mape': lambda simulated, empirical: np.mean(np.abs((empirical - simulated) / empirical))
  }
  columns_minus_data = list(df.columns)
  index_of_data = columns_minus_data.index('data')
  columns_minus_data.remove('data')
  df_comparison_results = pd.DataFrame(columns=columns_minus_data + list(metrics.keys()))

  for row in df.iterrows():
    metric_results = [ metric_fn(row[1]['data'][:64], target_data[row[1]['pen_name']]) for metric_fn in metrics.values() ]
    values = list(row[1])
    values.pop(index_of_data)
    df_comparison_results.loc[len(df_comparison_results)] = values + metric_results

  return df_comparison_results

def analysis_dfs():
  data_path = ANALYSIS_DATA_DIR
  graph_types = [ GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H ]
  dfs = {}
  for cascade_type in CASCADE_TYPES:
    ct_str = cascade_type.value
    for graph_type in graph_types:
      gt_str = graph_type.value
      for all_mean in ['all','mean']:
        entire_name = f'{ct_str}-{gt_str}-{all_mean}'
        dfs[entire_name] = pd.read_csv(f'{data_path}/{entire_name}.csv')
        dfs[entire_name].drop(columns=['Unnamed: 0'], inplace=True)
  return dfs

def pen_name_mean_analysis_dfs():
  data_path = f'{ANALYSIS_DATA_DIR}/pen-means'
  dfs = {}
  graph_types = [GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H]
  for cascade_type in CASCADE_TYPES:
    ct_str = cascade_type.value
    for graph_type in graph_types:
      gt_str = graph_type.value
      for all_mean in ['all','mean']:
        entire_name = f'{ct_str}-{gt_str}-{all_mean}'
        dfs[entire_name] = pd.read_csv(f'{data_path}/{entire_name}.csv')
        dfs[entire_name].drop(columns=['Unnamed: 0'], inplace=True)
  return dfs

def add_combo_measures_to_analysis_dfs(dfs):
  for df_name, df in dfs.items():
    df_copy = df.copy()
    df_copy['mape*pearson'] = (1 - df['mape']) * df['pearson']
    dfs[df_name] = df_copy
  return dfs

def analysis_df_name_to_cascade_graph_type(df_name):
  '''
  Return a tuple of CASCADE_TYPE, GRAPH_TYPE from a dataframe name (they should be named
  according to the convention CT.value-GT.value-[all,mean'].csv)
  '''
  cascade_type = None
  graph_type = None
  # Use this list just so 'ba-group-homophily' doesn't improperly come back as GRAPH_TYPES.BA
  graph_types_ordered = [ GRAPH_TYPES.BA_GROUP_H, GRAPH_TYPES.BA, GRAPH_TYPES.ER, GRAPH_TYPES.WS ]
  for ct in CASCADE_TYPES:
    if ct.value in df_name:
      cascade_type = ct
      break
  for gt in graph_types_ordered:
    if gt.value in df_name:
      graph_type = gt
      break
  return (cascade_type,graph_type)

def mean_analysis_dfs_along_pen_name():
  dfs = analysis_dfs()
  dfs_mean_pen = {}
  measures = ['pearson','euclidean','mape']
  for df_name,df in dfs.items():
    print(f'Processing {df_name}...')
    (cascade_type, graph_type) = analysis_df_name_to_cascade_graph_type(df_name)

    param_columns = [ param for param in SIM_PARAM_ORDERS[(cascade_type, graph_type)] ]
    if 'all' in df_name:
      param_columns += ['run']
    # param_columns += measures

    mean_df = None
    if 'all' in df_name:
      mean_df = pd.DataFrame(columns=param_columns+['rand_id']+measures)
    else:
      mean_df = pd.DataFrame(columns=param_columns+measures)

    combined_param_values = {}
    combined_param_values.update(CASCADE_PARAM_VALUES[cascade_type].copy())
    combined_param_values.update(GRAPH_PARAM_VALUES[graph_type].copy())
    param_values = { param: combined_param_values[param] for param in param_columns if (param != 'repetition' and param != 'run') }
    param_values.update({ 'repetition': list(range(NUM_REPETITIONS))})
    if 'all' in df_name:
      param_values.update({ 'run': list(range(NUM_RUNS)) })

    param_combos = itertools.product(*param_values.values())
    for combo in param_combos:
      param_to_value = { param_columns[i]: PARAM_TYPES[param_columns[i]](combo[i]) for i in range(len(combo)) }
      query = ' and '.join([ f'{param} == "{value}"' if type(value) == str else f'{param} == {value}' for param, value in param_to_value.items() ])
      res = df.query(query)
      if len(res) > 0:
        measure_means = {}
        for measure in measures:
          all_runs_data = res[measure]
          measure_means[measure] = all_runs_data.mean()
        # print(mean_df.columns)
        # print(list(param_to_value_no_measures.values()) + list(measure_means.values()))
        if 'all' in df_name:
          mean_df.loc[len(mean_df)] = list(param_to_value.values()) + [res.iloc[0]['rand_id']] + list(measure_means.values())
        else:
          mean_df.loc[len(mean_df)] = list(param_to_value.values()) + list(measure_means.values())
      else:
        print(f'Missing rows for param combo: {combo}')
    dfs_mean_pen[df_name] = mean_df
  return dfs_mean_pen

def write_mean_top_results_latex_single_n(n):
  '''
  Write out the results of write_mean_top_results as LaTeX tables.
  '''
  latex_format = """\\begin{table}[]
      \\centering
      \\begin{tabular}{c|c|c|c}
      \\textbf{Cascade Type} & \\textbf{Graph Topology} & \\textbf{Means} & \\textbf{All}\\\\
      \\hline
      \\multirow{2}{*}{Simple} & BA & simple-ba-mean & simple-ba-all \\\\
      \\cline{2-4}
      & BA-hg & simple-ba-group-homophily-mean & simple-ba-group-homophily-all \\\\
      \\hline
      \\multirow{2}{*}{Complex} & BA & complex-ba-mean & complex-ba-all \\\\
      \\cline{2-4}
      & BA-hg & complex-ba-group-homophily-mean & complex-ba-group-homophily-all \\\\
      \\hline
      \\multirow{2}{*}{Cognitive} & BA & cognitive-ba-mean & cognitive-ba-all \\\\
      \\cline{2-4}
      & BA-hg & cognitive-ba-group-homophily-mean & cognitive-ba-group-homophily-all \\\\
    \\end{tabular}
    \\caption{}
    \\label{tab:results-mean-top}
  \\end{table}"""

  mean_results = mean_top_results([n])
  for top, mean_measures in mean_results.items():
    min_for_top_all = min([ val['mape'] for df_name, val in mean_measures.items() if 'all' in df_name ])
    min_for_top_mean = min([ val['mape'] for df_name, val in mean_measures.items() if 'mean' in df_name ])
    for df_name, measures in mean_measures.items():
      keyword = f'{df_name}'
      measure_value = measures['mape']
      if measure_value == min_for_top_all or measure_value == min_for_top_mean:
        latex_format = latex_format.replace(keyword, f'\\textbf{{{str(measure_value.round(3))}}}')
      else:
        latex_format = latex_format.replace(keyword, str(measure_value.round(3)))

  with open(f'{ANALYSIS_DATA_DIR}/pen-means/mean-top-mape_{n}.tex','w') as f:
    f.write(latex_format)

def write_mean_top_results_latex_multi_n():
  '''
  Write out the results of write_mean_top_results as LaTeX tables.
  '''
  latex_format = """\\begin{table}[]
      \\centering
      \\begin{tabular}{c|c|c|c|c}
      \\textbf{$n$} & \\textbf{Cascade Type} & \\textbf{Graph Topology} & \\textbf{Mean $n$ (Means)} & \\textbf{Mean $n$ (All)}\\\\
      \\hline
      \\multirow{12}{*}{50} & \\multirow{4}{*}{Simple} & ER & simple-er-mean-50 & simple-er-all-50 \\\\
      \\cline{3-5}
      & & WS & simple-ws-mean-50 & simple-ws-all-50 \\\\
      \\cline{3-5}
      & & BA & simple-ba-mean-50 & simple-ba-all-50 \\\\
      \\cline{3-5}
      & & BA-hg & simple-ba-group-homophily-mean-50 & simple-ba-group-homophily-all-50 \\\\
      \\cline{2-5}
      & \\multirow{4}{*}{Complex} & ER & complex-er-mean-50 & complex-er-all-50 \\\\
      \\cline{3-5}
      & & WS & complex-ws-mean-50 & complex-ws-all-50 \\\\
      \\cline{3-5}
      & & BA & complex-ba-mean-50 & complex-ba-all-50 \\\\
      \\cline{3-5}
      & & BA-hg & complex-ba-group-homophily-mean-50 & complex-ba-group-homophily-all-50 \\\\
      \\cline{2-5}
      & \\multirow{4}{*}{Cognitive} & ER & cognitive-er-mean-50 & cognitive-er-all-50 \\\\
      \\cline{3-5}
      & & WS & cognitive-ws-mean-50 & cognitive-ws-all-50 \\\\
      \\cline{3-5}
      & & BA & cognitive-ba-mean-50 & cognitive-ba-all-50 \\\\
      \\cline{3-5}
      & & BA-hg & cognitive-ba-group-homophily-mean-50 & cognitive-ba-group-homophily-all-50 \\\\
      \\hline
      \\multirow{12}{*}{100} & \\multirow{4}{*}{Simple} & ER & simple-er-mean-100 & simple-er-all-100 \\\\
      \\cline{3-5}
      & & WS & simple-ws-mean-100 & simple-ws-all-100 \\\\
      \\cline{3-5}
      & & BA & simple-ba-mean-100 & simple-ba-all-100 \\\\
      \\cline{3-5}
      & & BA-hg & simple-ba-group-homophily-mean-100 & simple-ba-group-homophily-all-100 \\\\
      \\cline{2-5}
      & \\multirow{4}{*}{Complex} & ER & complex-er-mean-100 & complex-er-all-100 \\\\
      \\cline{3-5}
      & & WS & complex-ws-mean-100 & complex-ws-all-100 \\\\
      \\cline{3-5}
      & & BA & complex-ba-mean-100 & complex-ba-all-100 \\\\
      \\cline{3-5}
      & & BA-hg & complex-ba-group-homophily-mean-100 & complex-ba-group-homophily-all-100 \\\\
      \\cline{2-5}
      & \\multirow{4}{*}{Cognitive} & ER & cognitive-er-mean-100 & cognitive-er-all-100 \\\\
      \\cline{3-5}
      & & WS & cognitive-ws-mean-100 & cognitive-ws-all-100 \\\\
      \\cline{3-5}
      & & BA & cognitive-ba-mean-100 & cognitive-ba-all-100 \\\\
      \\cline{3-5}
      & & BA-hg & cognitive-ba-group-homophily-mean-100 & cognitive-ba-group-homophily-all-100 \\\\
    \\end{tabular}
    \\caption{}
    \\label{tab:results-mean-top}
  \\end{table}"""

  mean_results = mean_top_results([50,100])
  for top, mean_measures in mean_results.items():
    min_for_top_all = min([ val['mape'] for df_name, val in mean_measures.items() if 'all' in df_name ])
    min_for_top_mean = min([ val['mape'] for df_name, val in mean_measures.items() if 'mean' in df_name ])
    for df_name, measures in mean_measures.items():
      keyword = f'{df_name}-{top}'
      measure_value = measures['mape']
      if measure_value == min_for_top_all or measure_value == min_for_top_mean:
        latex_format = latex_format.replace(keyword, f'\\textbf{{{str(measure_value.round(3))}}}')
      else:
        latex_format = latex_format.replace(keyword, str(measure_value.round(3)))

  with open(f'{ANALYSIS_DATA_DIR}/pen-means/mean-top-mape.tex','w') as f:
    f.write(latex_format)

def write_mean_top_results_json():
  '''
  Write out the mean of the top X measures for the analysis data. This
  hardcodes a series of X to take the means across, and can be changed
  to supoprt different analyses. This writes the results out to JSON
  in the analyses directory.
  '''
  mean_results = mean_top_results([10,50,100])
  for top, mean_measures in mean_results.items():
    with open(f'{ANALYSIS_DATA_DIR}/pen-means/mean-top-{top}-mape.json', 'w') as f:
      json.dump(mean_measures, f)

def mean_top_results(tops):
  dfs = pen_name_mean_analysis_dfs()
  dfs = add_combo_measures_to_analysis_dfs(dfs)
  measures = ['mape']
  mean_results = {}
  for top in tops:
    mean_measures = mean_top_measures(dfs, measures, top)
    mean_results[top] = mean_measures
  return mean_results

def mean_top_measures(dfs, measures, num_top):
  '''
  Take the mean of the num_top rows for each measure in measures, across
  all dataframes in dfs. Return this as a dictionary keyed by dataframe
  name with values for each measure name and mean.

  :param dfs: The analysis dataframes to analyse.
  :param measures: A list of all measure names (as strings) to take.
  :param num_top: An integer to use as the number of rows (top rows after
  sorting) to take the mean across.
  '''
  return { df_name: {
    measure: df.sort_values(by=measure, ascending=True).head(num_top)[measure].mean() for measure in measures
  } for df_name, df in dfs.items() }

def write_num_high_scoring_metrics_latex():
  latex_format = """\\begin{table}[]
      \\centering
      \\begin{tabular}{p{0.1\linewidth} | p{0.15\linewidth} | p{0.2\linewidth} | p{0.2\linewidth} | p{0.1\linewidth} | p{0.1\linewidth}}
      \\textbf{Cascade Type} & \\textbf{Graph Topology} & \\textbf{Number of Runs (Mean)} & \\textbf{Number of Runs (All)} & \\textbf{Mean Total} & \\textbf{All Total} \\\\
      \\hline
      \\multirow{2}{*}{Simple} & BA & simple-ba-mean & simple-ba-all & \\multirow{2}{*}{simple-mean-total} & \\multirow{2}{*}{simple-all-total} \\\\
      \\cline{2-4}
      & BA-hg & simple-ba-group-homophily-mean & simple-ba-group-homophily-all & & \\\\
      \\hline
      \\multirow{2}{*}{Complex} & BA & complex-ba-mean & complex-ba-all & \\multirow{2}{*}{complex-mean-total} & \\multirow{2}{*}{complex-all-total} \\\\
      \\cline{2-4}
      & BA-hg & complex-ba-group-homophily-mean & complex-ba-group-homophily-all & & \\\\
      \\hline
      \\multirow{2}{*}{Cognitive} & BA & cognitive-ba-mean & cognitive-ba-all & \\multirow{2}{*}{cognitive-mean-total} & \\multirow{2}{*}{cognitive-all-total} \\\\
      \\cline{2-4}
      & BA-hg & cognitive-ba-group-homophily-mean & cognitive-ba-group-homophily-all & & \\\\
    \\end{tabular}
    \\caption{}
    \\label{tab:results-high-scoring}
  \\end{table}"""

  all_dfs = pen_name_mean_analysis_dfs()
  results, high_scoring_dfs = num_high_scoring_metrics()
  totals_means = {}
  totals_all = {}
  for cascade_type in CASCADE_TYPES:
    ct_str = cascade_type.value
    ct_mean_runs = [ num_runs for df_name,num_runs in results.items() if (f'{ct_str}' in df_name and 'mean' in df_name) ]
    ct_all_runs = [ num_runs for df_name,num_runs in results.items() if (f'{ct_str}' in df_name and 'all' in df_name) ]
    totals_means[ct_str] = sum(ct_mean_runs)
    totals_all[ct_str] = sum(ct_all_runs)

    highest_mean = max(ct_mean_runs)
    highest_all = max(ct_all_runs)

    graph_types = [GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H]
    for graph_type in graph_types:
      gt_str = graph_type.value
      df_name_mean = f'{ct_str}-{gt_str}-mean' 
      num_all_mean_results = len(all_dfs[df_name_mean])
      mean_score = "{:.2f}".format(results[df_name_mean] / num_all_mean_results)
      if mean_score == highest_mean:
        latex_format = latex_format.replace(df_name_mean, f'\\textbf{{{str(mean_score)}}}')
      else:
        latex_format = latex_format.replace(df_name_mean, str(mean_score))
      df_name_all = f'{ct_str}-{gt_str}-all' 
      num_all_all_results = len(all_dfs[df_name_all])
      all_score = "{:.2f}".format(results[df_name_all] / num_all_all_results)
      if all_score == highest_all:
        latex_format = latex_format.replace(df_name_all, f'\\textbf{{{str(all_score)}}}')
      else:
        latex_format = latex_format.replace(df_name_all, str(all_score))

  highest_total_mean = max(list(totals_means.values()))
  highest_total_all = max(list(totals_all.values()))
  for cascade_type in CASCADE_TYPES:
    ct_str = cascade_type.value
    total_mean = totals_means[ct_str]
    if total_mean == highest_total_mean:
      latex_format = latex_format.replace(f'{ct_str}-mean-total', f'\\textbf{{{total_mean}}}')
    else:
      latex_format = latex_format.replace(f'{ct_str}-mean-total', str(total_mean))
    total_all = totals_all[ct_str]
    if total_all == highest_total_all:
      latex_format = latex_format.replace(f'{ct_str}-all-total', f'\\textbf{{{total_all}}}')
    else:
      latex_format = latex_format.replace(f'{ct_str}-all-total', str(total_all))

  with open(f'{ANALYSIS_DATA_DIR}/pen-means/num-high-scores.tex','w') as f:
    f.write(latex_format)

def num_high_scoring_metrics():
  '''
  For each analysis dataframe, calculate the number of runs it yielded where the MAPE
  metric was greater than a certain threshold.
  '''
  dfs = pen_name_mean_analysis_dfs()
  dfs = add_combo_measures_to_analysis_dfs(dfs)
  # This is from when we just used MAPE alone, and the check was less than or equal to
  mape_threshold = 0.5
  mape_pearson_threshold = 0.75

  high_scoring_dfs = { df_name: df[df['mape'] <= mape_threshold] for df_name,df in dfs.items() }
  results = { df_name: len(df) for df_name,df in high_scoring_dfs.items() }

  return results, high_scoring_dfs

def top_params_dfs(high_scoring_dfs):
  '''
  Create a dictionary of dataframes to get their top parameters -- essentially
  this just sorts the high_scoring_dfs and takes the top num_top_score_head rows.
  This can be used to generate histograms of the parameter distribution for
  different cascade X topology combinations.

  :param high_scoring_dfs: The result of running num_high_scoring_metrics.
  '''
  num_top_score_head = 100
  top_params_dfs = {}
  for df_name,df in high_scoring_dfs.items():
    df_sorted = df.sort_values(by='mape')
    top_params_dfs[df_name] = df_sorted.head(num_top_score_head)
  return top_params_dfs

def top_params_df_table_df(high_scoring_dfs):
  '''
  Create a dataframe that can be used to output a CSV to visualize the distribution
  of top parameters in a table.

  :param high_scoring_dfs: The result of running num_high_scoring_metrics.
  '''
  num_top_score_head = 50
  top_params_df = pd.DataFrame(columns=['c1','c2','c3','c4','c5'])

  for df_name, df in high_scoring_dfs.items():
    df_sorted = df.sort_values(by='mape')
    print(f'analyzing {df_name}')
    top_params_df.loc[len(top_params_df)] = [df_name, '','','','']
    if '-er-'in df_name:
      if 'simple' in df_name:
        top_params_df.loc[len(top_params_df)] = ['er_p','p','mape','','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['er_p'], row['simple_spread_chance'], row['mape'], '', '' ]
      if 'complex' in df_name:
        top_params_df.loc[len(top_params_df)] = ['er_p','ratio','mape','','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['er_p'], row['complex_spread_ratio'], row['mape'], '', '' ]
      if 'cognitive' in df_name:
        top_params_df.loc[len(top_params_df)] = ['er_p','translate','exponent','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['er_p'], row['cognitive_translate'], row['cognitive_exponent'], row['mape'], '' ]
    if '-ws-'in df_name:
      if 'simple' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ws_p','ws_k','p','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ws_p'], row['ws_k'], row['simple_spread_chance'], row['mape'], '']
      if 'complex' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ws_p','ws_k','ratio','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ws_p'], row['ws_k'], row['complex_spread_ratio'], row['mape'], '']
      if 'cognitive' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ws_p','ws_k','translate','exponent','mape']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ws_p'], row['ws_k'], row['cognitive_translate'], row['cognitive_exponent'], row['mape']]
    if '-ba-group-homophily-'in df_name:
      if 'simple' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','group_homophily','p','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['group_homophily'], row['simple_spread_chance'], row['mape'], '']
      if 'complex' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','group_homophily','ratio','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['group_homophily'], row['complex_spread_ratio'], row['mape'],'']
      if 'cognitive' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','group_homophily','translate','exponent','mape']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['group_homophily'], row['cognitive_translate'], row['cognitive_exponent'], row['mape']]
    elif '-ba-'in df_name:
      if 'simple' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','p','mape','','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['simple_spread_chance'], row['mape'], '', '']
      if 'complex' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','ratio','mape','','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['complex_spread_ratio'], row['mape'], '','']
      if 'cognitive' in df_name:
        top_params_df.loc[len(top_params_df)] = ['ba_m','translate','exponent','mape','']
        for row_tuple in df_sorted.head(num_top_score_head).iterrows():
          row = row_tuple[1]
          top_params_df.loc[len(top_params_df)] = [ row['ba_m'], row['cognitive_translate'], row['cognitive_exponent'], row['mape'],'']
  return top_params_df
  # top_params_df.to_csv(f'{data_path}/top-params.csv')

def plot_sim_run_from_df(data_df, top_df, output_dir='', output_prefix=''):
  '''
  Plot several rows of top results from analysis of simulation data
  against the empirical polling data. This plots the mean of
  all run repetitions with its standard deviation, getting each
  individual run data from data_df, and the top parameter combinations
  from top_df.

  :param data_df: A dataframe containing all run data for a given
  simulation cascade type X graph type.
  :param top_df: A dataframe containing the top matches of simulated
  data against polling data.
  :output_dir: Optional -- string specifying the directory to save
  plot figures in.
  :output_prefix: Optional -- the beginning of the filename used to
  save plot figurse (this is currently set as the contagion X graph
  combination)
  '''
  num_rows_to_plot = 1
  top_rows = top_df.iloc[0:num_rows_to_plot,:]
  query_cols = list_subtract(list(top_df.columns), ['measure','mape','pearson','euclidean','mape*pearson'])
  i = 0
  for row in top_rows.iterrows():
    row_data = row[1]
    query = ' and '.join([ f'{col}=={row_data[col]}' for col in query_cols ])
    data_rows = data_df.query(query)
    gallup_dict = read_gallup_data_into_dict('../labeled-data/public/gallup-polling.csv')
    plot_opinion_timeseries_against_polling(data_rows, gallup_dict, output_dir, f'{output_prefix}-{i}_{"-".join([str(row_data[col]) for col in query_cols])}.png')
    i+=1

def plot_belief_exposure_by_belief(data_path, out_filename):
  '''
  Generate and save plots showing exposure and belief of different messages
  by different groups of agents.

  :param data_path: The raw data path containing message files to process
  for one repetition of simulation trials.
  :param out_filename: A filename piece to use to distinguish the saved
  plots from other plots.
  '''
  message_multidata = process_multi_message_data(data_path)
  combined_exposure_by_belief = message_exposure_by_belief_multi_analysis(message_multidata)
  combined_belief_by_belief = message_belief_by_belief_multi_analysis(message_multidata)
  graph_messages_interaction_by_belief('', combined_exposure_by_belief, 'heard', out_path='../paper-figures/cascade-experiment-figures', out_name=f'exposure-by-belief_{out_filename}.png')
  graph_messages_interaction_by_belief('', combined_belief_by_belief, 'believed', out_path='../paper-figures/cascade-experiment-figures', out_name=f'belief-by-belief_{out_filename}.png')

def plot_belief_exposure_by_group(data_path, graph_path, out_filename):
  '''
  Generate and save plots showing exposure and belief of different messages
  by different groups of agents.

  :param data_path: The raw data path containing message files to process
  for one repetition of simulation trials.
  :param graph_path: A path to the corresponding graph used for simulation
  for those runs on the repetition.
  :param out_filename: A filename piece to use to distinguish the saved
  plots from other plots.
  '''
  combined_group_exposure = message_exposure_by_group_multi_analysis(data_path, graph_path)
  combined_group_belief = message_belief_by_group_multi_analysis(data_path, graph_path)
  graph_message_interaction_by_belief_by_group('', combined_group_exposure, 'heard', out_path='../paper-figures/cascade-experiment-figures', out_name=f'exposure-by-belief-by-group_{out_filename}.png')
  graph_message_interaction_by_media_by_group('', combined_group_exposure, 'heard', out_path='../paper-figures/cascade-experiment-figures', out_name=f'exposure-by-media-by-group_{out_filename}.png')
  graph_message_interaction_by_belief_by_group('', combined_group_belief, 'believed', out_path='../paper-figures/cascade-experiment-figures', out_name=f'belief-by-belief-by-group_{out_filename}.png')
  graph_message_interaction_by_media_by_group('', combined_group_belief, 'believed', out_path='../paper-figures/cascade-experiment-figures', out_name=f'belief-by-media-by-group_{out_filename}.png')

def process_experiment_data_to_dfs():
  data_out = SIM_DF_DATA_DIR
  contagion_types_tested = [ CASCADE_TYPES.SIMPLE, CASCADE_TYPES.COMPLEX, CASCADE_TYPES.COGNITIVE ] #['simple','complex','cognitive']
  graph_topos_tested = [ GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H ] #['er','ws','ba','ba-group-homophily']
  contagion_graph_combos = itertools.product(contagion_types_tested,graph_topos_tested)
  fns = {
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER): get_simple_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS): get_simple_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): get_simple_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): get_simple_contagion_param_sweep_BA_group_homophily_multidata,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER): get_complex_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS): get_complex_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): get_complex_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): get_complex_contagion_param_sweep_BA_group_homophily_multidata,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER): get_cognitive_contagion_param_sweep_ER_multidata,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS): get_cognitive_contagion_param_sweep_WS_multidata,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): get_cognitive_contagion_param_sweep_BA_multidata,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): get_cognitive_contagion_param_sweep_BA_group_homophily_multidata,
  }
  measures_to_report = ['opinion-timeseries']
  for combo in contagion_graph_combos:
    cascade_type = combo[0]
    graph_type = combo[1]
    ct_str = cascade_type.value
    gt_str = graph_type.value
    all_filename = f'{data_out}/{ct_str}-{gt_str}-all.csv' 
    mean_filename = f'{data_out}/{ct_str}-{gt_str}-mean.csv'
    if exists(all_filename) and exists(mean_filename):
      print(f'Skipping {ct_str}-{gt_str} both because data exists')
      continue

    all_df = None
    if exists(all_filename):
      print(f'Skipping {ct_str}-{gt_str}-all because data exists')
      all_df = read_dataframe_with_simulation_data(all_filename)
    else:
      print('Generating all runs dataframe...')
      multidata = fns[(cascade_type,graph_type)](f'{SIM_RAW_DATA_DIR}/{ct_str}-contagion-sweep-{GRAPH_TYPE_DIRECTORY_NAMES[graph_type]}')
      multidata_measure = { key: val for key, val in multidata.items() if key[1] in measures_to_report }
      all_params = SIM_PARAM_ORDERS[(cascade_type, graph_type)]
      all_df = multidata_as_dataframe(multidata_measure, all_params)
      write_dataframe_with_simulation_data(all_df, all_filename)

    if exists(mean_filename):
      print(f'Skipping {ct_str}-{gt_str}-mean because data exists')
    else:
      print('Generating mean runs dataframe...')
      # mean_df = mean_multidata_as_dataframe(multidata_measure, all_params)
      mean_df = all_dataframe_to_mean_dataframe(all_df)
      write_dataframe_with_simulation_data(mean_df, mean_filename)

def process_all_exp_metrics():
  cascade_types = [ CASCADE_TYPES.SIMPLE, CASCADE_TYPES.COMPLEX, CASCADE_TYPES.COGNITIVE ]
  graph_topologies = [ GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H ]

  metric_functions = {
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER): metrics_for_simple_contagion_param_sweep_ER,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS): metrics_for_simple_contagion_param_sweep_WS,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): metrics_for_simple_contagion_param_sweep_BA,
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): metrics_for_simple_contagion_param_sweep_BA_group_homophily,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER): metrics_for_complex_contagion_param_sweep_ER,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS): metrics_for_complex_contagion_param_sweep_WS,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): metrics_for_complex_contagion_param_sweep_BA,
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): metrics_for_complex_contagion_param_sweep_BA_group_homophily,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER): metrics_for_cognitive_contagion_param_sweep_ER,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS): metrics_for_cognitive_contagion_param_sweep_WS,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): metrics_for_cognitive_contagion_param_sweep_BA,
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): metrics_for_cognitive_contagion_param_sweep_BA_group_homophily,
  }
  processing_functions = {
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER): {
      'mean': process_simple_contagion_param_sweep_ER_top_mean,
      'all': process_simple_contagion_param_sweep_ER_top_all,
    },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS): {
      'mean': process_simple_contagion_param_sweep_WS_top_mean,
      'all': process_simple_contagion_param_sweep_WS_top_all,
    },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): {
      'mean': process_simple_contagion_param_sweep_BA_top_mean,
      'all': process_simple_contagion_param_sweep_BA_top_all,
    },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): {
      'mean': process_simple_contagion_param_sweep_BA_group_homophily_top_mean,
      'all': process_simple_contagion_param_sweep_BA_group_homophily_top_all,
    },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER): {
      'mean': process_complex_contagion_param_sweep_ER_top_mean,
      'all': process_complex_contagion_param_sweep_ER_top_all,
    },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS): {
      'mean': process_complex_contagion_param_sweep_WS_top_mean,
      'all': process_complex_contagion_param_sweep_WS_top_all,
    },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): {
      'mean': process_complex_contagion_param_sweep_BA_top_mean,
      'all': process_complex_contagion_param_sweep_BA_top_all,
    },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): {
      'mean': process_complex_contagion_param_sweep_BA_group_homophily_top_mean,
      'all': process_complex_contagion_param_sweep_BA_group_homophily_top_all,
    },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER): {
      'mean': process_cognitive_contagion_param_sweep_ER_top_mean,
      'all': process_cognitive_contagion_param_sweep_ER_top_all,
    },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS): {
      'mean': process_cognitive_contagion_param_sweep_WS_top_mean,
      'all': process_cognitive_contagion_param_sweep_WS_top_all,
    },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): {
      'mean': process_cognitive_contagion_param_sweep_BA_top_mean,
      'all': process_cognitive_contagion_param_sweep_BA_top_all,
    },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): {
      'mean': process_cognitive_contagion_param_sweep_BA_group_homophily_top_mean,
      'all': process_cognitive_contagion_param_sweep_BA_group_homophily_top_all,
    }
  }
  metrics = {
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER): [],
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS): [],
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): [],
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): [],
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER): [],
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS): [],
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): [],
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): [],
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER): [],
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS): [],
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): [],
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): [],
  }
  top_dfs = {
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.ER): { 'all': None, 'mean': None },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.WS): { 'all': None, 'mean': None },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA): { 'all': None, 'mean': None },
    (CASCADE_TYPES.SIMPLE, GRAPH_TYPES.BA_GROUP_H): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.ER): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.WS): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COMPLEX, GRAPH_TYPES.BA_GROUP_H): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.ER): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.WS): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA): { 'all': None, 'mean': None },
    (CASCADE_TYPES.COGNITIVE, GRAPH_TYPES.BA_GROUP_H): { 'all': None, 'mean': None },
  }
  data_path = ANALYSIS_DATA_DIR + '/pen-means'
  # data_path = ANALYSIS_DATA_DIR

  for cascade_type in cascade_types:
    for graph_topology in graph_topologies:
      ct_str = cascade_type.value
      gt_str = graph_topology.value
      ct_gt_combo = (cascade_type,graph_topology)
      gt_dir_name = GRAPH_TYPE_DIRECTORY_NAMES[graph_topology]

      # Generate metrics for all and mean data
      if exists(f'{data_path}/{ct_str}-{gt_str}-all.csv') and exists(f'{data_path}/{ct_str}-{gt_str}-mean.csv'):
        print(f'Read in existing {ct_str} {gt_str} metric data')
        all_df = pd.read_csv(f'{data_path}/{ct_str}-{gt_str}-all.csv')
        mean_df = pd.read_csv(f'{data_path}/{ct_str}-{gt_str}-mean.csv')
        all_df.drop(columns=['Unnamed: 0'], inplace=True)
        mean_df.drop(columns=['Unnamed: 0'], inplace=True)
        metrics[ct_gt_combo].append(all_df)
        metrics[ct_gt_combo].append(mean_df)
      else:
        metrics[ct_gt_combo] = metric_functions[(cascade_type,graph_topology)](f'{SIM_RAW_DATA_DIR}/{ct_str}-contagion-sweep-{gt_dir_name}')
        metrics[ct_gt_combo][0].to_csv(f'{data_path}/{ct_str}-{gt_str}-all.csv')
        metrics[ct_gt_combo][1].to_csv(f'{data_path}/{ct_str}-{gt_str}-mean.csv')
      metrics_all = metrics[ct_gt_combo][0]
      metrics_mean = metrics[ct_gt_combo][1]

      # Get top results for all and mean
      if exists(f'{data_path}/{ct_str}-{gt_str}-all_top.csv'):
        top_dfs[ct_gt_combo]['all'] = pd.read_csv(f'{data_path}/{ct_str}-{gt_str}-all_top.csv')
        top_dfs[ct_gt_combo]['all'].drop(columns=['Unnamed: 0'], inplace=True)
      else:
        top_dfs[ct_gt_combo]['all'] = top_matches_for_metrics(metrics_all)
        top_dfs[ct_gt_combo]['all'].to_csv(f'{data_path}/{ct_str}-{gt_str}-all_top.csv')
      if exists(f'{data_path}/{ct_str}-{gt_str}-mean_top.csv'):
        top_dfs[ct_gt_combo]['mean'] = pd.read_csv(f'{data_path}/{ct_str}-{gt_str}-mean_top.csv')
        top_dfs[ct_gt_combo]['mean'].drop(columns=['Unnamed: 0'], inplace=True)
      else:
        top_dfs[ct_gt_combo]['mean'] = top_matches_for_metrics(metrics_mean)
        top_dfs[ct_gt_combo]['mean'].to_csv(f'{data_path}/{ct_str}-{gt_str}-mean_top.csv')

      # Graph the top results
      all_df = read_dataframe_with_simulation_data(f'{SIM_DF_DATA_DIR}/{ct_str}-{gt_str}-all.csv')
      if 'Unnamed: 0' in all_df.columns:
        all_df.drop(columns=['Unnamed: 0'], inplace=True)
      poll_plot_output_dir = f'{data_path}/sim-poll-plots'
      plot_sim_run_from_df(all_df, top_dfs[ct_gt_combo]['mean'], output_dir=poll_plot_output_dir, output_prefix=f'{ct_str}-{gt_str}')

def get_df_columns_in_order():
  # data_paths = [ SIM_DF_DATA_DIR, ANALYSIS_DATA_DIR, f'{ANALYSIS_DATA_DIR}/pen-means' ]
  data_paths = [ SIM_DF_DATA_DIR, f'{ANALYSIS_DATA_DIR}/pen-means' ]
  dfs = {}
  select_graph_types = [ GRAPH_TYPES.BA_GROUP_H ]
  for data_path in data_paths:
    for cascade_type in CASCADE_TYPES:
      ct_str = cascade_type.value
      cascade_params = CASCADE_PARAMETERS[cascade_type]
      # for graph_type in select_graph_types:
      for graph_type in GRAPH_TYPES:
        gt_str = graph_type.value
        graph_params = GRAPH_PARAMETERS[graph_type]
        for all_mean in ['all','mean']:
          filename = f'{ct_str}-{gt_str}-{all_mean}.csv'
          full_path = f'{data_path}/{filename}'
          print(f'Fixing {full_path}...')
          df = pd.read_csv(full_path)
          df.drop(columns=['Unnamed: 0'], inplace=True)
          renamed_df = df.copy()
          ct_gt_column_order = graph_params + cascade_params
          non_ct_gt_cols = list_subtract(list_subtract(list(df.columns), cascade_params), graph_params)
          # Note: This is just an artifact of how the functions were originally written
          # For future functions that work dynamically, the order should go back to
          # cascade params, graph params, extras
          all_cols = None
          if graph_type == GRAPH_TYPES.BA_GROUP_H:
            # This one was done out of the typical order :(
            if cascade_type == CASCADE_TYPES.SIMPLE:
              all_cols = ['ba_m','simple_spread_chance','group_homophily'] + non_ct_gt_cols
            elif cascade_type == CASCADE_TYPES.COMPLEX:
              all_cols = ['ba_m','complex_spread_ratio','group_homophily'] + non_ct_gt_cols
            elif cascade_type == CASCADE_TYPES.COGNITIVE:
              all_cols = ['ba_m','cognitive_translate','cognitive_exponent','group_homophily'] + non_ct_gt_cols
          else:
            all_cols = ct_gt_column_order + non_ct_gt_cols
          for i in range(len(df.columns)):
            # renamed_df.rename(columns={df_col: new_col}, inplace=True)
            renamed_df.columns.values[i] = all_cols[i]
          dfs[full_path] = renamed_df
          renamed_df.to_csv(full_path)
  return dfs

def get_dfs_with_column_types_correct():
  data_path = f'{ANALYSIS_DATA_DIR}/pen-means'
  dfs = {}
  select_graph_types = [ GRAPH_TYPES.BA, GRAPH_TYPES.BA_GROUP_H ]
  for cascade_type in CASCADE_TYPES:
    ct_str = cascade_type.value
    cascade_params = CASCADE_PARAMETERS[cascade_type]
    for graph_type in select_graph_types:
    # for graph_type in GRAPH_TYPES:
      gt_str = graph_type.value
      graph_params = GRAPH_PARAMETERS[graph_type]
      for all_mean in ['all','mean']:
        filename = f'{ct_str}-{gt_str}-{all_mean}.csv'
        full_path = f'{data_path}/{filename}'
        print(f'Fixing {full_path}...')
        df = pd.read_csv(full_path)
        df.drop(columns=['Unnamed: 0'], inplace=True)
        typed_df = df.copy()
        for param in cascade_params + graph_params:
          typed_df[param] = typed_df[param].astype(PARAM_TYPES[param])
        dfs[full_path] = typed_df
        typed_df.to_csv(full_path)
  return dfs

'''
================
ANALYSES OF RESULTS
================
'''