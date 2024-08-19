import os
import json
import math
import numpy as np
from scipy.stats import truncnorm

'''
Generic utilities file to keep track of useful functions.
'''

def dict_sort(d, reverse=False):
  return {key: value for key, value in sorted(d.items(), key=lambda item: item[1], reverse=reverse)}

def rgb_to_hex(rgb):
    return '%02x%02x%02x' % tuple(rgb)

def create_nested_dirs(path):
  path_thus_far = path.split('/')[0]
  for d in path.split('/')[1:]:
    if not os.path.isdir(f'{path_thus_far}/{d}'):
      # print(f'Creating {path_thus_far}/{d}')
      os.mkdir(f'{path_thus_far}/{d}')
    path_thus_far += f'/{d}'

def curr_sigmoid_p_dynamic(cognitive_fn):
  '''
  A curried sigmoid function used to calculate probabilty of belief
  given a certain distance. This way, it is initialized to use exponent
  and translation, and can return a function that can be vectorized to
  apply with one param -- message_distance.

  :param exponent: An exponent factor in the sigmoid function.
  :param translation: A translation factor in the sigmoid function.
  '''
  sigmoid_vals = {
    # (exponent,translate)
    'sigmoid-polarizing-stubborn': [
      (4,0),
      (3,1),
      (3,1),
      (2,2),
      (3,1),
      (3,1),
      (4,0)
    ],
    'sigmoid-polarizing-mid': [
      (3,1),
      (2,2),
      (2,2),
      (1,3),
      (2,2),
      (2,2),
      (3,1)
    ]
  }
  return lambda bel: curr_sigmoid_p(sigmoid_vals[cognitive_fn][bel][0], sigmoid_vals[cognitive_fn][bel][1])

def curr_sigmoid_p(exponent, translation):
  '''
  A curried sigmoid function used to calculate probabilty of belief
  given a certain distance. This way, it is initialized to use exponent
  and translation, and can return a function that can be vectorized to
  apply with one param -- message_distance.

  :param exponent: An exponent factor in the sigmoid function.
  :param translation: A translation factor in the sigmoid function.
  '''
  return lambda message_distance: (1 / (1 + math.exp(exponent * (message_distance - translation))))

def sigmoid_contagion_p(message_distance, exponent, translation):
  '''
  A sigmoid function to calcluate probability of belief in a given distance
  between beliefs, governed by a few parameters.
  '''
  return (1 / (1 + math.exp(exponent * (message_distance - translation))))

def uniform_dist_multiple(maxx, n, k):
  '''
  Return a series of samples drawn from a uniform distribution from [0, max] where each of en samples has k entries.

  :param maxx: The maximum to draw from.
  :param n: The number of k entry samples to draw.
  :param k: The number of entries per n sample.
  '''
  samples = [ uniform_dist(maxx, n) for i in range(k) ]
  return [ [ samples[i][j] for i in range(k) ] for j in range(n) ]

def uniform_dist(maxx, n):
  '''
  Draw n samples from a uniform distribution from [0, maxx]

  :param maxx: The maximum to draw from.
  :param n: The number of samples to take.
  '''
  return np.array(list(map(lambda el: math.floor(el), np.random.uniform(low=0, high = maxx, size=n))))

def normal_dist_multiple(maxx, mean, sigma, n, k):
  '''
  Return a series of samples drawn from a normal distribution from
  [0, max] with mean and std deviation specified, where each of en
  samples has k entries.

  :param maxx: The maximum to draw from.
  :param mean: The mean of the distribution.
  :param sigma: The standard deviation of the distribution.
  :param n: The number of k entry samples to draw.
  :param k: The number of entries per n sample.
  '''
  samples = [ normal_dist(maxx, mean, sigma, n) for i in range(k) ]
  return [ [ samples[i][j] for i in range(k) ] for j in range(n) ]

def normal_dist(maxx, mean, sigma, n):
  '''
  Draw n samples from a truncated normal distribution from [0, maxx]
  with mean and sigma specified.

  :param maxx: The maximum to draw from.
  :param mean: The mean of the distribution.
  :param sigma: The standard deviation of the distribution.
  :param n: The number of samples to take.
  '''
  lower=-0.5
  upper=maxx+0.5
  # mean=math.floor(resolution/2)
  # sigma=mean/3
  dist = truncnorm((lower - mean) / sigma, (upper - mean) / sigma, loc=mean, scale=sigma)
  return np.array(list(map(lambda el: math.floor(el), dist.rvs(n))))

def matched_normal_dist(maxx, mean, sigma, n, target_vals, target_count, threshold):
  sample_val_count = 0
  dist = []
  while abs(sample_val_count - target_count) > threshold:
    dist = normal_dist(maxx, mean, sigma, n)
    vals = np.zeros(len(dist)).astype(bool)
    for val in target_vals:
      vals = vals | (dist == val)
    sample_val_count = vals.sum()
  return dist

def test_initial_belief_distribution():
  distribution_data = json.load(open('./gallup-cit-init-dist.json', 'r'))
  distributions = { group: { belief: [] for belief in data['beliefs'].keys() } for group, data in distribution_data.items() }
  for group, specs in distribution_data.items():
    print(f'Calculating for group {group}...')
    for belief_name, params in specs['beliefs'].items():
      for i in range(100):
        print(f'Sample {i}')
        dist = params[0]
        if dist == 'normal':
          mean = params[1]
          std = params[2]
          distributions[group][belief_name].append(normal_dist(6, mean, std, specs['n']))
        elif dist == 'matched_normal':
          mean = params[1]
          std = params[2]
          target_val = params[3]
          target_count = params[4]
          threshold = params[5]
          distributions[group][belief_name].append(matched_normal_dist(6, mean, std, specs['n'], target_val, target_count, threshold))
  supportive_beliefs = {
    group: {
      belief: [ [ len([bel for bel in dist if bel >= 5]) for dist in distributions[group][belief] ] ] for belief in data['beliefs'].keys()
    } for group, data in distribution_data.items()
  }
  means_and_vars = {
    group: { belief: {
      'mean': np.array(supportive_beliefs[group][belief]).mean(),
      'var': np.array(supportive_beliefs[group][belief]).var(),
      'std': np.array(supportive_beliefs[group][belief]).std()
    } for belief in data['beliefs'].keys() }
  for group,data in distribution_data.items() }
  return means_and_vars

def list_subtract(l1, l2):
  return [ el for el in l1 if el not in l2 ]
  