import numpy as np

def distribute_beliefs_per_citizen(belief_to_dist, malleables):
  if len(belief_to_dist) == 1:
    return np.array(list(map(lambda el: [int(el)], list(belief_to_dist.values())[0])))

  stacked_beliefs = np.array([])
  num_cits = -1
  for bel,dist in belief_to_dist.items():
    if len(stacked_beliefs) == 0:
      stacked_beliefs = np.array(dist, dtype=int)
      num_cits = len(dist)
    else:
      stacked_beliefs = np.vstack((stacked_beliefs, np.array(dist, dtype=int)))
  per_citizen_beliefs = []
  for i in range(num_cits):
    per_citizen_beliefs.append(stacked_beliefs[:,i])
  return per_citizen_beliefs