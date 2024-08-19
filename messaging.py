import numpy as np
import networkx as nx
import math
import json
from random import *
from copy import deepcopy
import sys
import os
from enums import INSTITUTION_MESSAGING_TYPES, messaging_file_names
from enums import INSTITUTION_ECOSYSTEM_TYPES
from enums import eco_file_names
from utils import curr_sigmoid_p, curr_sigmoid_p_dynamic, sigmoid_contagion_p

"""
MESSAGES

In general a message should be defined as a dictionary:
    { A: x, B: y, C: z, ... }
Where each key is an agent attribute, and each value is the agent's corresponding value given that attribute's set.

Two messages should be compared by their Euclidean distance from each other - taking into account all of the attributes of the message.

AGENT BRAINS:

Each agent brain consists of multiple attributes, { A, B, C, ... } each with a value taken from the attribute space.

Updating beliefs:
An agent's brain is used for receptivity to messages, and each agent has a threshold "beta" for which these rules apply:
    - If the distance between the message and the agent's brain is within beta, the agent will increment any of their "attribute counters" (say an agent brain consists of { A, B } where A in [1,2,3] and B in [1,2,3,4]. If the agent's brain is { 2, 3 } and the message within beta is { 3, 3 }, then the agent will increment a counter on A such that they are becoming "more inclined" to believe A = 3 rather than A = 2. There should be a threshold for belief updating "gamma" that the counter must reach before the agent switches beliefs. 

"""

"""
MESSAGING FUNCTIONS
"""

"""
Get the distance between two different messages by Euclidean distance.

:param m1: The first message.
:param m2: The second message.
"""
def message_distance(m1, m2):
    return np.linalg.norm(m1-m2)

"""
Get the distance between two messages with weighted attributes.

Note: Each message and weight param should simply be an ARRAY OF VALUES.

:param m1: The first message.
:param m2: The second message.
:param m1_weights: The weighting for each index of m1.
:param m2_weights: The weighting for each index of m2.
"""
def weighted_message_distance(m1, m2, m1_weights, m2_weights):
    weighted_m1 = m1 * m1_weights
    weighted_m2 = m2 * m2_weights
    return np.linalg.norm(weighted_m1-weighted_m2)

def message_as_array(m):
    return np.array(list(m.values()))

"""
Generate a random message based on distributions of each
attribute in a list of attributes.

:param attrs: An array of attributes from the Attributes enumeration.
"""
def random_message(attrs):
    m = {}
    for attr in attrs:
        if attr.name in AttributeDistributions:
            m[attr.name] = random_dist_sample(attr)
        else:
            m[attr.name] = random_sample(attr)
    return m 

def test_random_message():
    attrs = [ Attributes.I, Attributes.P, Attributes.VG ]
    m = random_message(attrs)
    print(m)

"""
AGENT BRAIN FUNCTIONS
"""

"""
Create an agent brain model. Paramters are described below.

:param ID: The ID of the agent.
:param prior_attributes: Unchanging attributes the agent has beliefs about - from the Attributes enumeration.
:param malleable_attributes: Malleable attributes the agent has beliefs about - from the Attributes enumeration.
:param prior_initial: An initial configuration for the agent's attributes.
:param malleable_initial: An initial configuration for the agent's attributes.
:param beta: The value for the agent's willingness to update beliefs - if a
  message is received an its distance to the agent's brain is within beta, the
  agent will start to lean toward updating its belief by incrementing a token
  counter for the given direction of differing attributes.
  belief.
:param alpha: The value for the agent's willingness to share a message - if a
  message is received and its distance to the agent's brain is within alpha, the
  agent will share that message to its neighbors.
"""
def create_agent_brain(
    ID,
    prior_attributes,
    malleable_attributes,
    prior_initial,
    malleable_initial,
    brain_type,
    beta=1.0,
    alpha=1.0
    ):
    agent = {
        'ID': ID,
        'alpha': alpha,
        'beta': beta,
        'malleable': malleable_attributes,
        'prior': prior_attributes,
        # 'malleable': list(map(lambda attr: attr.name, malleable_attributes)),
        # 'prior': list(map(lambda attr: attr.name, prior_attributes)),
        'cont_tokens': {},
    }
    # Initialize malleable beliefs w/ tokens for belief updates
    for i in range(0, len(malleable_attributes)):
      attr = malleable_attributes[i]
      # agent[attr.name] = malleable_initial[i]
      agent[attr] = malleable_initial[i]

    # Initialize prior beliefs that cannot change - no tokens
    for i in range(0, len(prior_attributes)):
        attr = prior_attributes[i]
        # agent[attr.name] = prior_initial[i]
        agent[attr] = prior_initial[i]

    if brain_type == 'discrete':
      for i in range(0, len(malleable_attributes)):
        attr = malleable_attributes[i]
        # agent[attr.name] = malleable_initial[i]
        agent[attr] = malleable_initial[i]
        # Initialize belief change counters
    elif brain_type == 'continuous':
      for i in range(0, len(malleable_attributes)):
        attr = malleable_attributes[i]
        # agent['cont_tokens'][attr.name] = malleable_initial[i]
        agent['cont_tokens'][attr] = malleable_initial[i]

    return agent

"""
Have an agent compare its beliefs to those in the message. The distance between
the agent's beliefs and the message content will determine whether the agent
is inclined to agree with the message and move its beliefs in the corresponding
direction. If the distance is adequate, the agent will add tokens to its
belief update counters, and if the token threshold for change is met, the agent
will update its beliefs.

:param agent: The agent to receieve the message.
:param message: The message to be receieved.
:param spread_type: Either "simple", "complex" or "buckets" to say which type
of spread to simulate.
"""
def receive_message(agent, message, spread_type):
    dist = dist_to_agent_brain(agent, message)

    # Update agent belief tokens if the message is within updating threshold
    if dist <= agent['beta']:
      believe_message(agent, message, spread_type)

    return agent

"""
Believe a message: have an agent react to the mesasge based on the type
of spread being utilized.

:param agent: The agent to receieve the message.
:param message: The message to be receieved.
:param spread_type: Either "simple", "complex" or "buckets" to say which type
of spread to simulate.
"""
def believe_message(agent, message, spread_type, brain_type):
  if brain_type == 'discrete':
    agent_beliefs = agent_beliefs_from_message(agent, message)
    agent_empty_beliefs = list(filter(lambda key: agent_beliefs[key] == -1, agent_beliefs))
    for m in message:
      # We made a decision so that even if an agent doesn't believe
      # the rest of the message, they adopt beliefs that they 
      # didn't have
      if m in agent['malleable'] or m in agent_empty_beliefs: agent[m] = message[m]

  elif brain_type == 'continuous':
    for attr in filter(lambda el: el in agent['malleable'], message):
      cont_attr = agent['cont_tokens'][attr]
      agent['cont_tokens'][attr] = (cont_attr + message[attr]) / 2
      agent[attr] = round(cont_attr)

  return agent

'''
Get all agent beliefs as a dictionary of belief name and value.

:param agent: The agent brain object.
'''
def agent_brain_belief_dict(agent):
  all_beliefs = []
  all_beliefs.extend(sorted(agent['malleable']))
  all_beliefs.extend(sorted(agent['prior']))
  temp_message = { bel: -1 for bel in all_beliefs }
  return agent_beliefs_from_message(agent, temp_message)

"""
Get the distance between two agent brains.

:param agent: The agent's brain.
:param message: The message.
"""
def dist_between_agent_brains(agent1, agent2):
  agent1_beliefs = agent_brain_belief_dict(agent1)
  agent2_beliefs = agent_brain_belief_dict(agent2)
  a1_arr = message_as_array(agent1_beliefs)
  a2_arr = message_as_array(agent2_beliefs)
  return message_distance(a1_arr, a2_arr)

"""
Get the distance between a message and the corresponding vector of agent brain
parameters.

:param agent: The agent's brain.
:param message: The message.
"""
def dist_to_agent_brain(agent, message):
  agent_beliefs = agent_beliefs_from_message(agent, message)
  agent_nonempty_beliefs = {key: val for (key, val) in agent_beliefs.items() if val != -1}
  m_arr = message_as_array(message)
  a_arr = message_as_array(agent_nonempty_beliefs)
  return message_distance(m_arr, a_arr)

"""
Get the weighted distance between a message and the corresponding vector of agent brain
parameters.

:param agent: The agent's brain.
:param message: The message.
:param weights: The weighting scheme to use for both messages
"""
def weighted_dist_to_agent_brain(agent, message, weight):
    a_arr = agent_belief_vec_from_message(agent, message)
    w_arr = np.array([ weight for attr in message ])
    m_arr = message_as_array(message)

    return weighted_message_distance(m_arr, a_arr, w_arr, w_arr)

def agent_beliefs_from_message(agent, message):
  agent_beliefs = {}
  for attr in message:
    if attr in agent:
      agent_beliefs[attr] = agent[attr]
    else:
      agent_beliefs[attr] = -1
  return agent_beliefs

def agent_belief_vec_from_message(agent, message):
    agent_beliefs = []
    for attr in message:
        agent_beliefs.append(agent[attr])
    return np.array(agent_beliefs)

def agent_trust_in_other_belief_func(agent_memory, agent_brain, topic_beliefs, bel_func):
  '''
  Return agent trust in another agent based off of a certain trust function
  applied to the difference between their memory and their internal beliefs.

  :param agent_memory: The agent's memory of the other agent's messages
  for all belief propositions.
  :param agent_brain: The agent's brain, including its internal beliefs.
  :param topic_beliefs: The beliefs relevant to the topic trust is being
  calculated for.
  :param bel_func: The function used to calculate belief values between memory
  and the agent's beliefs. This should be a function that takes one input so
  it can be vectorized.
  '''
  # TODO: Make this more general than just 'A'
  if bel_func == curr_sigmoid_p_dynamic:
    bel_func = bel_func(agent_brain['A'])

  bf_vectorized = np.vectorize(bel_func)
  topic_memories = { bel: np.array(list(map(lambda el: int(el), mem))) for (bel, mem) in agent_memory.items() if bel in topic_beliefs }
  topic_mem_diffs = { bel: bf_vectorized(abs(mem - agent_brain[bel])) for (bel,mem) in topic_memories.items() if len(mem) > 0 }
  all_belief_vals = np.array([ val for val in topic_mem_diffs.values() ])
  # print(f'trust between {agent_memory} and {agent_brain} is {all_belief_vals.mean()}')
  return all_belief_vals.mean()
  
def one_spread_iteration(G, agent, message, bel_fn):
  # N = len(G.nodes)
  adj = []
  for i in G.nodes:
    adj.append([ (i,j) in G.edges for j in G.nodes ])
  adj = np.matrix(adj).astype(int)
  # adj = nx.adjacency_matrix(G)
  dists = np.array([ dist_to_agent_brain(G.nodes[i],message) for i in G.nodes ])
  pf_vec = np.vectorize(bel_fn)
  neighbors = adj[agent].toarray()
  neighbors_bel = np.multiply(dists, neighbors)
  ps = np.multiply(pf_vec(neighbors_bel),neighbors)
  beliefs = (ps >= random()).astype(int)
  return np.nonzero(beliefs)[1]

def spread_from(G, agents, message, bel_fn, limit):
  '''
  Spread a message from a set of agents through the graph G, based on
  belief function bel_fn. This stops once all the new agents probabilities
  of believing (done in rounds) is below limit.

  :param G: The graph of citizen agents.
  :param agents: The initial set of agents to spread from.
  :param message: The message to spread.
  :param bel_fn: The belief function to use when an agent is presented with
  a message.
  :param limit: The probability to stop at once all new agents reached with
  the message have p < limit.
  '''
  max_loops = 10
  cur_loop = 0
  N = len(G.nodes)
  adj = []
  for i in G.nodes:
    adj.append([ (i,j) in G.edges for j in G.nodes ])
  adj = np.matrix(adj).astype(int)
  dists = np.array([ dist_to_agent_brain(G.nodes[i],message) for i in G.nodes ])
  pf_vec = np.vectorize(bel_fn)
  cont = True
  curr_agents = np.array([ int(i in agents) for i in range(N) ])
  heard_agents = np.zeros(N)
  believed_agents = np.zeros(N)
  heard_from = { n: [] for n in range(N) }
  p = np.multiply(pf_vec(np.multiply(curr_agents,dists)),curr_agents)
  while cont and cur_loop < max_loops:
    heard_agents += curr_agents
    heard_matrix = np.tile(heard_agents, (N,1))
    adj_minus_heard = np.clip(adj - heard_matrix, 0, 1)

    for agent in range(N):
      if curr_agents[agent] == 1: 
        for connection in range(N):
          if adj_minus_heard[agent,connection] == 1:
            heard_from[connection].append(agent)

    rolls = np.multiply(curr_agents, np.random.rand(N))
    successes = (np.multiply(p,curr_agents) > rolls).astype(int)
    believed_agents += successes

    new_agents = np.clip(successes * adj_minus_heard, 0, 1)[0]
    new_agents = np.ravel(new_agents.sum(axis=0))
    # Mask out the believers so only they propagate
    propagation = np.multiply(p, successes) * adj_minus_heard
    propagation = np.ravel(propagation.sum(axis=0))
    new_agents_bel_p = np.multiply(pf_vec(np.multiply(new_agents,dists)),new_agents)
    next_p = np.multiply(new_agents_bel_p,propagation)
    p = p + next_p

    new_p_over_limit = (next_p >= limit).astype(int)
    curr_agents = new_agents
    cont = new_p_over_limit.sum() > 0
    cur_loop += 1
  return {'heard': heard_agents, 'believed': believed_agents, 'heard_from': heard_from}

'''
Update an agent brain dictionary to change the value of agent[attr] to value.

:param agent: The agent's brain.
:param attr: The attribute to update.
:param value: The value to update it to.
'''
def update_agent_belief(agent, attr, value):
  agent[attr.name] = value
  return agent

def topics_in_message(topics, message):
  '''
  Return topics contained in a message by set intersection

  :param topics: a dictionary of topic -> belief proposition
  :param message: a dictionary of belief proposition -> value
  '''
  message_beliefs = set(message.keys())
  topics = { key: set(val) for key,val in topics.items() }
  return [ key for key,val in topics.items() if len(val.intersection(message_beliefs)) > 0 ]
    
"""
AGENT GENERAL FUNCTIONS
"""

def pass_message(speaker, m, receiver):
    receive_message(receiver, m)

"""
INTEGRATION TESTING FUNCTIONS
"""

'''
Read message data over time for given media agent IDs.

:param path: The path to the file to read.
'''
def read_message_over_time_data(path):
  data = json.load(open(path, 'r'))
  converted_messages = {}
  for tick, messages in data.items():
    if tick != 'start' and tick != 'stop':
      converted_messages[int(tick)] = messages
  return converted_messages

'''
UTILITY FUNCTIONS
'''

def generate_all_messaging_patterns(start, stop, resolution, bel, out_path):
  for m_type in INSTITUTION_MESSAGING_TYPES:
    for eco_type in INSTITUTION_ECOSYSTEM_TYPES:
      generate_messaging_patterns(start, stop, m_type, eco_type, resolution, bel, out_path)

def generate_messaging_patterns(start, stop, m_type, eco_type, resolution, bel, out_path):
  '''
  Create JSON files for messaging patterns to be used by instituional agents
  for experiments.
  NOTE: This only includes files that use ONE belief (specified by `bel`) -- this
  should be modified to include more complex belief objects.

  :param start: Integer start timestep value for the simulation
  :param stop: Integer end timestep value for the simulation
  :param m_type: INSTITUTIONAL_MESSAGING_TYPES value for a pattern to use
  :param eco_type: INSTITUTIONAL_ECOSYSTEM_TYPES value for a pattern to use
  :param resolution: Integer belief resolution (must be >= stop)
  :param bel: String belief key to make messages for
  :param out_path: String path to make messaging files in
  '''

  if not os.path.isdir(f'{out_path}/{resolution}'):
    os.mkdir(f'{out_path}/{resolution}')
  if not os.path.isdir(f'{out_path}/{resolution}/{eco_file_names[eco_type]}'):
    os.mkdir(f'{out_path}/{resolution}/{eco_file_names[eco_type]}')

  f = open(f'{out_path}/{resolution}/{eco_file_names[eco_type]}/{messaging_file_names[m_type]}.json', 'w')
  
  pattern_obj = { 'start': start, 'stop': stop }
  if m_type == INSTITUTION_MESSAGING_TYPES.DEFAULT or m_type == INSTITUTION_MESSAGING_TYPES.SPLIT:
    pattern_obj[f'{start}'] = {}
    if eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MAX:
      pattern_obj[f'{start}']['MAX'] = [ { f'{bel}': resolution-1 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MIN:
      pattern_obj[f'{start}']['MIN'] = [ { f'{bel}': 0 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MID:
      pattern_obj[f'{start}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_POLARIZED:
      pattern_obj[f'{start}']['MAX'] = [ { f'{bel}': resolution-1 } ]
      pattern_obj[f'{start}']['MIN'] = [ { f'{bel}': 0 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_MID:
      pattern_obj[f'{start}']['LOWER'] = [ { f'{bel}': math.floor(resolution/4) } ]
      pattern_obj[f'{start}']['UPPER'] = [ { f'{bel}': 3*math.floor(resolution/4) } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_POLARIZED:
      pattern_obj[f'{start}']['MIN'] = [ { f'{bel}': 0 } ]
      pattern_obj[f'{start}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
      pattern_obj[f'{start}']['MAX'] = [ { f'{bel}': resolution-1 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_MID:
      pattern_obj[f'{start}']['MIN'] = [ { f'{bel}': math.floor(resolution/6) } ]
      pattern_obj[f'{start}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
      pattern_obj[f'{start}']['MAX'] = [ { f'{bel}': 5*math.floor(resolution/6) } ]
  if m_type == INSTITUTION_MESSAGING_TYPES.SPLIT:
    pattern_obj[f'{round(stop/2)}'] = {}
    if eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MAX:
      pattern_obj[f'{round(stop/2)}']['MAX'] = [ { f'{bel}': 0 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MIN:
      pattern_obj[f'{round(stop/2)}']['MIN'] = [ { f'{bel}': resolution-1 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MID:
      pattern_obj[f'{round(stop/2)}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_POLARIZED:
      pattern_obj[f'{round(stop/2)}']['MIN'] = [ { f'{bel}': resolution-1 } ]
      pattern_obj[f'{round(stop/2)}']['MAX'] = [ { f'{bel}': 0 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_MID:
      pattern_obj[f'{round(stop/2)}']['UPPER'] = [ { f'{bel}': math.floor(resolution/4) } ]
      pattern_obj[f'{round(stop/2)}']['LOWER'] = [ { f'{bel}': 3*math.floor(resolution/4) } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_POLARIZED:
      pattern_obj[f'{round(stop/2)}']['MAX'] = [ { f'{bel}': 0 } ]
      pattern_obj[f'{round(stop/2)}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
      pattern_obj[f'{round(stop/2)}']['MIN'] = [ { f'{bel}': resolution-1 } ]
    elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_MID:
      pattern_obj[f'{round(stop/2)}']['MAX'] = [ { f'{bel}': math.floor(resolution/6) } ]
      pattern_obj[f'{round(stop/2)}']['MID'] = [ { f'{bel}': math.floor(resolution/2) } ]
      pattern_obj[f'{round(stop/2)}']['MIN'] = [ { f'{bel}': 5*math.floor(resolution/6) } ]
  elif m_type == INSTITUTION_MESSAGING_TYPES.GRADUAL:
    buckets = 6
    step = math.floor(60 / buckets)
    for t in range(start, 61, step):
      pattern_obj[f'{t}'] = {}
      if eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MAX:
        pattern_obj[f'{t}']['MAX'] = [ { f'{bel}': (resolution-1) - math.floor(((resolution-1) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MIN:
        pattern_obj[f'{t}']['MIN'] = [ { f'{bel}': math.floor(((resolution-1) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MID:
        pattern_obj[f'{t}']['MID'] = [ { f'{bel}': 3*math.floor((resolution-1)/4) - math.floor((2*((resolution-1)/4) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_POLARIZED:
        pattern_obj[f'{t}']['MIN'] = [ { f'{bel}': math.floor(((resolution-1) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['MAX'] = [ { f'{bel}': (resolution-1) - math.floor(((resolution-1) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_MID:
        pattern_obj[f'{t}']['UPPER'] = [ { f'{bel}': 3*math.floor((resolution-1)/4) - math.floor((2*((resolution-1)/4) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['LOWER'] = [ { f'{bel}': math.floor((resolution-1)/4) + math.floor((2*((resolution-1)/4) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_POLARIZED:
        pattern_obj[f'{t}']['MIN'] = [ { f'{bel}': math.floor(((resolution-1) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['MAX'] = [ { f'{bel}': (resolution-1) - math.floor(((resolution-1) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['MID'] = [ { f'{bel}': 3*math.floor((resolution-1)/4) - math.floor((2*((resolution-1)/4) / buckets) * (t / step)) } ]
      elif eco_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_MID:
        pattern_obj[f'{t}']['UPPER'] = [ { f'{bel}': 5*math.floor((resolution-1)/6) - math.floor((4*((resolution-1)/6) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['LOWER'] = [ { f'{bel}': math.floor((resolution-1)/6) + math.floor((4*((resolution-1)/6) / buckets) * (t / step)) } ]
        pattern_obj[f'{t}']['MID'] = [ { f'{bel}': 3*math.floor((resolution-1)/4) - math.floor((2*((resolution-1)/4) / buckets) * (t / step)) } ]
  f.write(json.dumps(pattern_obj))
  f.close()
