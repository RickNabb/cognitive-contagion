import numpy as np
import math
import json
from data import *
from random import *
from copy import deepcopy
import sys

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
:param threshold: How many tokens an agent must stack up before they switch
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
    threshold=5,
    alpha=1.0
    ):
    agent = {
        'ID': ID,
        'alpha': alpha,
        'beta': beta,
        'malleable': list(map(lambda attr: attr.name, malleable_attributes)),
        'prior': list(map(lambda attr: attr.name, prior_attributes)),
        'tokens': {},
        'cont_tokens': {},
        'update_threshold': threshold
    }
    # Initialize malleable beliefs w/ tokens for belief updates
    for i in range(0, len(malleable_attributes)):
      attr = malleable_attributes[i]
      agent[attr.name] = malleable_initial[i]

    # Initialize prior beliefs that cannot change - no tokens
    for i in range(0, len(prior_attributes)):
        attr = prior_attributes[i]
        agent[attr.name] = prior_initial[i]

    if brain_type == 'discrete':
      for i in range(0, len(malleable_attributes)):
        attr = malleable_attributes[i]
        agent[attr.name] = malleable_initial[i]
        # Initialize belief change counters
        agent["tokens"][attr.name] = {}
        for val in attrs_as_array(attr):
          agent["tokens"][attr.name][val] = 0
    elif brain_type == 'continuous':
      for i in range(0, len(malleable_attributes)):
        attr = malleable_attributes[i]
        agent['cont_tokens'][attr.name] = malleable_initial[i]

    return agent

"""
Update an agent's belief tokens. This is the modeling of an agent starting
to change their beliefs. If the token count reaches a certain threshold,
the agent will change their appropriate belief to match the value that breached
the threshold.

:param agent: The agent to update belief tokens on.
:param attrs: Attributes to update tokens for.
"""
def update_agent_tokens(agent, attrs):
    for attr in attrs:
        token = agent['tokens'][attr][attrs[attr]]

        # If the update threshold is reached, update the agent belief
        if token+1 >= agent['update_threshold']:
            agent[attr] = attrs[attr]
            #print('Updating agent belief on ' + str(attr.name) + ' to ' + str(attrs[attr]))

            # Clear tokens
            for tokens in agent['tokens'][attr]:
                agent['tokens'][attr][tokens] = 0
        else:
            agent['tokens'][attr][attrs[attr]] += 1

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
    agent_beliefs = agent_beliefs_from_message(agent, message)
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
    # agent_beliefs = agent_beliefs_from_message(agent, message)
    # # Update agent belief tokens if the message is within updating threshold
    # if spread_type == "cognitive":
    #   #print('updating belief tokens')
    #   update_attrs = {}
    #   for attr in message:
    #       if message[attr] != agent_beliefs[attr] and attr in agent['tokens']:
    #           update_attrs[attr] = message[attr]
    #   update_agent_tokens(agent, update_attrs)
    # # Just have the agent believe whatever it is
    # elif spread_type == "simple" or spread_type == "complex":
    for m in message:
      if m in agent['malleable']: agent[m] = message[m]
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
  m_arr = message_as_array(message)
  a_arr = message_as_array(agent_beliefs)
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
    agent_beliefs[attr] = agent[attr]
  return agent_beliefs

def agent_belief_vec_from_message(agent, message):
    agent_beliefs = []
    for attr in message:
        agent_beliefs.append(agent[attr])
    return np.array(agent_beliefs)
  
'''
Update an agent brain dictionary to change the value of agent[attr] to value.

:param agent: The agent's brain.
:param attr: The attribute to update.
:param value: The value to update it to.
'''
def update_agent_belief(agent, attr, value):
  agent[attr.name] = value
  return agent
    
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
    f = open(path, 'r')
    raw = f.read()
    data = json.loads(raw)
    messages = {}
    last_valid_t = -1
    for t in range(data['start'], data['stop']):
        if str(t) in data:
            last_valid_t = str(t)
        if last_valid_t != -1:
          messages[t] = data[last_valid_t]
    f.close()
    return messages