'''
A module to safely report graph structures and functions
from Python to NetLogo.

Author: Nick Rabb (nick.rabb2@gmail.com)
'''

from enum import Enum
import networkx as nx
import numpy as np
import math
from networkx import community
from messaging import dist_to_agent_brain, agent_trust_in_other_belief_func
from random import random
from functools import reduce
from enums import INSTITUTION_ECOSYSTEM_TYPES, eco_file_names
from utils import curr_sigmoid_p, sigmoid_contagion_p, normal_dist, normal_dist_multiple
import json

"""
BELIEF ATTRIBUTES
"""

NUM_BELIEF_BUCKETS = 32

discrete = range(NUM_BELIEF_BUCKETS)

# class TestDiscrete7(Enum):
#   STRONG_DISBELIEF=12
#   DISBELIEF=1
#   MOD_DISBELIEF=2
#   UNCERTAIN=3
#   MOD_BELIEF=4
#   BELIEF=5
#   STRONG_BELIEF=6

"""
RELEVANT EMPIRICAL DATA
"""

class Attributes(Enum):
  A = discrete

def attrs_as_array(attr):
  # return map(lambda a: a.value, list(attr.value))
  return attr.value

AttributeValues = {
  Attributes.A.name: {
    "vals": normal_dist,
    "depends_on": None
  }
}

'''
This is a simple data structure to put groups on a 1-dimensional
axis where their distance from each other can be used to calculate, for
example, similarity and difference.
'''
GroupEmbeddings = {
  'DEM': 0,
  'MOD': 1,
  'REP': 2
}

def group_to_embedding(group):
  return GroupEmbeddings[group]

def groups_to_embedding(groups):
  return np.array([ group_to_embedding(group) for group in groups ]).sum()

# AttributeDistributions = {
#   Attributes.A.name: {
#     "dist": ADist,
#     "depends_on": None
#   }
# }

# AttributeMAGThetas = {
#   Attributes.A.name: {
#     'default': AMAGDefaultTheta,
#     'homophilic': AMAGHomophilicTheta,
#     'heterophilic': AMAGHeterophilicTheta
#   }   
# }

# Attribute A distribution values
'''
Calculate a row of homophily values for belief value `row` based on the 
equation: 1 / (1 + d + s|(row-i)^pow|)

Higher values of d, s, and p yield higher homophily values -- smaller scalar
multipliers for p based on distance make the probability of connecting lower
as distance increases.

:param row: The belief value to calculate homophily values for -- aka the
row of a homophily matrix if rows are belief values.
:param l: The length of the row (the belief resolution)
:param p: A value to use as the power term.
:param s: A value to use as the scalar term.
:param d: A value to use as the displacement term.
'''
HomophilicThetaRow = lambda row, l, p, s, d: [ 1/(1 + d + s * abs(pow(row - i, p))) for i in range(0, l) ]
SquareHomophilicThetaRow = lambda row, l: HomophilicThetaRow(row, l, 2, 5, 0.25)
LinearHomophilicThetaRow = lambda row, l: HomophilicThetaRow(row, l, 1, 5, 0.25)

HeterophilicThetaRow = lambda row, l, p, s, d: [ (abs(pow(i-row,p)))/(s*pow(max(abs(l-row-1), row)+d,p)) for i in range(0, l) ]
SquareHeterophilicThetaRow = lambda row, l: HeterophilicThetaRow(row, l, 2, 2, 0)
LinearHeterophilicThetaRow = lambda row, l: HeterophilicThetaRow(row, l, 1, 2, 0)

# AVals = np.ones(NUM_BELIEF_BUCKETS) #[1, 1, 1, 1, 1, 1, 1]
# ADist = create_discrete_dist_sm(AVals)
AMAGDefaultTheta = lambda resolution: np.ones((resolution,resolution)) * 0.05
AMAGHomophilicTheta = lambda resolution: np.matrix([ HomophilicThetaRow(i, resolution, 2, 50, 5) for i in range(0, resolution) ])
AMAGHeterophilicTheta = lambda resolution: np.matrix([ HeterophilicThetaRow(i, resolution, 2, 5, 1) for i in range(0, resolution) ])

'''
Return a NetLogo-safe Erdos-Renyi graph from the NetworkX package.

:param n: The number of nodes for the graph.
:param p: The probability of two random nodes connecting.
'''
def ER_graph_bidirected(n, p):
  G = nx.erdos_renyi_graph(n, p)
  return nlogo_safe_nodes_edges(bidirected_graph(G))

'''
Return a NetLogo-safe Erdos-Renyi graph from the NetworkX package.

:param n: The number of nodes for the graph.
:param p: The probability of two random nodes connecting.
'''
def ER_graph(n, p):
  G = nx.erdos_renyi_graph(n, p)
  return nlogo_safe_nodes_edges(G)

'''
Return a Netlogo-safe Watts-Strogatz graph from the NetworkX package.

:param n: The number of nodes.
:param k: The number of initial neighbors.
:param p: The probability of an edge rewiring.
'''
def WS_graph_bidirected(n, k, p):
  G = nx.watts_strogatz_graph(n, k, p)
  return nlogo_safe_nodes_edges(bidirected_graph(G))

'''
Return a Netlogo-safe Watts-Strogatz graph from the NetworkX package.

:param n: The number of nodes.
:param k: The number of initial neighbors.
:param p: The probability of an edge rewiring.
'''
def WS_graph(n, k, p):
  G = nx.watts_strogatz_graph(n, k, p)
  return nlogo_safe_nodes_edges(G)

'''
Return a Netlogo-safe Barabasi-Albert graph from the NetworkX package.

:param n: The number of nodes.
:param m: The number of edges to connect with when a node is added.
'''
def BA_graph_bidirected(n, m):
  G = nx.barabasi_albert_graph(n, m)
  return nlogo_safe_nodes_edges(bidirected_graph(G))

'''
Return a Netlogo-safe Barabasi-Albert graph from the NetworkX package.

:param n: The number of nodes.
:param m: The number of edges to connect with when a node is added.
'''
def BA_graph(n, m):
  G = nx.barabasi_albert_graph(n, m)
  return nlogo_safe_nodes_edges(G)

'''
Return a Netlogo-safe Barabasi-Albert graph from the NetworkX package that
has extra parameters to govern the degree of homophily present in the graph.

:param n: The number of nodes.
:param m: The number of edges to connect with when a node is added.
:param resolution: Belief resolution
:param bel_homophily: Homophily value from 0-1 for homophily based on
belief similarity.
:param group_homophily: Homophily value from 0-1 for homophily based on
group similarity.
:param brains: A list of agent brain structures containing their beliefs
:param groups: A list of which groups agents are members of
'''
def BA_graph_homophilic(n, m, resolution, bel_homophily, group_homophily, brains, groups):
  # Params: row, len, pow, scalar, diff
  # Higher values yield more homophily
  HOMOPHILY_SCALAR = 10
  AMAGHomophilicThetaBel = lambda resolution: np.matrix([ HomophilicThetaRow(i, resolution, HOMOPHILY_SCALAR*bel_homophily, HOMOPHILY_SCALAR*bel_homophily, 0) for i in range(0, resolution) ])
  homophily_group = lambda groups1, groups2, p, s, d: 1/(1 + d + s * abs(pow(groups_to_embedding(groups1) - groups_to_embedding(groups2), p))) 
  homophily_bel = AMAGHomophilicThetaBel(resolution)
  G = nx.complete_graph(n=m)
  i = 0
  # print(f'started with G: {G}')
  while len(G.nodes) < n:
    G.add_node(m+i)
    # print(f'added node{m+i}: {G.nodes}')
    total_degree = 2 * len(G.edges)
    # Add m edges
    edges_added = 0
    for k in range(m):
      for node in G.nodes:
        if node == m+i: continue

        node_degree = G.degree(node)
        p = node_degree / total_degree
        for j in range(len(brains[node])):
          # print(f'multiplier between {attrs[m+i][j]} and {attrs[node][j]} is {AMAGHomophilicTheta(resolution)[(attrs[m+i][j],attrs[node][j])]}')
          p *= homophily_bel[(brains[m+i][j],brains[node][j])]
        p *= homophily_group(groups[m+i],groups[node], HOMOPHILY_SCALAR*group_homophily, HOMOPHILY_SCALAR*group_homophily, 0)
        rand = random()
        if rand <= p:
          G.add_edge(m+i, node)
          edges_added += 1
    if edges_added == 0:
      G.remove_node(m+i)
      i -= 1
    i += 1
  return nlogo_safe_nodes_edges(G)

'''
Create a MAG graph for N nodes, given L attributes, and a style of connection
if there is no specified connection affinity matrix.

:param n: The number of nodes.
:param attrs: A list of attributes to gather Theta affinity matrices for in order
to properly calculate the product of all attribute affinities for the matrix.
:param style: A string denoting how to connect the attributes - default, homophilic, or heterophilic.
'''
# def MAG_graph_bidirected(n, attrs, style, resolution):
#   (p_edge, L) = mag.attr_mag(n, attrs, style, resolution)
#   # print(p_edge)
#   # print(L)
#   G = nx.Graph()
#   G.add_nodes_from(range(0, len(p_edge[0])))
#   for i in range(0,len(p_edge)):
#     for j in range(0,len(p_edge)):
#       rand = random()
#       if (rand <= p_edge[(i,j)]):
#         # if (abs(L[i][0]-L[j][0]) >= 2):
#           # print(f'Chance to connect {L[i]} and {L[j]}: {p_edge[(i,j)]}')
#           # print(f'Rolled {rand}: {rand <= p_edge[(i,j)]}')
#         G.add_edge(i, j)
#   # print(f'Num edges: {len(G.edges)}')
#   nlogo_G = nlogo_safe_nodes_edges(bidirected_graph(G))
#   nlogo_G.update({'L': L})
#   return nlogo_G

'''
Create a MAG graph for N nodes, given L attributes, and a style of connection
if there is no specified connection affinity matrix.

:param n: The number of nodes.
:param attrs: A list of attributes to gather Theta affinity matrices for in order
to properly calculate the product of all attribute affinities for the matrix.
:param style: A string denoting how to connect the attributes - default, homophilic, or heterophilic.
'''
# def MAG_graph(n, attrs, style, resolution):
#   (p_edge, L) = mag.attr_mag(n, attrs, style, resolution)
#   # print(p_edge)
#   # print(L)
#   G = nx.Graph()
#   G.add_nodes_from(range(0, len(p_edge[0])))
#   for i in range(0,len(p_edge)):
#     for j in range(0,len(p_edge)):
#       rand = random()
#       if (rand <= p_edge[(i,j)]):
#         # if (abs(L[i][0]-L[j][0]) >= 2):
#           # print(f'Chance to connect {L[i]} and {L[j]}: {p_edge[(i,j)]}')
#           # print(f'Rolled {rand}: {rand <= p_edge[(i,j)]}')
#         G.add_edge(i, j)
#   # print(f'Num edges: {len(G.edges)}')
#   nlogo_G = nlogo_safe_nodes_edges(G)
#   nlogo_G.update({'L': L})
#   return nlogo_G

# def kronecker_graph(seed, k):
#   '''
#   Make a kronecker graph from a given seed to a power.

#   :param seed: An np array to Kronecker power.
#   :param k: An integer to raise the graph to the Kronecker power of.
#   '''
#   G_array = kronecker_pow(seed, k)
#   G = nx.Graph()
#   G.add_nodes_from(range(0, G_array.shape[0]))
#   for i in range(G_array.shape[0]):
#     row = G_array[i]
#     for j in range(G_array.shape[1]):
#       if i == j:
#         continue
#       p = row[j]
#       if random() < p:
#         G.add_edge(i,j)
#   largest_connected_component = max(nx.connected_components(G), key=len)
#   G.remove_nodes_from(G.nodes - largest_connected_component)
#   # return G
#   return nlogo_safe_nodes_edges(G)

# def kronecker_graph_bidirected(seed, k):
#   '''
#   Make a kronecker graph from a given seed to a power.

#   :param seed: An np array to Kronecker power.
#   :param k: An integer to raise the graph to the Kronecker power of.
#   '''
#   G_array = kronecker_pow(seed, k)
#   G = nx.Graph()
#   G.add_nodes_from(range(0, G_array.shape[0]))
#   for i in range(G_array.shape[0]):
#     row = G_array[i]
#     for j in range(G_array.shape[1]):
#       if i == j:
#         continue
#       p = row[j]
#       if random() < p:
#         G.add_edge(i,j)
#   largest_connected_component = max(nx.connected_components(G), key=len)
#   G.remove_nodes_from(G.nodes - largest_connected_component)
#   return nlogo_safe_nodes_edges(bidirected_graph(G))

def bidirected_graph(G):
  '''
  Convert an undirected graph to a directed graph where each
  undirected edge becomes two directed edges.

  :param G: An undirected networkx graph.
  '''
  bidirected_G = nx.DiGraph()
  for edge in G.edges:
    bidirected_G.add_edge(edge[0], edge[1])
    bidirected_G.add_edge(edge[1], edge[0])
  return bidirected_G

'''
Return NetLogo-safe graph structures.

:param G: The networkx graph to convert.
'''
def nlogo_safe_nodes_edges(G):
  nodes = list(G.nodes)
  edges = [ [e[0], e[1]] for e in G.edges ]
  return { 'nodes': nodes, 'edges': edges }

'''
Convert a graph from NetLogo to a networkx graph.

:param citizens: A list of citizen agents' brain objects.
:param friend_links: A list of citizen agents' friend links
'''
def nlogo_graph_to_nx(citizens, friend_links):
  G = nx.Graph()
  for cit in citizens:
    cit_id = int(cit['ID'])
    G.add_node(cit_id)
    if 'malleable' in cit and 'prior' in cit:
      for attr in cit['malleable']:
        G.nodes[cit_id][attr] = cit[attr]
      for attr in cit['prior']:
        G.nodes[cit_id][attr] = cit[attr]
    if 'groups' in cit:
      G.nodes[cit_id]['groups'] = cit['groups']
  for link in friend_links:
    link_split = link.split(' ')
    end1 = link_split[1]
    end2 = link_split[2].replace(')','')
    G.add_edge(int(end1), int(end2))
  return G

def nlogo_graph_to_nx_with_media(citizens, friend_links, media, subscribers):
  G = nx.Graph()
  agents = citizens + media
  links = friend_links + subscribers
  for agent in agents:
    cit_id = int(agent['ID'])
    G.add_node(cit_id)
    for attr in agent['malleable']:
      G.nodes[cit_id][attr] = agent[attr]
    for attr in agent['prior']:
      G.nodes[cit_id][attr] = agent[attr]
    if 'groups' in agent:
      G.nodes[cit_id]['groups'] = agent['groups']
  for link in links:
    link_split = link.split(' ')
    end1 = link_split[1]
    end2 = link_split[2].replace(')','')
    G.add_edge(int(end1), int(end2))
  return G

def nlogo_saved_graph_to_nx(citizens, social_links, media, subscribers):
  G = nx.Graph()
  links = social_links + subscribers
  for cit in citizens:
    cit_id = int(cit[0])
    cit_belief = int(cit[1])
    cit_group = cit[2]
    G.add_node(cit_id)
    # TODO: Change this eventually -- this is also hardcoded into the
    # graph saving so that only proposition 'A' is saved
    G.nodes[cit_id]['A'] = cit_belief
    G.nodes[cit_id]['groups'] = cit_group
  for m in media:
    media_id = int(m[0])
    media_name = m[1]
    media_belief = int(m[2])
    G.add_node(media_id)
    G.nodes[media_id]['name'] = media_name
    G.nodes[media_id]['A'] = media_belief
  for link in links:
    G.add_edge(int(link[0]), int(link[1]))
  return G

def citizen_media_connections_by_zeta(citizen_beliefs, citizen_memories, zeta, citizen_memory_len, num_media, topics, trust_fn):
  '''
  Return a matrix of media subscribers based off of citizen trust in media
  based on their memory of media messaging.

  :param citizen_beliefs: An array of citizen brains but only their beliefs
  :param citizen_memories: An array of citizen memories for media
  :param zeta: The trust threshold for citizen-media connection
  :param citizen_memory_len: The length of memory citizens have
  :param num_media: The number of media in the graph
  :param topics: A dictionary of topics and the beliefs they contain
  :param trust_fn: A function to use for the trust calculation between
  memories and citizen beliefs.
  '''
  num_citizens = len(citizen_beliefs)
  trust_matrix = np.zeros((num_citizens,num_media))

  for topic,topic_beliefs in topics.items():
    # topic_trust = np.array(([[ agent_trust_in_other_belief_func(citizen_memories[c][f'(media ${len(citizen_beliefs)+i})'],citizen_beliefs[c],topic_beliefs,trust_fn) if f'(media ${len(citizen_beliefs)+i})' in citizen_memories[c] else -1 for c in range(len(citizen_beliefs)) ] for i in range(num_media)]))
    topic_trust = np.array(([[ agent_trust_in_other_belief_func(citizen_memories[c][f'(media {num_citizens+i})'],citizen_beliefs[c],topic_beliefs,trust_fn) if f'(media {num_citizens+i})' in citizen_memories[c] else -1 for i in range(num_media) ] for c in range(num_citizens)]))
    trust_matrix += topic_trust
  return (trust_matrix / len(topics.keys()) >= zeta).astype(int)

def citizen_citizen_connections_by_zeta(citizen_beliefs, citizen_memories, zeta, topics, trust_fn):
  '''
  Return a matrix of citizen connections based off of citizen trust in citizens
  based on their memory of media messaging.

  :param citizen_beliefs: An array of citizen brains but only their beliefs
  :param citizen_memories: An array of citizen memories for media
  :param zeta: The trust threshold for citizen-media connection
  :param topics: A dictionary of topics and the beliefs they contain
  :param trust_fn: A function to use for the trust calculation between
  memories and citizen beliefs.
  '''
  num_citizens = len(citizen_beliefs)
  trust_matrix = np.zeros((num_citizens,num_citizens))

  for topic,topic_beliefs in topics.items():
    topic_trust = np.array(([[ agent_trust_in_other_belief_func(citizen_memories[c][f'(citizen {i})'],citizen_beliefs[c],topic_beliefs,trust_fn) if f'(citizen {i})' in citizen_memories[c] else -1 for i in range(num_citizens) ] for c in range(num_citizens)]))
    trust_matrix += topic_trust
  return (trust_matrix / len(topics.keys()) >= zeta).astype(int)

def citizen_media_initial_connections_by_zeta(citizen_beliefs, media_beliefs, zeta, citizen_memory, topics, trust_fn):
  '''
  Return a matrix of media subscribers based off of citizen trust in media
  beliefs -- but only for an initial state, as this generates a citizen
  memory to be used for the trust function.

  :param citizen_beliefs: An array of citizen brains but only their beliefs
  :param media_beliefs: An array of media brains but only their beliefs
  :param zeta: The trust threshold for citizen-media connection
  :param citizen_memory: The length of memory citizens have
  :param topics: A dictionary of topics and the beliefs they contain
  :param trust_fn: A function to use for the trust calculation between
  memories and citizen beliefs.
  '''
  citizen_memory_for_media = lambda media_belief: { bel: [media_belief[bel] for i in range(citizen_memory)] for bel in topic_beliefs }
  trust_matrix = np.zeros((len(media_beliefs),len(citizen_beliefs)))

  for topic,topic_beliefs in topics.items():
    topic_trust = np.array(([[ agent_trust_in_other_belief_func(citizen_memory_for_media(media_belief),citizen_belief,topic_beliefs,trust_fn) for citizen_belief in citizen_beliefs ] for media_belief in media_beliefs]))
    trust_matrix += topic_trust
  return (trust_matrix / len(topics.keys()) >= zeta).astype(int)

def reconstruct_spread_path(heard_from, end_citizen):
  G = nx.DiGraph()
  G.add_node(end_citizen)
  G = reconstruct_spread_path_rec(heard_from, end_citizen, G)
  return nlogo_safe_nodes_edges(G)

def reconstruct_spread_path_rec(heard_from, cur_citizen, G):
  if not heard_from[cur_citizen]:
    return G
  
  for cit in heard_from[cur_citizen]:
    G.add_node(cit)
    G.add_edge(cit, cur_citizen)
    G = reconstruct_spread_path_rec(heard_from, cit, G)
  return G

'''
ANALYSIS FUNCTIONS
'''

def influencer_paths(G, subscribers, target):
  target_id = int(target.split(' ')[1].replace(')', ''))
  return { subscriber.split(' ')[2].replace(')',''): nx.all_simple_paths(G, subscriber.split(' ')[2].replace(')',''), target, cutoff=5) for subscriber in subscribers }

'''
Get all paths from an influencer to a target node who only contain nodes within
a certain threshold distance from a given message.

:param citizens: A list of citizen agents' brain objects.
:param friend_links: A list of citizen agents' friend links
:param subscribers: A list of subscribers of the influencer.
:param target: The target node to find paths to.
:param message: The message to use for agent distance.
:param threshold: A value that the distance between message and agent cannot
exceed in valid paths.
'''
def influencer_paths_within_distance(citizens, friend_links, subscribers, target, message, threshold):
  G = nlogo_graph_to_nx(citizens, friend_links)

  # Assign edge weights of the message distance to the first agent in the link
  for e in G.edges:
    G[e[0]][e[1]]['weight'] = dist_to_agent_brain(G.nodes[e[0]], message)

  target_id = int(target.split(' ')[1].replace(')', ''))
  paths = { int(subscriber.split(' ')[2].replace(')','')): nx.dijkstra_path(G, int(subscriber.split(' ')[2].replace(')','')), target_id) for subscriber in subscribers }

  distance_paths = {}
  threshold_paths = {}
  for subscriber in paths.keys():
    dist_path = [ dist_to_agent_brain(G.nodes[v], message) for v in paths[subscriber] ]
    dist_path = dist_path[:-1]
    distance_paths[subscriber] = dist_path
    if sum((np.array(dist_path)-threshold) > 0) == 0:
      threshold_paths[subscriber] = dist_path
      # threshold_paths[subscriber] = paths[subscriber]
  return threshold_paths

'''
MEASURE FUNCTIONS
'''

def simple_power_fn(p):
  '''
  A first-degree power function for simple contagion: simply returning
  the probability p of contagion.

  :param p: The probability of contagion.
  '''
  return lambda u, v: p

def complex_power_fn(G, attr, threshold):
  '''
  A first-degree power function for complex contagion: adding up the
  number of nodes in an agent's neighborhood who their belief would
  sway.

  :param G: The graph.
  :param attr: The belief attribute in an agent's brain to compare to its
  neighbors.
  :param threshold: The ratio needed to induce complex contagion.
  '''
  complex_fn = lambda bel, neighbors: 1 if (len(list(filter(lambda neighbor: G.nodes[neighbor][attr] == bel, neighbors))) / len(neighbors)) >= threshold else 0
  return lambda u, v: complex_fn(G.nodes[u][attr], list(nx.neighbors(G, v)))

def cognitive_power_fn(G, attr, fn, scalar, ex, trans):
  '''
  A first-degree power function for cognitive contagion: adding
  up the probabilities of contagion given different contagion
  functions between u and v's beliefs in attr.

  :param G: The graph.
  :param attr: The belief attribute in an agent's brain to compare to its
  neighbors.
  :param fn: The string cognitive contagion function.
  :param scalar: A scalar multiplier for the cognitive function.
  :param ex: An exponent on the cognitive function.
  :param trans: A translation for the cognitive function.
  '''
  bel_fn = lambda u, v: -1
  if 'sigmoid' in fn:
    bel_fn = lambda u, v: (1 / (1 + math.exp(ex * (abs(G.nodes[u][attr] - G.nodes[v][attr]) - trans))))
  elif 'linear' in fn:
    bel_fn = lambda u, v: (1 / (trans + (scalar * abs(G.nodes[u][attr] - G.nodes[v][attr])) ** ex))
  elif 'threshold' in fn:
    bel_fn = lambda u, v: 1 if abs(G.nodes[u][attr]-G.nodes[v][attr]) <= trans else 0
  return bel_fn

def first_degree_power_simple(G, u, p):
  return first_degree_power(G, u, simple_power_fn(p))

def first_degree_power_dist_simple(G, p):
  return first_degree_power_distribution(G, simple_power_fn(p))

def first_degree_power_complex(G, u, attr, threshold):
  return first_degree_power(G, u, complex_power_fn(G, attr, threshold))

def first_degree_power_dist_complex(G, attr, threshold):
  return first_degree_power_distribution(G, complex_power_fn(G, attr, threshold))

def first_degree_power_cognitive(G, u, attr, fn, scalar, ex, trans):
  return first_degree_power(G, u, cognitive_power_fn(G, attr, fn, scalar, ex, trans))

def first_degree_power_dist_cognitive(G, attr, fn, scalar, ex, trans):
  return first_degree_power_distribution(G, cognitive_power_fn(G, attr, fn, scalar, ex, trans))

def first_degree_power(G, u, bel_fn):
  '''
  Calculate the power of an agent u in graph G according to belief
  function bel_fn on its first order neighbors. Power here is the
  sum of the belief function values over the list of neighbors.

  :param G: The graph.
  :param u: The agent to calculate power for.
  :param bel_fn: A function determining belief probability between
  agents.
  '''
  neighbors = nx.neighbors(G, u)
  vals = np.array(list(map(lambda neighbor: bel_fn(u, neighbor), neighbors)))
  return vals.sum()

def first_degree_power_distribution(G, bel_fn):
  '''
  Generate a distribution of all agents' first-degree power
  in the graph.
  :param G: The graph.
  :param bel_fn: The function to use for belief and power calculations.
  '''
  return [ { u: first_degree_power(G, u, bel_fn) } for u in G.nodes ]

# def plot_graph_communities(G, level):
#   '''

#   '''
#   dendrogram = community.generate_dendrogram(G)
#   partition = community.partition_at_level(dendrogram, level)
#   pos = nx.spring_layout(G)
#   cmap = cm.get_cmap('viridis', max(partition.values()) + 1)
#   nx.draw_networkx_nodes(G, pos, partition.keys(), node_size=40, cmap=cmap, node_color=list(partition.values()))
#   nx.draw_networkx_edges(G, pos, alpha=0.5)
#   plt.show()

def node_homophily(G, node):
  '''
  Get the homophily value for a single node in G.

  :param G: The graph.
  :param node: The integer node index to get.
  '''
  attrs = np.array([ list(G.nodes[node].values()) for node in G.nodes ])
  norm_vector = np.array([ np.linalg.norm(attr - attrs[node]) for attr in attrs ])
  adj = nx.adj_matrix(G)
  # Note: adj[node] * norm_vector sums the values already
  return (adj[node] * norm_vector) / adj[node].sum()

def graph_homophily(G):
  '''
  Takes a measure of homophily in the graph based on first-level neighbor
  distance on a given node attribute. Details can be found in Rabb et al. 2022
  Eq (9).

  :param G: The networkx graph to take the measure on.
  '''
  distances = []
  attrs = np.array([ list(G.nodes[node].values()) for node in G.nodes ])
  adj = []
  for i in G.nodes:
    adj.append([ (i,j) in G.edges for j in G.nodes ])
  adj = np.matrix(adj).astype(int)
  # adj = nx.adjacency_matrix(G)
  for node in G.nodes:
    norm_vector = np.array([ np.linalg.norm(attr - attrs[node]) for attr in attrs ])
    # Note: adj[node] * norm_vector sums the values already
    # Note 2: The max(sum(), 1) is to protect against division by 0
    # in the case of disconnected nodes
    distances.append((adj[node] * np.matrix(norm_vector).transpose())[0] / max(adj[node].sum(),1))

  return (np.array(distances).mean(), np.array(distances).var())

def graph_fragmentation(cit_beliefs, media_beliefs, subscribers):
  '''
  Takes a measure of fragmentation in the graph based on first-level neighbor
  distance, only for media neighbors. Details can be found in Rabb et al. 2023

  :param cit_beliefs: A list of citizen belief dictionaries.
  :param media_beliefs: A list of media belief dictionaries.
  :param subscribers: A list of subscribers lists for each media agent.
  '''
  distances = []
  N = len(cit_beliefs)
  cit_belief_vectors = np.array([ list(bel.values()) for bel in cit_beliefs ])
  media_belief_vectors = np.array([ list(bel.values()) for bel in media_beliefs ])
  adj = []
  for media in range(len(media_beliefs)):
    adj.append([ agent in subscribers[media] for agent in range(N) ])
  # Transpose so we can loop through citizens
  adj = np.matrix(adj).astype(int).transpose()

  # for media in range(len(media_beliefs)):
  for citizen in range(N):
    norm_vector = np.array([ np.linalg.norm(media_belief - cit_belief_vectors[citizen]) for media_belief in media_belief_vectors ])
    # print(norm_vector)
    # print(adj[citizen])
    # norm_vector = np.array([ np.linalg.norm(citizen_belief - media_belief_vectors[media]) for citizen_belief in cit_belief_vectors ])

    # Note: adj[node] * norm_vector sums the values already
    # Note 2: The max(sum(), 1) is to protect against division by 0
    # in the case of disconnected nodes
    distances.append((norm_vector * adj[citizen].transpose())[0,0] / max(adj[citizen].sum(),1))
    # distances.append((norm_vector * adj[media].transpose())[0,0] / max(adj[media].sum(),1))

  # return distances
  return (np.array(distances).mean(), np.array(distances).var())

def graph_polarization(G, node_attr, max_attr_value):
  '''
  Take a measure of global polarization across the graph from a measure
  motivated in Musco et al., 2018. Note that their belief model uses
  beliefs in [0,1] so we convert node attributes to the [0,1] scale
  for comparability purposes.

  :param G: The networkx graph to take the measure on.
  :param node_attr: The node attribute (string) to take values for. This
  attribute must be a number.
  :param max_attr_value: The maximum that a belief value could be (belief
  resolution - 1).
  '''
  # This is done to keep the value in line w/ Musco et al. 2018's
  # scale of belief from [0,1]
  attrs = np.array([ G.nodes[node][node_attr] / max_attr_value for node in G.nodes ])
  mean_centered_attrs = attrs - attrs.sum()/len(attrs)
  return mean_centered_attrs.dot(np.transpose(mean_centered_attrs))

def nlogo_graph_polarization(citizens, friend_links, node_attr, max_attr_val):
  G = nlogo_graph_to_nx(citizens, friend_links)
  return graph_polarization(G, node_attr, max_attr_val)

def graph_disagreement(G, node_attr, max_attr_value):
  '''
  Take a measure of global disagreement across the graph from a measure
  motivated in Musco et al., 2018. Note that their belief model uses
  beliefs in [0,1] so we convert node attributes to the [0,1] scale
  for comparability purposes.

  :param G: The networkx graph to take the measure on.
  :param node_attr: The node attribute (string) to take values for. This
  attribute must be a number.
  :param max_attr_value: The maximum that a belief value could be (belief
  resolution - 1).
  '''
  total = 0
  # This is done to keep the value in line w/ Musco et al. 2018's
  # scale of belief from [0,1]
  attrs = np.array([ G.nodes[node][node_attr] / max_attr_value for node in G.nodes ])
  adj = nx.adj_matrix(G)
  for node in G.nodes:
    total += (adj[node] * (attrs - attrs[node])**2)[0]
  return total/2
  
def nlogo_graph_disagreement(citizens, friend_links, node_attr, max_attr_val):
  G = nlogo_graph_to_nx(citizens, friend_links)
  return graph_disagreement(G, node_attr, max_attr_val)

def graph_democracy(G):
  return 0

def test_ws_graph_normal(n, k, p):
  G = nx.watts_strogatz_graph(n, k, p)
  agent_bels = normal_dist_multiple(7, 3, 1, n, 2)
  for i in range(n):
    G.nodes[i]['A'] = agent_bels[i][0]
    G.nodes[i]['B'] = agent_bels[i][1]
  return G

# Command to run: generate_media_ecosystems(500, 7, ["A"], [], 'D:/school/grad-school/Tufts/research/cog-contagion-media-ecosystem/ecosystems/')

def generate_media_ecosystems(citizen_n, resolution, malleables, priors, out_path):
  for e_type in file_names.keys():
    generate_media_ecosystem(citizen_n, e_type, resolution, malleables, priors, out_path)

def generate_media_ecosystem(citizen_n, e_type, resolution, malleables, priors, out_path):
  '''
  Create JSON files for media ecosystem setups (to generate
  institutional agents)

  :param citizen_n: The number of citizens in the system.
  :param e_type: INSTITUTIONAL_ECOSYSTEM_TYPE value
  :param resolution: Integer belief resolution 
  :param malleables: A list of malleable variables to put in the
  agent brain.
  :param priors: A list of prior variables to put in the
  agent brain.
  :param out_path: String path to make ecosystem files in
  '''

  if not os.path.isdir(f'{out_path}/{resolution}'):
    os.mkdir(f'{out_path}/{resolution}')

  f = open(f'{out_path}/{resolution}/{file_names[e_type]}.json', 'w')
  
  medias = []
  if e_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MIN:
    medias.append({ 'id': 'MIN', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [0], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MAX:
    medias.append({ 'id': 'MAX', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [resolution-1], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.ONE_MID:
    medias.append({ 'id': 'MID', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [math.floor(resolution/2)], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_POLARIZED:
    medias.append({ 'id': 'MIN', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [0], 'discrete') })
    medias.append({ 'id': 'MAX', 'brain': create_agent_brain(citizen_n+1, priors, malleables, [], [resolution-1], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.TWO_MID:
    medias.append({ 'id': 'LOWER', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [math.floor(resolution/4)], 'discrete') })
    medias.append({ 'id': 'UPPER', 'brain': create_agent_brain(citizen_n+1, priors, malleables, [], [3*math.floor(resolution/4)], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_POLARIZED:
    medias.append({ 'id': 'MIN', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [0], 'discrete') })
    medias.append({ 'id': 'MID', 'brain': create_agent_brain(citizen_n+1, priors, malleables, [], [math.floor(resolution/2)], 'discrete') })
    medias.append({ 'id': 'MAX', 'brain': create_agent_brain(citizen_n+2, priors, malleables, [], [resolution-1], 'discrete') })
  elif e_type == INSTITUTION_ECOSYSTEM_TYPES.THREE_MID:
    medias.append({ 'id': 'LOWER', 'brain': create_agent_brain(citizen_n, priors, malleables, [], [math.floor(resolution/6)], 'discrete') })
    medias.append({ 'id': 'MID', 'brain': create_agent_brain(citizen_n+1, priors, malleables, [], [math.floor(resolution/2)], 'discrete') })
    medias.append({ 'id': 'UPPER', 'brain': create_agent_brain(citizen_n+2, priors, malleables, [], [5*math.floor(resolution/6)], 'discrete') })
  f.write(json.dumps(medias))
  f.close()

def read_media_ecosystem_data(path):
  f = open(path, 'r')
  raw = f.read()
  data = json.loads(raw)
  f.close()
  return data