'''
A module to safely report graph structures and functions
from Python to NetLogo.

Author: Nick Rabb (nick.rabb2@gmail.com)
'''

import networkx as nx
import numpy as np
import mag
from messaging import *
from random import random

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
def WS_graph(n, k, p):
  G = nx.watts_strogatz_graph(n, k, p)
  return nlogo_safe_nodes_edges(G)

'''
Return a Netlogo-safe Barabasi-Albert graph from the NetworkX package.

:param n: The number of nodes.
:param m: The number of edges to connect with when a node is added.
'''
def BA_graph(n, m):
  G = nx.barabasi_albert_graph(n, m)
  return nlogo_safe_nodes_edges(G)

'''
Create a MAG graph for N nodes, given L attributes, and a style of connection
if there is no specified connection affinity matrix.

:param n: The number of nodes.
:param attrs: A list of attributes to gather Theta affinity matrices for in order
to properly calculate the product of all attribute affinities for the matrix.
:param style: A string denoting how to connect the attributes - default, homophilic, or heterophilic.
'''
def MAG_graph(n, attrs, style):
  (p_edge, L) = mag.attr_mag(n, attrs, style)
  # print(p_edge)
  # print(L)
  G = nx.Graph()
  G.add_nodes_from(range(0, len(p_edge[0])))
  for i in range(0,len(p_edge)):
    for j in range(0,len(p_edge)):
      rand = random()
      if (rand <= p_edge[(i,j)]):
        # if (abs(L[i][0]-L[j][0]) >= 2):
          # print(f'Chance to connect {L[i]} and {L[j]}: {p_edge[(i,j)]}')
          # print(f'Rolled {rand}: {rand <= p_edge[(i,j)]}')
        G.add_edge(i, j)
  # print(f'Num edges: {len(G.edges)}')
  nlogo_G = nlogo_safe_nodes_edges(G)
  nlogo_G.update({'L': L})
  return nlogo_G

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
    for attr in cit['malleable']:
      G.nodes[cit_id][attr] = cit[attr]
    for attr in cit['prior']:
      G.nodes[cit_id][attr] = cit[attr]
  for link in friend_links:
    link_split = link.split(' ')
    end1 = link_split[1]
    end2 = link_split[2].replace(')','')
    G.add_edge(int(end1), int(end2))
  return G

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
    distance_paths[subscriber] = dist_path
    if sum((np.array(dist_path)-threshold) > 0) == 0:
      threshold_paths[subscriber] = dist_path
      # threshold_paths[subscriber] = paths[subscriber]
  return threshold_paths
