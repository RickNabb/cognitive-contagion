'''
A module to safely report graph structures and functions
from Python to NetLogo.

Author: Nick Rabb (nick.rabb2@gmail.com)
'''

import networkx as nx
import mag
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
  p_edge = mag.attr_mag(n, attrs, style)
  G = nx.Graph()
  G.add_nodes_from(range(0, len(p_edge[0])))
  for i in range(0,len(p_edge)):
    for j in range(0,len(p_edge)):
      rand = random()
      if (rand <= p_edge[(i,j)]):
        G.add_edge(i, j)
  return nlogo_safe_nodes_edges(G)

'''
Return NetLogo-safe graph structures.

:param G: The networkx graph to convert.
'''
def nlogo_safe_nodes_edges(G):
  nodes = list(G.nodes)
  edges = [ [e[0], e[1]] for e in G.edges ]
  return { 'nodes': nodes, 'edges': edges }