'''
A module to safely report graph structures and functions
from Python to NetLogo.

Author: Nick Rabb (nick.rabb2@gmail.com)
'''

import networkx as nx

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
Return NetLogo-safe graph structures.

:param G: The networkx graph to convert.
'''
def nlogo_safe_nodes_edges(G):
  nodes = list(G.nodes)
  edges = [ [e[0], e[1]] for e in G.edges ]
  return { 'nodes': nodes, 'edges': edges }