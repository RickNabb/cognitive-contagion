import numpy as np
from data import *
from stats import *
import networkx as nx

def mag(n, L, Thetas):
  P = []
  for u in range(0, n):
    P.append([])
    for v in range(0, n):
      if (u == v):
        P[u].append(0)
      else:
        # The two attribute lists for agent1 (u) and agent2 (v)
        lu = L[u]
        lv = L[v]
        p = 1
        for i in range(0, len(lu)):
          p *= Thetas[i][(lu[i],lv[i])]
        P[u].append(p)
  return np.array(P)

def sample_L(n, attrs, resolution):
  # Create nodes w/ attributes
  L = []
  for i in range(0, n):
    L.append(list(map(lambda attr: random_dist_sample(attr, resolution), attrs)))
  return L

def print_IP(L):
    Is = [ 'VLib', 'Lib', 'Mod', 'Con', 'VCon' ]
    Ps = [ 'Dem', 'Ind', 'Rep' ]
    counted = {}
    for l in L:
        tup = (Is[l[0]],Ps[l[1]])
        if tup not in counted:
            counted[tup] = 1
        else:
            counted[tup] += 1
    print(counted)

'''
Return a probability matrix for the probabilities of each node i connecting
to node j.

:param n: The number of nodes.
:param attrs: A list of attributes to gather Theta affinity matrices for in order
to properly calculate the product of all attribute affinities for the matrix.
:param style: A string denoting how to connect the attributes - default, homophilic, or heterophilic.
:param resolution: An integer denoting how finely to break up discrete beliefs.
'''
def attr_mag(n, attrs, style, resolution):
  L = sample_L(n, attrs, resolution)
  Thetas = list(map(lambda attr: AttributeMAGThetas[attr.name][style](resolution), attrs))
  # print(Thetas)
  g = mag(n,L,Thetas)
  # print(g)
  return (g, L)

def main():
    #test_create_dist()
    print(attr_mag(25, [ Attributes.TestDiscrete7 ]))

if __name__ == '__main__':
    main()

