import numpy as np
import math
from random import *

"""
Use Kronecker matrix multiplication to extend an initial matrix
g to its Kronecker product, g2. This process is described
in Leskovec & Faloutsos, 2007, "Scalable Modeling of Real Graphs
using Kroncker Multiplication"

:param g_init: The initial seed graph, U
:param g: The graph to multiply it by, V.
"""
def kronecker(g_init, g):
    g2 = []
    for i in range(0, g_init.shape[0]):
        new_row = []
        for j in range(0, g_init.shape[1]):
            if j == 0:
                new_row = g.copy() * g_init[i,j]
            else:
                new_row = np.concatenate((new_row, g*g_init[i,j]), 1)
        if i == 0:
            g2 = new_row
        else:
            g2 = np.concatenate((g2, new_row), 0)
    return g2

"""
Take the power product of a single Kronecker seed graph k times -
i.e. recursively take the Kronecker product of the initial graph
with its results to depth k.

:param g: The graph as an adjacency matrix to use.
:param k: The power to raise this Kronecker product to.
"""
def kronecker_pow(g, k):
    return kronecker_rec(g, g, k, 1)

def kronecker_rec(g_init, g, k, i):
    if (i == k):
        return g
    gk = kronecker(g_init, g)
    return kronecker_rec(g_init, gk, k, i+1)

def kronecker_stochastic(g, k):
    N1 = g.shape[0]
    P = []
    Nk = int(math.pow(N1, k))
    for u in range(0, Nk):
        P.append([])
        for v in range(0, Nk):
            # Calculate product of Thetas
            prod = 1
            for i in range(0, k):
                theta_u = int(math.floor(u/math.pow(N1, i)) % N1)
                theta_v = int(math.floor(v/math.pow(N1, i)) % N1)
                prod *= g[theta_u,theta_v]
            P[u].append(prod)
    return np.array(P)

"""
Test the Kronecker multiplication functions.
"""
def test_kronecker():
    #a = np.array([[1,1,0],[1,1,1],[0,1,1]])
    #g = kronecker_pow(a, 3)
    #print(g, g.shape)
    b = np.array([[0.7,0.1],[0.1,0.8]])
    g2 = kronecker_pow(b, 4)
    print(g2)

def test_kronecker_stochastic():
    b = np.array([[0.7,0.1],[0.1,0.8]])
    g2 = kronecker_stochastic(b, 4)
    print(g2)

def main():
    test_kronecker()
    test_kronecker_stochastic()

if __name__ == '__main__':
    main()

