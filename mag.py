import numpy as np
from data import *
from stats import *

def mag(n, L, Thetas):
    P = []
    for u in range(0, n):
        P.append([])
        for v in range(0, n):
            # The two attribute lists for agent1 (u) and agent2 (v)
            lu = L[u]
            lv = L[v]
            p = 1
            for i in range(0, len(lu)):
                p *= Thetas[i][lu[i]][lv[i]]
            P[u].append(p)
    return np.array(P)

def political_L(n):
    # Create nodes w/ attributes
    L = []
    for i in range(0, n):
        ideology = sample_dist(IVals, IDist)
        partisanship = sample_dist(PIValsByI[ideology], PGivenIDist[ideology])
        L.append([ideology, partisanship])
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

def political_mag(n):
    L = political_L(n)
    #print_IP(L)
    Thetas = [I_homophily,P_homophily]
    g = mag(n,L,Thetas)
    return g


def main():
    #test_create_dist()
    print(political_mag(25))

if __name__ == '__main__':
    main()

