from enum import Enum

"""
A model of SIR epidemiological conditions to use for simulation.
"""

class State(Enum):
  SUSCEPTIBLE = 0
  INFECTED = 1
  REMOVED = 2

