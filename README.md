# Cognitive Contagion

## Author: Nick Rabb 

#### Contact: nicholas.rabb@tufts.edu

<hr/>

## Project Overview

This repository contains the code used to do modeling of cognitive contagion phenomena (paper in review). In a nutshell, cognitive contagion is a social contagion framework used to model the spread of beliefs through networks where each agent in the network is adopting or rejecting beliefs based on an internal cognitive model. This work extends similar social contagion work that has been done around [*simple contagion*](https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.5408?casa_token=TEr9gN2OBv8AAAAA:pPNtSdI_yFDX7tHnVZ3RGxYiQH6lzxLp6t3aKzy3AQjQUlj_3x-ardPnhDo4HI4m491e7l0IYpDY7Q) and [*complex contagion*](https://www.journals.uchicago.edu/doi/full/10.1086/521848?casa_token=nLalDS_cZ7EAAAAA:BsRiNKpW_lFrj8K_cXWlvwBPEUWA_B1B16pnp8yiog2w-VYi8HCIs34Hd6lMe7ltpkCu3M5vJQ). This work also builds on a large body of work done in the Agent-Based Modeling (ABM) arena that has studied and modeled the spread of ideas such as innovations (for a comprehensive review, see [Zhang & Vorobeychik 2019](https://link.springer.com/article/10.1007/s10462-017-9577-z)).

The models that we have made are co-constructed through the ABM tool NetLogo, and Python files it runs through its Python extension. The simulation file `cognitive-contagion.nlogo` contains implementations of simple, complex, and cognitive contagion models, as well as several random graph structures to test simulations in. 

## Project Structure

This project's main simulation code is located in the `cognitive-contagion.nlogo` file in the root. This NetLogo project runs code from all the associated `.py` files with its Python extension. Model logic is broken up between the NetLogo simulation and its associated Python scripts.

### Main Simulation Files

The bulk of the simulation is done through the `cognitive-contagion.nlogo` and `messaging.py` scripts. Because these files are so crucial to the understanding of this project, we will describe each in turn.

#### `cognitive-contagion.nlogo`

This NetLogo file contains most of the code necessary to run the simulation. Because NetLogo is the primary ABM system we use for the simulations, we delegate most of the agent construction, network construction, and interaction code to this file. Additionally, it contains code necessary to interface with `.py` files, and report post-simulation results so they can be processed by analysis Python files.

For more information on how the simulation model itself is set up, see the [simulation breakdown](#simulation-breakdown) section below.

#### `messaging.py`

This file contains all of the scripting to handle interactions between messages and agent belief models. There are descriptions of the scheme we used for both in the file, so anyone interested should find more there.

In general, functions in this file are responsible for creating message vectors, agent cognitive models (referred to in the file as "brains"), and handling the receipt and belief of messages, and subsequent updating of cognitive models. As each agent in the model is primarily tasked with holding beliefs, and sharing messages they hear, these functions are the means by which this occurs. Many of these functions have corollaries in `cognitive-contagion.nlogo` which receive input back and utilize it in the main simulation model.

### Analysis Files

In particular, the `stats.py`, `data.py`, and `plotting.py` files are largely used to conduct post-simulation data analysis. The analyses found in our corresponding paper (in review) were conduced through these Python scripts.

### Utility Files

`utils.py`, `nlogo_colors.py` and `nlogo_graphs.py` hold code that was useful for aiding in the simulation process. The code in these files should more or less speak for itself.

## Simulation Breakdown

The theoretical structure of the simulation is more clearly explicated in the paper (in review), but we can provide a sufficient description so that the simulation code can be understood. There are a few main components of the simluation that we will describe in turn:

* Agents
* Networks
* Messages
* Institutional agents
* Simulation Step

### Citizen Agents

Agents are the main building-block of any ABM. In our model we have two types of agents: `medias` and `citizens`. Citizen agents are those we will describe here, and institutional agents will be described [below](#institutional-agents).

Citizens are agents with cognitive models holding their belief strengths in particular beliefs. In our model, these beliefs can be found in `data.py` represented as Enumerations. The only belief we used in the simple model for our paper was `TestDiscrete` (also referred to by the belief attribute `A`). Citizen cognitive models can have either `prior` attributes or `malleable` attributes: the former being ones that *do not* change, and the latter being changeable. The citizen cognitive model also has attributes `alpha`, `beta`, and `threshold`. `alpha` refers to the distance between a received message and the agent's beliefs necessary to *share* the message. `beta` refers to the distance necessary to *believe* the message and update agent beliefs. `threshold` is used for a discrete belief update model using tokens. Finally, the cognitive model can be of two types: discrete, or continuous. Discrete agent cognitive models use a different update mechanism than continuous agent cognitive models.

Additionally, in the NetLogo simulation, citizen agents keep track of messages that they have *heard* and *believed*. Because each message has a unique ID (combined with its time step, as described [below](#messages)), the agent can keep a list of these messages for post-simulation analysis. Messages that have been *heard* are simply ones that reached that agent on a given time step, but were not necessarily believed. Messages that were *believed* are ones that ended up updating agent beliefs, and were subsequently shared with that agent's neighbors.

### Networks

Agents are networked together in the simluation with two types of NetLogo links: `social-friends` and `subscribers`.

Social friend links are undirected edges between citizen agents. These are constructed in accordance to whichever graph topology is selected in the simulation controls. These links dictate how agents share messages across the graph.

Subscribers are directed ties between an institutional agent and a citizen agent. These are determined by the `epsilon` parameter: if an institutional agent's held beliefs are within `epsilon` distance from an agent's beliefs, then the agent will subscribe to the institution. These links are formed at the *beginning* of the simulation and *not altered* throughout the course of it. Subscriber links are how the institution-initiated messages begin their spread through the citizen population.

### Messages

Messages are represented as vectors of belief attributes. These are initially spread from institutional agents to subscribers, and then subsequently spread based on the mechanisms described above in the [citizen agents](#citizen-agents) section. Importantly, in the simulation, every message is given a *unique ID* that governs whether or not citizen agents interact with it. This unique ID is a combination of the time step which it was intitiated during, and an incremented integer. Insitutional agents additionally keep track of which message IDs they initiated based on a given time step. 

### Institutional Agents

Insitutional agents are those which citizen agents subscribe to (described [above](#citizen-agents)). They initiate the spread of beliefs according to a messging `.json` file specified in the NetLogo simulation controls. These agents do not change their beliefs, and are not influenced in any way by citizen agents.

### Simulation Step

To understand the simulation, it is necessary to understand what happens during a single time step of the process. First, a given institutional agent shares its message for that time step (specified by a messaging file) with its subscribers. Each subscriber, in turn, has a chance to *believe* and *share* the message. This is how cascades start through the network. After a cascade has reached its conclusion, either the next message is shared by the institutional agent, or the next institutional agent begins sharing its messages. After all messages for this time step were initiated by institutional agents, the time step (tick in NetLogo) increases by 1.

## How to Run Simulations

Simulations are run through the NetLogo **Interface** tab. Parameters can be set in turn (parameters described below) before starting the simulation. Once parameters are set, the **setup** button can be pressed. This will take some time to generate a graph of agents and set up the initial conditions of the simulation. After setup is complete, the simulation can either be run continuously with the **go** button, or one time step can be executed with the **step** button. The simulation will end once the ticks reach the number specified in `tick-end`.

## Experiments in Cognitive Cascade paper

This is a description of how to replicate experiments from the Rabb et al. 2021 paper, "Cognitive cascades: How to model (and potentially counter) the spread of fake news."

### BehaviorSpace

All of the experiments were run through the `cognitive-contagion.nlogo` NetLogo file, inside of the BehaviorSpace tool (Tools -> BehaviorSpace). This is simply a built-in tool that allows for running simulations multiple times and reporting results into data sheets. Our paper results were built on doing analysis on top of those raw experiment results.

### graph-exp

This experiment generated data that supports most of our main results. This experiment will run cascade processes across all contagion methods (simple, complex, cognitive), all graph topologies (ER, BA, WS, MAG), and all message sets (single, split, gradual). Additionally, the experiment can be tweaked to run the cascade processes at differing belief resolutions by adding to the array for the `belief-resolution` variable. Our experiments were run with `N=500` and `cognitive-fn='sigmoid-stubborn'`. Changing the `Repetitions` field in the experiment dialogue will also change how many times a single combination of parameters is run. To support our results across differing numbers of simulation runs, we changed this parameter.

Results are output to the directory given to the `sim-output-dir` field in the main NetLogo simulation interface. To aggregate results and generate graphs like those in the main paper and supplemental materials, the `process_graph_exp_outputs` function in `data.py` was run on the directory where raw results were located. This creates a `results` directory that contains aggregated graphs of means and variances. Results from this process supported our results in Figs. 7-10 in the main paper, and S12-21, S23-S26 in supplemental materials.

### cognitive-exp

This experiment generated data that supports results for testing the contagion functions beta described by Eq. 4-6. This experiment does not require changing any variables in the saved experiment in this repository to reproduce the results that led to those in our main paper. Simply running the experiment is sufficient.

Graphs that we analyze in S5-S10 Figs are generated by running the `process_cognitive_exp_outputs` function in `data.py` given the output directory where experiment results were written. As in the graph-exp experiment, a `results` directory is created to store visual graphs of the means across however many experiments are conducted.