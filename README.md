# Goal Reasoning using rates of change and aggregation

This repository contains the implementation for our paper entitled _Modeling Rates of Change and Aggregations in Runtime Goal Models_, which has been accepted for presentation in ER 2022 : 41st International Conference on Conceptual Modeling (citation below).

The research presented in our publication, and the implementation supplied in this repository, forms part of the Advanced Integrated Modelling Environment (AIME) project. The AIME project aims to deliver self-integrating architectures to enable agile C2 information systems for the Defence Science and Technology Group (DSTG). AIME is a collaboration between the University of South Australia (UniSA) and the University of Wollongong (UOW).

**Acknowledgements:**
This research is supported by the Commonwealth of Australia as represented by the Defence Science and Technology Group of the Department of Defence and the Defence Artificial Intelligence Research Network (DAIRNet), an initiative of the Department of Defence and the Next Generation Technologies Fund (NGTF).

**Citation:**
_Morgan, R., Pulawski, S., Selway, M., Mayer, W., Grossmann, G., Stumptner M., Ghose, A., Kyprianou, R.: Modeling Rates of Change and Aggregations in Runtime Goal Models. 41st International Conference on Conceptual Modeling (ER 2022)_ 

---

## Requirements

To run the program [SWI-Prolog](https://www.swi-prolog.org/) must be installed.

## Quickstart

To load the program run the following line from the command line:

```
swipl goalReasoner.pl
```

To run the program and generate output run the following query in SWI-Prolog prompt immediately after loading the program:

```prolog
?- run.
```

To observe an example where no goals fail, run the program in branch **main**. To observe an example where an integral goal fails, run the program in branch **failure-example**. See below for more details.

---
## Success & Failure Examples

The branch **main** contains an example of a goal model and state history for which no goals have failed. Each active integral and differential goal (those in the initial _Normal Context_) are checked against the state histories by considering each goal's parameters. For example, the parameters for goal _G1: Service Availability_ are as follows:

```prolog
stateVar(g1,servicesAvailable,1).
start(g1,0).
finish(g1,inf).
intervalSize(g1,1440).
atLeast(g1,285).
atMost(g1,inf).
frequency(g1,5).
```

State histories take the ```at(Predicate,Time)```. Some examples of state history predicates pertaining to goal _G1_ are shown below:

```prolog
at(servicesAvailable(1),15).
at(servicesAvailable(0),20).
at(servicesAvailable(1),25).
```

These state histories are used to determine the success, failure or nofail conditions of differential and integral goals.

Because there is no failure when checking each goal, the goal model in branch **main** switches contexts from _Normal Context_ to _Analysis Context_ when probe _P5: Alert raised by IDS_ is determined to have been achieved because its success condition ```alertRaisedByIDS``` was found to be true at the current timestep. In the subsequent time step, _G5: Analyse Attack_ was determined to be successful due to its success condition ```attackAnalysed``` being true and so the _Response Context_ was activated.

Each time a context is activated, some new goals are activated and some old goals may be suspended. These are specified using the predicates ```activates(Context,Goal).``` and ```suspends(Context,Goal).``` respectively. The activation and suspension of each goal when shifting context can be observed in the output of running the program in branch **main**.

When running the program in branch **failure-example**, it can be observed that the program exits due to goal _G1_ failing. This is a consequence of providing a different state history where the service is considered unavailable in time steps 0-25 given the following predicates:

```prolog
at(servicesAvailable(0),0).
at(servicesAvailable(0),5).
at(servicesAvailable(0),10).
at(servicesAvailable(0),15).
at(servicesAvailable(0),20).
at(servicesAvailable(0),25).
```
After summing the total number of time steps where the service is available it is determined that the service was unavailable for too long given the integral goal's parameters and so the goal fails.
