% ********** GOAL MODEL **********

:- ensure_loaded(metamodel).

:- discontiguous stateVar/3.
:- discontiguous start/2.
:- discontiguous finish/2.
:- discontiguous intervalSize/2.
:- discontiguous atLeast/2.
:- discontiguous atMost/2.
:- discontiguous frequency/2.

:- dynamic currentContext/1.
:- dynamic goalStatus/2.
:- dynamic contextUninitiated/0.

subgoal(G1,G2) :-
    orSubgoal(and(Gs),G2),
    member(G1,Gs),
    isa(G1,goal).
subgoal(G1,G2) :-
    orSubgoal(G1,G2),
    isa(G1,goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%% GOAL MODEL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% supersedes(Goal1,Goal2)


% precondition(Subgoal/Effector,Goal)

precondition(g1_1,g1).
precondition(p3,g2).
precondition(p4,g3).
precondition(p5,g4).

% inhibits(Goal,Effector)

inhibits(g2_1,e2).
inhibits(g6_2,e1).

% succeeds([Effector],Delay)

succeeds(p1,100).
succeeds(p2,500).
succeeds(e1,1000).
succeeds(e2,500).
succeeds(e3,200).
succeeds(e4,50).
succeeds(p6,20).
succeeds(e4,50).
succeeds(e4,50).
succeeds(e4,50).


% orSubgoal(+Goal1,+Goal2)

orSubgoal(g1_1,g1).
orSubgoal(g1_2,g1).
orSubgoal(g2_1,g2).
orSubgoal(and([]),g3).
orSubgoal(and([g6_1,g6_2,g6_3]),g6).
orSubgoal(g8_1,g8).
orSubgoal(g2_1,g6).

% probe & effector subgoals

orSubgoal(p1,g1_1).
orSubgoal(p2,g1_1).
orSubgoal(e1,g1_2).
orSubgoal(e2,g1_2).
orSubgoal(e3,g2_1).
orSubgoal(p3,g2).
orSubgoal(p4,g3).
orSubgoal(p5,g4).
orSubgoal(p6,g6_3).
orSubgoal(e4,g6_1).
orSubgoal(e5,g8_1).

% violationAchieves


% suspends(Context,Goal)

suspends(responseC,g1).

% activates(Context,Goal)

activates(normalC,g1).
activates(normalC,g2).
activates(normalC,g3).
activates(normalC,g4).
activates(analysisC,g5).
activates(responseC,g6).
activates(learnC,g7).
activates(restorationC,g8).

% enables(Goal,Context)   - upon achievement

enables(g4,analysisC).
enables(g5,responseC).
enables(g6,learnC).
enables(g7,restorationC).
enables(g8,normalC).

% drops(Context,Goal)

drops(restorationC,g6_2).

achieves(p5,g4).
successCondition(p5,alertRaisedByIDS).
successCondition(g5,attackAnalysed).

% INSTANCES  

% goals

% inst(+Label,+Class)
% 
% Instantiate an object of a specified class



% inst(+Label,+Class,+Description)
% 
% Instantiate an object of a specified class with a description

inst(g1,integralGoal,"Service Availability").
inst(g1_1,goal,"Monitor Service Availability").
inst(g1_2,goal,"Establish Availability").

inst(g2,integralGoal,"Limit POD allocation").
inst(g2_1,goal,"Reduce Resources").

inst(g3,differentialGoal,"Limit Resource Change").

inst(g4,goal,"Detect DDOS attack").

inst(g5,goal,"Analyse Attack").

inst(g6,goal,"Mitigate Attack").
inst(g6_1,goal,"Filter Attack").
inst(g6_2,goal,"Prevent Restart").
inst(g6_3,goal,"Monitor Logs").

inst(g7,goal,"Confirm Attack Mitigated").

inst(g8,goal,"Restore Normality").
inst(g8_1,differentialGoal,"Redirect Requests").

inst(sg1,softgoal,"Maintain System Operations").

% effectors

inst(e1,effector,"Restart Failed Instance").
inst(e2,effector,"Spawn Instance").
inst(e3,effector,"Stop Instance").
inst(e4,effector,"Redirect Routes").
inst(e5,effector,"Restore Route").

% probes

inst(p1,probe,"Active Probing").
inst(p2,probe,"Monitor Response Times").
inst(p3,probe,"Monitor Resources").
inst(p4,probe,"Monitor Resources").
inst(p5,probe,"Alert raised by IDS").
inst(p6,probe,"Alert raised by IDS").

% ********** CALCULUS GOAL PARAMETERS **********

% stateVar(<goal>,<attribute functor>,<value argument position>).
% start(<goal>,<time>).
% finish(<goal>,<time>).
% intervalSize(<goal>,<value>).
% atLeast(<goal>,<value>).
% atMost(<goal>,<value>).
% frequency(<goal>,<value>).

% G1: Service Availability INTEGRAL
% 
% Services shall be available for no less than 1425 min in any 24hr
% interval (i.e., 99%)
% 
stateVar(g1,servicesAvailable,1).
start(g1,0).
finish(g1,inf).
intervalSize(g1,1440).
atLeast(g1,285).
atMost(g1,inf).
frequency(g1,5).

% G2: Limit POD allocation INTEGRAL
% 
% Limit number of pod allocation events to no more than 5 over the
% baseline (e.g., 3) while we have 2 nodes.
% baseline = 3 pod allocation events
% 
stateVar(g2,podAllocationEvent,0).
start(g2,0).
finish(g2,inf).
intervalSize(g2,1440).
atLeast(g2,3).
atMost(g2,8).
frequency(g2,inf).

% G3: Limit resource change DIFFERENTIAL
% 
% Planning constraint limits resource alloc. change (no. pods) to within
% 35% (i.e., 2 pods) per 5min
% 
% 
stateVar(g3,resourceUsage,1).
start(g3,0).
finish(g3,inf).
intervalSize(g3,5).
atLeast(g3,0).
atMost(g3,7).
frequency(g3,5).

% G8.1 Redirect Requests DIFFERENTIAL
% 
% Redirect requests to original; no more than 20% of service instances
% (i.e., 1 instance) per 5 mins
% 
stateVar(g8_1,instances,1).
start(g8_1,0).
finish(g8_1,inf).
intervalSize(g8_1,5).
atLeast(g8_1,0).
atMost(g8_1,0.2).
frequency(g8_1,5).

% ********** GOAL STATUSES & CONTEXTS **********

currentContext(normalC).
contextUninitiated.

goalStatus(g1,inactive).
goalStatus(g1_1,inactive).
goalStatus(g1_2,inactive).
goalStatus(g2,inactive).
goalStatus(g2_1,inactive).
goalStatus(g3,inactive).
goalStatus(g4,inactive).
goalStatus(g5,inactive).
goalStatus(g6,inactive).
goalStatus(g6_1,inactive).
goalStatus(g6_2,inactive).
goalStatus(g6_3,inactive).
goalStatus(g7,inactive).
goalStatus(g8,inactive).
goalStatus(g8_1,inactive).

goalStatus(e1,inactive).
goalStatus(e2,inactive).
goalStatus(e3,inactive).
goalStatus(e4,inactive).
goalStatus(e5,inactive).

goalStatus(p1,inactive).
goalStatus(p2,inactive).
goalStatus(p3,inactive).
goalStatus(p4,inactive).
goalStatus(p5,inactive).
goalStatus(p6,inactive).
