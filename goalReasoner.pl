% Differential and Integral Goal Reasoner
% 
% This program reasons about goal status and contexts shift by checking goal conditions
% including the satisfaction or failure of novel differential and integral goal types
% 

% ********** PROGRAM HEAD *********
% 

:- ensure_loaded(auxillary).
:- ensure_loaded(metaModel).
:- ensure_loaded(goalModel).
:- ensure_loaded(stateHistory).

:- dynamic currentTime/1.

% ********** GOAL CHECKING PROCEDURES **********

% checkGoal(+Goal)
% 
% check to see if a differential/integral goal as been, achieved, failed, on-going etc.
% 
checkGoal(Goal) :- 
    isa(Goal,differentialGoal),
    goalStatus(Goal,active),
    writeList(1,["Checking active differential goal:",Goal]),
    checkDiff(Goal,Return),
    setGoalStatus(Goal,Return).
checkGoal(Goal) :- 
    isa(Goal,differentialGoal),
    goalStatus(Goal,active),
    \+ checkDiff(Goal,_),
    writeList(1,["Goal",Goal,"failed"]),
    setGoalStatus(Goal,failed).
checkGoal(Goal) :-
    isa(Goal,integralGoal),
    goalStatus(Goal,active),
    writeList(1,["Checking active integral goal:",Goal]),
    checkIntegral(Goal,Return),
    setGoalStatus(Goal,Return).
checkGoal(Goal) :-
    isa(Goal,integralGoal),
    goalStatus(Goal,active),
    \+ checkIntegral(Goal,_),
    writeList(1,["Goal",Goal,"failed"]),
    setGoalStatus(Goal,failed).
checkGoal(Goal) :-
    isa(Goal,goal),
    successCondition(Goal,Cond),
    currentTime(CurrTime),
    at(Cond,CurrTime),
    setGoalStatus(Goal,achieved).
checkGoal(Goal) :-
    isa(Goal,goal),
    successCondition(Goal,Cond),
    currentTime(CurrTime),
    \+ at(Cond,CurrTime).
checkGoal(_) :-
    writeList(1,["checkGoal not working"]), !,
    fail.

% checkGoalList(+GoalList)
% 
% Runs checkGoal on all goals in GoalList
% 
checkGoalList([]).
checkGoalList([G]) :-
    checkGoal(G).
checkGoalList([G|Gs]) :-
    checkGoal(G),
    checkGoalList(Gs).

% checkDiff(+Goal,-Status)
% 
% Checks the differential goal and returns nofail or achieved
% 
checkDiff(Goal,nofail) :-
    start(Goal,Start),
    finish(Goal,Finish),
    currentTime(Time1),
    Time1 < Finish,
    Time1 > Start,
    intervalSize(Goal,Interval),
    Time2 is Time1 - Interval,
    checkDiff(Goal,Time1,Time2).
checkDiff(Goal,achieved) :-
    start(Goal,Start),
    finish(Goal,Finish),
    currentTime(Time1),
    Time1 >= Finish,
    Time1 > Start,
    intervalSize(Goal,Interval),
    Time2 is Time1 - Interval,
    checkDiff(Goal,Time1,Time2).

checkDiff(Goal,Time1,Time2) :-
    start(Goal,Start),
    atLeast(Goal,LowerBound),
    atMost(Goal,UpperBound),
    val(Goal,Time1,Val1),
    val(Goal,Time2,Val2),
    TimeDiff is Time1 - Time2,
    abs(Val1 - Val2, ValDiff),
    RateChange = ValDiff / TimeDiff,
    RateChange >= LowerBound,
    RateChange =< UpperBound,
    Time2 > Start,
    NewTime1 is Time1 - TimeDiff,
    NewTime2 is Time2 - TimeDiff,
    checkDiff(Goal,NewTime1,NewTime2).
checkDiff(Goal,Time1,Time2) :-
    start(Goal,Start),
    atLeast(Goal,LowerBound),
    atMost(Goal,UpperBound),
    val(Goal,Time1,Val1),
    val(Goal,Time2,Val2),
    TimeDiff is Time1 - Time2,
    abs(Val1 - Val2, ValDiff),
    RateChange = ValDiff / TimeDiff,
    RateChange >= LowerBound,
    RateChange =< UpperBound,
    Time2 =< Start.

% val(+Goal,+Time,-Val)
% 
% Returns the value of a Goal's stateVar at Time
% 
val(Goal,Time,Val) :-
    stateVal(Goal,Pred,Val),
    at(Pred,Time).

% checkIntegral(+Goal,-Return)
% 
% Checks an integral goal and returns the updated goalStatus
checkIntegral(Goal,Return) :-
    stateVal(Goal,Pred,_),
    start(Goal,Start),
    intervalSize(Goal,Interval),
    atLeast(Goal,LowerBound),
    atMost(Goal,UpperBound),
    currentTime(CurrTime),
    \+ Start = -inf,
    LowerTime is CurrTime - Interval,
    LowerTime >= Start,
    findall(Pred,(at(Pred,Time),Time > LowerTime,Time =< CurrTime),StateVarList),
    sumStateVarList(Goal,0,StateVarList,Sum), !, % if this is false we do not want to backtrack past it but simply fail
    Sum >= LowerBound,
    Sum =< UpperBound,
    checkGoalReturn(Goal,Return).
checkIntegral(Goal,_) :-
    stateVal(Goal,Pred,_),
    start(Goal,Start),
    intervalSize(Goal,Interval),
    atMost(Goal,UpperBound),
    currentTime(CurrTime),
    LowerTime is CurrTime - Interval,
    LowerTime < Start,
    findall(Pred,(at(Pred,Time),Time >= Start,Time =< CurrTime),StateVarList),
    sumStateVarList(Goal,0,StateVarList,Sum), !, % if this is false we do not want to backtrack past it but simply fail
    Sum =< UpperBound.
checkIntegral(Goal,Return) :-
    stateVal(Goal,Pred,_),
    start(Goal,Start),
    intervalSize(Goal,Interval),
    atLeast(Goal,LowerBound),
    atMost(Goal,UpperBound),
    currentTime(CurrTime),
    Start = -inf,
    LowerTime is CurrTime - Interval,
    firstTimestep(Goal,-inf,CurrTime,FirstTime),
    LowerTime >= FirstTime,
    findall(Pred,(at(Pred,Time),Time > LowerTime,Time =< CurrTime),StateVarList),
    sumStateVarList(Goal,0,StateVarList,Sum), !, % if this is false we do not want to backtrack past it but simply fail
    Sum >= LowerBound,
    Sum =< UpperBound,
    checkGoalReturn(Goal,Return).
checkIntegral(Goal,Return) :-
    stateVal(Goal,Pred,_),
    start(Goal,Start),
    intervalSize(Goal,Interval),
    atMost(Goal,UpperBound),
    currentTime(CurrTime),
    Start = -inf,
    LowerTime is CurrTime - Interval,
    firstTimestep(Goal,-inf,CurrTime,FirstTime),
    LowerTime < FirstTime,
    findall(Pred,(at(Pred,Time),Time >= FirstTime,Time =< CurrTime),StateVarList),
    sumStateVarList(Goal,0,StateVarList,Sum), !, % if this is false we do not want to backtrack past it but simply fail
    Sum =< UpperBound,
    checkGoalReturn(Goal,Return).

checkGoalReturn(Goal,nofail) :-
    currentTime(CurrTime),
    finish(Goal,Finish),
    CurrTime < Finish.
checkGoalReturn(Goal,achieved) :-
    currentTime(CurrTime),
    finish(Goal,Finish),
    CurrTime >= Finish.

lowerTime(Goal,LowerTime) :-
    intervalSize(Goal,Interval),
    currentTime(CurrTime),
    start(Goal,Start),
    LowerTime is CurrTime - Interval,
    LowerTime >= Start.
lowerTime(Goal,Start) :-
    intervalSize(Goal,Interval),
    currentTime(CurrTime),
    start(Goal,Start),
    LowerTime is CurrTime - Interval,
    LowerTime < Start.

% stateVal(+Goal,-Pred,-Val)
% 
% Used to retrieve the predicate and value of a goals given stateVar
% Additional clause required for different arity and value argument positions
% in the predicate
% 
stateVal(Goal,Pred,1) :-
    stateVar(Goal,Pred,0).
stateVal(Goal,Pred,Val) :-
    stateVar(Goal,Attr,1),
    Pred=..[Attr,Val].

sumStateVarList(_,_,[],_) :-
    writeList(1,["StateVarList empty"]),
    fail.
sumStateVarList(Goal,OldSum,[Pred],NewSum) :-
    stateVal(Goal,Pred,Val),
    Sum is OldSum + Val,
    NewSum is Sum.
sumStateVarList(Goal,OldSum,[Pred|SVs],NewSum) :-
    stateVal(Goal,Pred,Val),
    Sum is OldSum + Val,
    sumStateVarList(Goal,Sum,SVs,NewSum).

% firstTimestep(Goal,-Time)
% 
% Returns the time and value of the first timestep for a goal
% 
firstTimestep(Goal,Time,Val) :-
    stateVal(Goal,Pred,Val),
    start(Goal,Start),
    at(Pred,Time),
    Time >= Start,
    \+ intermedTimestep(Goal,Start,Time).

% nextTimestep(+Predicate,+LastTime,-Time)
% 
% Finds the next timestep at(Predicate,Time) with Predicate after LastTime
% returns the time and the value of the timestep
% 
nextTimestep(Goal,LastTime,Time,Val) :-
    stateVal(Goal,Pred,Val),
    at(Pred,Time),
    Time > LastTime,
    \+ intermedTimestep(Goal,LastTime,Time). % ensures no timesteps are skipped during the scan if they are out of order

% intermedTimestep(+Goal,+Time1,+Time2)
% 
% Given a goal and two times, return true if a timestep exists between Time1 & Time2
% 
intermedTimestep(Goal,Time1,Time2) :-
    stateVal(Goal,Pred,_),
    at(Pred,Time),
    Time > Time1,
    Time < Time2.

% raiseByInterval(+Goal,+LowerTime,-UpperVal,-TimeDiff)
% 
% Takes the LowerTime being scanned through, finds the timestep one interval
% higher and returns its value.
% Also returns the TimeDiff. If timesteps are not at regular intervals this becomes
% important as the higher time may be higher than lower time + one interval.
% 
raiseByInterval(Goal,LowerTime,UpperVal,TimeDiff) :-
    stateVal(Goal,Pred,UpperVal),
    intervalSize(Goal,Interval),
    finish(Goal,Finish),
    UpperTime is LowerTime + Interval,
    writeList(1,["UpperTime:",UpperTime]),
    at(Pred,Time),
    Time >= UpperTime,
    Time =< Finish,
    TimeDiff is Time - LowerTime,
    writeList(1,["Time2:",Time]).

% latestTimestepInInterval(+Goal,+Time)
% 
% Returns the latest timestep up to and including Time.
% If the latest timestep is less than Time, there must be a timestep
% greater than Time.
% 
% Return Time if there is a timestep at Time
latestTimestepInInterval(Goal,Time) :-
    stateVal(Goal,Pred,_),
    at(Pred,OtherTime),
    OtherTime = Time.
% Else return the time of the first timestep lower than Time
% fails if there is no timestep greater than Time because we hit the end of the data
latestTimestepInInterval(Goal,Time) :-
    stateVal(Goal,Pred,_),
    at(Pred,OtherTime),
    OtherTime < Time,
    \+ intermedTimestep(Goal,OtherTime,Time),
    nextTimestep(Goal,Time,_,_). % ensure there is still a timestep after Time

% lastTimestep(+Goal,+Time1,+Time2,-LastTime)
% 
% Returns the latest timestep time within the interval [Time1,Time2]
% 
lastTimestep(Goal,LowerTime,UpperTime,LastTime) :-
    stateVal(Goal,Pred,_),
    at(Pred,LastTime),
    LastTime >= LowerTime,
    LastTime =< UpperTime,
    \+ intermedTimestep(Goal,LastTime,UpperTime).

% firstTimestep(+Goal,+Time1,+Time2,-LastTime)
% 
% Returns the earliest timestep time within the interval [Time1,Time2]
% 
firstTimestep(Goal,LowerTime,UpperTime,FirstTime) :-
    stateVal(Goal,Pred,_),
    at(Pred,FirstTime),
    FirstTime >= LowerTime,
    FirstTime =< UpperTime,
    \+ intermedTimestep(Goal,LowerTime,FirstTime).

% ********** GOAL STATE AND CONTEXT CHECKING PROCEDURES **********

% activateCurrentContext
% 
% Initializes the first context
% 
activateCurrentContext :-
    currentContext(CurrContext),
    contextUninitiated,
    writeList(1,["Activating context:",CurrContext]),
    findall(Goal,activates(CurrContext,Goal),ActivateTopLevelGoals),
    setGoalStatusTopDown(ActivateTopLevelGoals,active),
    retract(contextUninitiated).

% activateNewContext(+NewContext) 
% 
% Activates the goals that NewContext activates and suspends the goals it suspends
% 
activateNewContext(NewContext) :-
    writeList(1,["Activating context:",NewContext]),
    findall(G1,activates(NewContext,G1),ActivateTopLevelGoals),
    setGoalStatusTopDown(ActivateTopLevelGoals,active),
    findall(G2,suspends(NewContext,G2),SuspendTopLevelGoals),
    setGoalStatusTopDown(SuspendTopLevelGoals,suspended),
    retract(currentContext(_)),
    assert(currentContext(NewContext)).

% setGoalStatusTopDown(+GoalList,Status)
% 
% Sets the goalStatus of all goals in GoalList and all of their children to Status
% 
setGoalStatusTopDown([],_).
setGoalStatusTopDown([G],Status) :-
    \+ subgoal(_,G),
    setGoalStatus(G,Status).
setGoalStatusTopDown([G|Gs],Status) :-
    \+ subgoal(_,G),
    setGoalStatus(G,Status),
    setGoalStatusTopDown(Gs,Status).
setGoalStatusTopDown([G],Status) :-
    findall(SubGoal,subgoal(SubGoal,G),Subgoals),
    setGoalStatusTopDown(Subgoals,Status),
    setGoalStatus(G,Status).
setGoalStatusTopDown([G|Gs],Status) :-
    findall(SubGoal,subgoal(SubGoal,G),Subgoals),
    setGoalStatusTopDown(Subgoals,Status),
    setGoalStatusTopDown(Gs,Status),
    setGoalStatus(G,Status).

% setGoalStatus(+Goal,+Status)
% 
% Sets the goalStatus of Goal to Status
% 
setGoalStatus(Goal,nofail) :-
    goalStatus(Goal,active),
    writeList(1,["No fail on",Goal,", remaining active"]).
setGoalStatus(Goal,State) :-
    retract(goalStatus(Goal,OldState)),
    assert(goalStatus(Goal,State)),
    \+ OldState = State,
    writeList(1,["Changing",Goal,"from",OldState,"to",State]).
setGoalStatus(Goal,State) :-
    goalStatus(Goal,OldState),
    OldState = State.

% setGoalStatusList(+GoalList,+Status)
% 
% Call setGoalStatus(+Goal,+Status) on all goals in GoalList
% 
setGoalStatusList([],_).
setGoalStatusList([G],Status) :-
    setGoalStatus(G,Status).
setGoalStatusList([G|Gs],Status) :-
    setGoalStatus(G,Status),
    setGoalStatusList(Gs,Status).

% getAllActiveCalculusGoals(-Goals)
% 
% Returns all calculus goals that are currently active
% 
getAllActiveDiffIntGoals(Goals) :-
    findall(DiffGoal,(inst(DiffGoal,differentialGoal), goalStatus(DiffGoal,active)),DiffGoals),
    findall(IntGoal,(inst(IntGoal,integralGoal), goalStatus(IntGoal,active)),IntGoals),
    append(DiffGoals,IntGoals,Goals).

% getAllActiveAchievableGoals(-Goals)
% 
% Return all goals that are active and have a success condition
%   
getAllActiveAchievableGoals(Goals) :-
    findall(
        Goal,
        (isa(Goal,goal), successCondition(Goal,_), goalStatus(Goal,active)),
        Goals    
    ).

% propagateAchievement
% 
% Achieves any goals that are achieved by the achievement of another goal that has been achieved
% 
propagateAchievement :-
    findall(
        G2,
        (achieves(G1,G2), goalStatus(G1,achieved), goalStatus(G2,active)),
        Goals
    ),
    setGoalStatusList(Goals,achieved).

% checkContextShifts 
% 
% Checks for context shifts and activates any new context which has been triggered.
% 
checkContextShifts :-
    enables(TriggerGoal,NewContext),
    goalStatus(TriggerGoal,achieved),
    writeList(1,["Trigger Goal", TriggerGoal, "achieved"]),
    setGoalStatusTopDown([TriggerGoal],active),
    activateNewContext(NewContext).
checkContextShifts :-
    writeList(1,["No context shifts detected"]).

% performChecks
% 
% Performs checks on differential, integral and all other goals and updates their
% goal status if it is determined that it should change. Shift contexts if any enabling
% goals have been achieved
% 
performChecks :-
    getAllActiveDiffIntGoals(DiffIntGoals),
    checkGoalList(DiffIntGoals),
    writeList(1,["Checking remaining achievable goals..."]),
    getAllActiveAchievableGoals(Goals),
    checkGoalList(Goals),
    writeList(1,["Aborting goal reasoning if any goal has failed..."]), !, % does not backtrack if any goals have failed
    noFailure,
    writeList(1,["Propogating achievement..."]), 
    propagateAchievement,
    checkContextShifts.

noFailure :-
    \+ goalStatus(_,failed).

changeCurrentTime(NewTime) :-
    retract(currentTime(_)),
    assert(currentTime(NewTime)).

% ########## EXECUTION ##########

runT1 :-
    currentTime(Time),
    writeList(1,["Current time:",Time]),
    activateCurrentContext, !, % does not backtrack if any goals have failed
    performChecks.

runT2 :-
    writeList(1,["********** NEXT TIMESTEP **********"]),
    changeCurrentTime(1445),
    currentTime(Time),
    writeList(1,["Current time:",Time]),
    performChecks.

% run
% 
% Runs two timesteps representing a goal to the goal reasoner with a state history
% and goal model followed by a subsequent call with the updated goal model from
% timestep one and an updated state history with new observations
% 
run :-
    runT1,
    runT2.
