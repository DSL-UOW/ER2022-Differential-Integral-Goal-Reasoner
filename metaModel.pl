
:- ensure_loaded(goalModel).

% ********** METAMETAMODEL **********

% Closure rules for inheritance

topcl(A) :-
    class(A),
    \+ subcl(A,_).

% subcl(A,A) :- class(A). % was problematic to subgoal(?Goal1,?Goal2)
subcl(A,B) :- sub(A,X), subcl(X,B).
subcl(A,B) :- sub(A,B), topcl(B).

% isa(O,C) :- inst(O,C), !. % cut doesn't allow us to find all.
isa(O,C) :- inst(O,C).
isa(O,C) :- subcl(C1,C), inst(O,C1). 

inst(A,B) :-
    inst(A,B,_).

desc(Obj,Desc) :-
    inst(Obj,_,Desc).
desc(Obj,Desc) :-
    inst(Obj,_),
    \+ inst(Obj,_,_),
    Desc = " ".

% ********** METAMODEL **********

class(goal).

sub(differentialGoal,goal).
sub(integralGoal,goal).
sub(probe,goal).
sub(effector,goal).

