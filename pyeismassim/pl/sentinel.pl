%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Sentinel                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

execDummy(Action) :- 
    action(Action),
    nl.


        
rolMetas.



rolSetBeliefs.
    
%-----------------------------------------------------------------------%

reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.

reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    myName(Name),
    currentStep(Step),
    energy(Step, Name, Energy),
    Energy >= Cost,
    !.

reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(1.1),write(', '),
    myEnergy(Energy),
    Energy > 0,
    write(1.2),write(', '),
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(1.3),
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%

action([goto, X]) :-
    write(2.1),write(', '),
    position(Step, Name, Position),
    k(edge(Position, X, Cost)),
    write(2.2),write(', '),
    Cost \= unknown,
    write(2.3),write(', '),
    energy(Step, Name, Energy),
    Energy >= Cost, 
    write(2.4),
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(3).

