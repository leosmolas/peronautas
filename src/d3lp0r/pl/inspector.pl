%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Inspector                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rolMetas.

rolSetBeliefs.

%-----------------------------------------------------------------------%

%Actions (priority order):
%                   -inspect
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    write(1),nl,
    action(Action).

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

%------------------------------  Inspect  --------------------------------%

action([inspect]):-
    myStatus(normal),
    lastAction(LastAction),
    LastAction \= inspect,
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    status(Step, Enemy, normal),
    write(1.3),nl,
    myName(Name),
    Enemy \= Name,
    write(1.4),nl,
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    write(1.5),nl,
    !.
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    myStatus(normal),
    write(2.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(2.3),nl,
    !.

%-----------------------------  Keep zone  ------------------------------%
action([recharge]) :-
    myStatus(normal),
    zoneScore(X),
    writeln('Keep calm and keep the zone! :D'),
    X > 40, !.

%-------------------------------  Goto  ---------------------------------%

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    myStatus(normal),
    write(3.1),nl,
    myPosition(Position),
    k(edge(Position, NeighborNode, Cost)), 
    write(3.2),nl,
    Cost \= unknown, 
    write(3.3),nl,
    myEnergy(Energy),
    Cost < Energy, 
    write(3.4),nl,
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    write(3.5),nl,
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    write(3.6),nl,
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),nl,
    myPosition(Position),
    k(edge(Position, X, Cost)),
    write(4.2),nl,
    Cost \= unknown,
    write(4.3),nl,
    myEnergy(Energy),
    Energy >= Cost, 
    write(4.4),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(5),nl.
