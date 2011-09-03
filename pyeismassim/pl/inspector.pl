%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -inspect
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action)
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

%------------------------------  Attack  --------------------------------%

action([inspect]):-
    write(1.1),write(', '),
    myEnergy(Energy),
    Energy > 1,
    write(1.2),write(', '),
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    write(1.3),write(', '),
    myName(Name),
    Enemy \= Name,
    write(1.4),write(', '),
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    write(1.5),
    !.
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(2.1),write(', '),
    myEnergy(Energy),
    Energy > 0,
    write(2.2),write(', '),
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(2.3),
    !.

%-------------------------------  Goto  ---------------------------------%

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    write(3.1),write(', '),
    myPosition(Position),
    k(edge(Position, NeighborNode, Cost)), 
    write(3.2),write(', '),
    Cost \= unknown, 
    write(3.3),write(', '),
    myEnergy(Energy),
    Cost < Energy, 
    write(3.4),write(', '),
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    write(3.5),write(', '),
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    write(3.6),
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),write(', '),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    write(4.2),write(', '),
    Cost \= unknown,
    write(4.3),write(', '),
    myEnergy(Energy),
    Energy >= Cost, 
    write(4.4),
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(5).
