%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -attack
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action).
    


rolMetas.



rolSetBeliefs.

%------------------------------  Attack  --------------------------------%

action([attack, Enemy]):-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    Enemy \= Name,
    write(1.3),nl,
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    write(1.4),nl,
    !.
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(2.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(2.3),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    write(3.1),nl,
    myPosition(Position),
    k(edge(Position, NeighborNode, Cost)), 
    Cost \= unknown, 
    write(3.2),nl,
    myEnergy(Energy),
    Cost < Energy, 
    write(3.3),nl,
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    write(3.4),nl,
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),nl,
    currentStep(Step),
    myPosition(Position),
    write(4.2),nl,
    myEnergy(Energy),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(4.3),nl,
    Energy >= Cost, 
    write(4.4),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(5),nl.

