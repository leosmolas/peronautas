%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['aux/saboteur.delp'].

rolMetas :-
    foreach(
        b(enemyPosition(Agent, _Node)),
        doNotFail(calcMeta(atacar(Agent)))
    ).



rolSetBeliefs :-
    myStatus(normal), !,
    calcTime(setEnemyPosition),
    calcTime(setEnemyDistance),
	calcTime(setAttackDifPuntos).
    
rolSetBeliefs.
    
setAttackDifPuntos :-
	myPosition(MyPosition),
	b(enemyPosition(Agent, Node)),
	(b(distancia(Node, [[attack, Agent]], _C, _E)) <- true),
	Node \= MyPosition, !,
	setDifPuntosSinMi.
	
setAttackDifPuntos.
	
setEnemyPosition :-
    myTeam(MyTeam),
    currentStep(Step),
    foreach(
        (
            team(Agent, Team),
            Team \= MyTeam,
            status(Step, Agent, normal)
        ),
        assertEnemyPosition(Step, Agent)
    ).
    
assertEnemyPosition(Step, Agent) :-
    writeln(assertEnemyPosition),
    position(Step, Agent, Position),
    write(Agent),writeln(Position),
    assert(b(enemyPosition(Agent, Position))),
    assert(b(enemyPosition(Agent, Position)) <- true).
	
assertEnemyPosition(_Step, _Agent).
    
setEnemyDistance :-
    myName(Name),
    currentStep(Step),
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 7)),
    foreach(
        b(enemyPosition(Agent, Node)),
        (
            searchPathSaboteur(Position, Node, Agent, Energy)
        )
    ),
    printFindAll('attack paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1, _))).

searchPathSaboteur(Position, FinalNode, Agent, Energy) :-
    retractall(isGoal(_)),	
    assert(isGoal(ucsNode(FinalNode, _, _, _, _))),
    singleton_heap(InitialFrontier, 0, ucsNode(Position, Energy, [], [], 0)),
    ucsAux(InitialFrontier, [], Path, Actions, PathCost, NewEnergy), 
    NewTurns is PathCost + 1,
    calcRecharge(NewEnergy, 2, [[attack, Agent]], NewActions, NewTurns, NewTurns2, RemainingEnergy1),
    append(Actions, NewActions, Plan),
    not(isFail(ucsNode(FinalNode, RemainingEnergy1, Path, Plan, NewTurns2))), !,
    assert(b(path(Position, FinalNode, [[attack, Agent]], Energy, Path, Plan, NewTurns2, RemainingEnergy1))),
    assert(b(distancia(FinalNode, [[attack, Agent]], NewTurns2, RemainingEnergy1)) <- true).
    
searchPathSaboteur(_Position, _FinalNode, _Agent, _Energy).

%------------------------------------------------------------------------%
% Actions (priority order):
%                   -attack
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%------------------------------------------------------------------------%
execDummy(Action) :- 
    action(Action).
	
%------------------------------  Attack  --------------------------------%

% action([buy, shield]):-
%     myStatus(normal),
%     myEnergy(Energy),
%     currentStep(Step),
%     Energy >= 2,
%     %0 is Step mod 10, % cada 10 turnos evaluamos si compramos shield
%     money(X),
%     X > 4, !.
    
% action([buy, sabotageDevice]):-
%     myStatus(normal),
%     currentStep(Step),
%     5 is Step mod 10, % cada 10 turnos evaluamos si compramos shield
%     money(X),
%     X > 4, !.

action([attack, Enemy]):-
    myStatus(normal),
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    status(Step, Enemy, normal),
    myName(Name),
    Enemy \= Name,
    write(1.3),nl,
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    write(1.4),nl,
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
    Cost \= unknown, 
    write(3.2),nl,
    myEnergy(Energy),
    Cost < Energy, 
    write(3.3),nl,
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    status(Step, EnemyAgent, normal),
    write(3.4),nl,
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),nl,
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

