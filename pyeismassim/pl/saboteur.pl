%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['delp/saboteur.delp'].

% Actions (priority order):
%                   -attack
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action),
    nl.
    
% rolMetas.

% rolSetBeliefs.

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
    assert(b(enemyPosition(Agent, Position))),
    assert(b(enemyPosition(Agent, Position)) <- true).
	
assertEnemyPosition(_Step, _Agent).
    
setEnemyDistance :-
    myName(Name),
    currentStep(Step),
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
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
    singleton_heap(InitialFrontier, ucsNode(Position, Energy, [], [], 0), 0),
    ucs(InitialFrontier, [], Path, Actions, PathCost, NewEnergy), 
    NewTurns is PathCost + 1,
    calcRecharge(NewEnergy, 2, [[attack, Agent]], NewActions, NewTurns, NewTurns2, RemainingEnergy1),
    append(Actions, NewActions, Plan),
    not(isFail(ucsNode(FinalNode, RemainingEnergy1, Path, Plan, NewTurns2))), !,
    assert(b(path(Position, FinalNode, [[attack, Agent]], Energy, Path, Plan, NewTurns2, RemainingEnergy1))),
    assert(b(distancia(FinalNode, [[attack, Agent]], NewTurns2, RemainingEnergy1)) <- true).
    
searchPathSaboteur(_Position, _FinalNode, _Agent, _Energy).

%------------------------------  Attack  --------------------------------%

action([attack, Enemy]):-
    write(1.1),write(', '),
    myEnergy(Energy),
    Energy > 1,
    write(1.2),write(', '),
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    myName(Name),
    Enemy \= Name,
    write(1.3),write(', '),
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    write(1.4),
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
    Cost \= unknown, 
    write(3.2),write(', '),
    myEnergy(Energy),
    Cost < Energy, 
    write(3.3),write(', '),
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    write(3.4),write(', '),
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    write(3.5),
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),write(', '),
    myPosition(Position),
    write(4.2),write(', '),
    myEnergy(Energy),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(4.3),write(', '),
    Energy >= Cost, 
    write(4.4),
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(5).

