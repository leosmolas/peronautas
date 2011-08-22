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
    action(Action).
    
rolMetas.

rolSetBeliefs.

% rolMetas :-
    % foreach(
        % b(enemyPosition(Agent, _Node)),
        % calcMeta(atacar(Agent))
    % ).



% rolSetBeliefs :-
    % setEnemyPosition,
    % setEnemyDistance.
    

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
    
setEnemyDistance :-
    myName(Name),
    currentStep(Step),
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        b(enemyPosition(_Agent, Node)),
        (
			
            searchPath(Position, Node, Energy, [[attack]], 2)
        )
    ),
    printFindAll('attack paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1))).

    

    
%------------------------------  Attack  --------------------------------%

action([attack, Enemy]):-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
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

