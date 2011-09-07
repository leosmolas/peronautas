%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Repairer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -repair
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

:- ['delp/repairer.delp'].

execDummy(Action) :- 
    action(Action).
    

        
% rolMetas.



% rolSetBeliefs.


rolMetas :-
    foreach(
        b(teammatePosition(Agent, _Node)),
        doNotFail(calcMeta(reparar(Agent)))
    ).



rolSetBeliefs :-
    setTeammatePosition, !,
    setTeammateDistance.
    

injuredAgent(Agent) :-
    myTeam(MyTeam),
    currentStep(Step),
    myName(Self),
    team(Agent, MyTeam),
    Agent \= Self,
    health(Step, Agent, Health),
    maxHealth(Step, Agent, Max),
    Health < Max.

setTeammatePosition :-
    foreach(
        injuredAgent(Agent),
        assertTeammatePosition(Agent)
    ).
    
assertTeammatePosition(Agent) :-
	currentStep(Step),
    position(Step, Agent, Position), !,
	health(Step, Agent, Health), !, 
	maxHealth(Step, Agent, MaxHealth), !,
    assert(b(teammateHealthInfo(Agent, Health, MaxHealth)) <- true),
    assert(b(teammatePosition(Agent, Position))).
    
assertTeammatePosition(_Step, _Agent).
    
setTeammateDistance :-
    myName(Name),
    currentStep(Step),
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    repairCost(Cost),
    foreach(
        b(teammatePosition(Agent, Node)),
        (

            searchPath(Position, Node, Energy, [[repair, Agent]], Cost)
        )
    ),
    printFindAll('repair paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1, _))).

repairCost(2) :-
    myStatus(normal).
    
repairCost(3) :-
    myStatus(disabled).


%------------------------------  Repair  --------------------------------%

action([repair, Ally]):-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Ally, Position),
    status(Step, Ally, disabled),
    write(1.3),nl,
    myName(Name),
    Ally \= Name,
    write(1.4),nl,
    myTeam(Team),
    team(Step, Ally, Team),
    write(1.5),nl,
    !.

%    
%-------------------------------  Parry  --------------------------------%
% action([parry]) :-
%     myStatus(normal),
%     myPosition(Position),
%     myTeam(MyTeam),
%     currentStep(Step),
%     myEnergy(Energy),
%     Energy >= 2,
%     position(Step, Enemy, Position),
%     team(Enemy, EnemyTeam),
%     MyTeam \= EnemyTeam.

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

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%
    
action([goto, X]) :-
    myStatus(normal),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    currentStep(Step),
    position(Step, AllyAgent, X),
    myTeam(Team),
    team(AllyAgent, Team),
    Cost \= unknown,
    write(3.2),nl,
    myEnergy(Energy),
    Energy >= Cost, 
    write(3.3),nl,
    !.

action([goto, X]) :-
    write(3.1),nl,
    myPosition(Position),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(3.2),nl,
    myEnergy(Energy),
    Energy >= Cost, 
    write(3.3),nl,
    !.
%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(4),nl.

