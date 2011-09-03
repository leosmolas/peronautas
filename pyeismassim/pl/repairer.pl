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
    action(Action),
    nl.
        
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
    writeln(assertTeamMatePosition),
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
    foreach(
        b(teammatePosition(Agent, Node)),
        (

            searchPath(Position, Node, Energy, [[repair, Agent]], 2)
        )
    ),
    printFindAll('repair paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1, _))).


%------------------------------  Repair  --------------------------------%

action([repair, Ally]):-
    write(1.1),write(', '),
    myEnergy(Energy),
    Energy > 1,
    write(1.2),write(', '),
    currentStep(Step),
    myPosition(Position),
    position(Step, Ally, Position),
    write(1.3),write(', '),
    myName(Name),
    Ally \= Name,
    write(1.4),write(', '),
    myTeam(Team),
    team(Step, Ally, Team),
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

%-- First Node Goto --%

action([goto, X]) :-
    write(3.1),write(', '),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(3.2),write(', '),
    myEnergy(Energy),
    Energy >= Cost, 
    write(3.3),
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(4).

