:- dynamic
           % Private
           myName/1,
           myTeam/1,
           energy/3,
           maxEnergy/3,
           health/3,
           maxHealth/3,
           status/3,
           lastAction/1,
           lastActionResult/1,
           strength/1,
           visRange/1,
           zoneScore/1,
           lastStepScore/1,
           money/1,
           score/1,
           % Public
           phase/1,
           currentStep/1,
           firstPerceivedStep/1,
           h/1,
           k/1,
           b/1,
           agentTeam/2,
           exploredNode/1,
           visibleNode/1,
           notVisible/1,
           notExplored/1,
           explored/1,
           plan/1,
           ultimaCompra/1,
           intention/1,
           countTurns/1,
           saveMap/0,
           verbose/0,
           muertos/2,
           buyCount/2,
           myVisionRange/1.

:- [graph/map
   , utils
   , beliefs
   ].

:- use_module(library(time)).
:- use_module(library(heaps)).

% :- set_prolog_flag(debug_on_error, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Knowledge and Beliefs                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Action representation:
%   Actions are represented by lists.
%   The 1st element of the list is an atom representing the type of action.
%   The rest of the list elements are terms representing the action parameters,
%   such as node names, integers, etc.
%       [parry]
%       [probe]
%       [survey]
%       [inspect]
%       [recharge]
%       [skip]
%       [goto,   Vertex]
%       [attack, Agent]
%       [repair, Agent]
%       [buy,    Item]

% Known information, coming from the percepts, is represented as arguments to
% the predicte k/1.

% Hypothetical information, used for planning, is represented as arguments to
% the predicate h/1.

% Beliefs are represented as arguments to the predicate b/1.

% Possible arguments to k/1, h/1, and b/1 :
%
% nodeValue(Name, Value)
%     Name is the vertex name.
%     Value is 'unknown' if the node value is unknown, otherwise an integer
%     representing its value.

% nodeTeam(Step, Name, Team)
%     Name is the vertex name.
%     Team is the owning team, or the atom 'none'.

% edge(Node1, Node2, Cost)
%     Node1 and Node2 are the vertex names.
%     Cost is the edge cost, and if unsurveyed will be unknown.

% k(agentTeam(Agent, Team))
% k(agentRole(Agent, Role))
% k(agentPosition(Agent, Step, Position))
% k(agentEnergy(Agent, Step, Energy))
% k(agentMaxEnergy(Agent, Step, MaxEnergy))
% k(agentHealth(Agent, Step, Health))
% k(agentMaxHealth(Agent, Step, MaxHealth))
% k(agentStrength(Agent, Step, Strength))
% k(agentVisualRange(Agent, Step, VisualRange))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Percept Processing                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------------------------------%
updateMyName(X) :-
    var(X),
    !,
    fail.

updateMyName(X) :-
   retractall( myName(_) ),
   asserta(    myName(X) ).

%------------------------------------------------------------------------------%
updateStep(X) :-
    firstPerceivedStep(_),
    !,
    retract( currentStep(_) ),
    asserta( currentStep(X) ).

updateStep(X) :-
    assert(firstPerceivedStep(X)),
    asserta( currentStep(X) ).

%------------------------------------------------------------------------------%
updateMaxEnergyDisabled(X) :-
    retractall( maxEnergyDisabled(_) ),
    asserta(    maxEnergyDisabled(X) ).

%------------------------------------------------------------------------------%
updateLastAction(X) :-
    retractall( lastAction(_) ),
    asserta(    lastAction(X) ).

%------------------------------------------------------------------------------%
updateLastActionResult(X) :-
    retractall( lastActionResult(_) ),
    asserta(    lastActionResult(X) ).

%------------------------------------------------------------------------------%
updateMoney(X) :-
    retractall( money(_) ),
    asserta(    money(X) ).

%------------------------------------------------------------------------------%
updateScore(X) :-
    retractall( score(_) ),
    asserta(    score(X) ).

%------------------------------------------------------------------------------%
updateZoneScore(X) :-
    retractall( zoneScore(_) ),
    asserta(    zoneScore(X) ).

%------------------------------------------------------------------------------%
updateLastStepScore(X) :-
    retractall( lastStepScore(_) ),
    asserta(    lastStepScore(X) ).

%------------------------------------------------------------------------------%
updateNodeValue(Name, unknown) :-
    % The value in the percept is unknown, and the vertex has already been
    % probed, so there is nothing to do.
    k(nodeValue(Name, _)),
    !.
updateNodeValue(Name, Value) :-
    % The value in the percept is not unknown, therefore it is asserted, whether
    % it as already known or not.
    Value \= unknown,
    !,
    retractall( k(nodeValue(Name,     _)) ),
    asserta(    k(nodeValue(Name, Value)) ).
updateNodeValue(Name, Value) :-
    % The vertex's value is unknown.
    asserta( notExplored(Name)         ),
    asserta( notVisible(Name)          ),
    asserta( k(nodeValue(Name, Value)) ).

%------------------------------------------------------------------------------%
updateNodeTeam(Name, CurrentTeam) :-
    currentStep(Step),
    retractall( k(nodeTeam(_Step, Name, _CurrentTeam)) ),
    asserta(    k(nodeTeam( Step, Name,  CurrentTeam)) ).

%------------------------------------------------------------------------------%
insertEdge(Node1, Node2, Cost) :-
    asserta( k(edge(Node1, Node2, Cost)) ),
    asserta( k(edge(Node2, Node1, Cost)) ).

%------------------------------------------------------------------------------%
deleteEdge(Node1, Node2, Cost) :-
    retract( k(edge(Node1, Node2, Cost)) ),
    retract( k(edge(Node2, Node1, Cost)) ).

%------------------------------------------------------------------------------%
% Succeeds if the edge must be updated.
updateEdge(Node1, Node2, Cost) :-
    % The edge is already in the agent's KB (either with unknown or real value)
    k(edge(Node1, Node2, Cost)),
    !.
updateEdge(Node1, Node2, unknown) :-
    % The edge is already in the KB with it's true cost.
    k(edge(Node1, Node2, Cost)),
    Cost \= unknown,
    !.
updateEdge(Node1, Node2, Cost) :-
    % The edge was known but not it's value.
    % In this case we could cask whether Cost \= unknown.
    k(edge(Node1, Node2, unknown)),
    !,
    deleteEdge(Node1, Node2, unknown),
    insertEdge(Node1, Node2, Cost).
updateEdge(Node1, Node2, Cost) :-
    % It is the first time we encounter this edge.
    insertEdge(Node1, Node2, Cost).

%------------------------------------------------------------------------------%

updateEntity(Agent, Team, _Position, Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange, _Status) :-
    (
        var(Agent)
    ;
        var(Team)
    ;
        var(Role)
    ),
    !.
updateEntity(Agent, Team, Position, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange, Status) :-
    currentStep(Step),
    retractall( k(agentPosition(Agent,    _, _Position))       ),
    retractall( k(agentEnergy(Agent,      _, _Energy))         ),
    retractall( k(agentMaxEnergy(Agent,   _, _MaxEnergy))      ),
    retractall( k(agentHealth(Agent,      _, _Health))         ),
    retractall( k(agentMaxHealth(Agent,   _, _MaxHealth))      ),
    retractall( k(agentStrength(Agent,    _, _Strength))       ),
    retractall( k(agentVisualRange(Agent, _, _VisualRange))    ),
    retractall( k(agentStatus(Agent,      _, _Status))         ),
    assertOnce( k(agentTeam(Agent,        Team))               ),
    assertOnce( k(agentRole(Agent,        Role))               ),
    assertOnce( k(agentPosition(Agent,    Step, Position))     ),
    assertOnce( k(agentEnergy(Agent,      Step, Energy))       ),
    assertOnce( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    assertOnce( k(agentHealth(Agent,      Step, Health))       ),
    assertOnce( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    assertOnce( k(agentStrength(Agent,    Step, Strength))     ),
    assertOnce( k(agentVisualRange(Agent, Step, VisualRange))  ),
    assertOnce( k(agentStatus(Agent,      Step, Status))       ).

%------------------------------------------------------------------------------%
updateEntity(Agent, Team, _Position, Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange) :-
    (
        var(Agent)
    ;
        var(Team)
    ;
        var(Role)
    ),
    !.

updateEntity(Agent, Team, Position, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange) :-
    currentStep(Step),
    retractall( k(agentPosition(Agent,    _, _Position))       ),
    retractall( k(agentEnergy(Agent,      _, _Energy))         ),
    retractall( k(agentMaxEnergy(Agent,   _, _MaxEnergy))      ),
    retractall( k(agentHealth(Agent,      _, _Health))         ),
    retractall( k(agentMaxHealth(Agent,   _, _MaxHealth))      ),
    retractall( k(agentStrength(Agent,    _, _Strength))       ),
    retractall( k(agentVisualRange(Agent, _, _VisualRange))    ),
    assertOnce( k(agentTeam(Agent,        Team))               ),
    assertOnce( k(agentRole(Agent,        Role))               ),
    assertOnce( k(agentPosition(Agent,    Step, Position))     ),
    assertOnce( k(agentEnergy(Agent,      Step, Energy))       ),
    assertOnce( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    assertOnce( k(agentHealth(Agent,      Step, Health))       ),
    assertOnce( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    assertOnce( k(agentStrength(Agent,    Step, Strength))     ),
    assertOnce( k(agentVisualRange(Agent, Step, VisualRange))  ).

%------------------------------------------------------------------------------%
updateTeammateEntity(Agent, Team, _Position, _Health, _MaxHealth, _VisualRange) :-
    (
        var(Agent)
    ;
        var(Team)
    ),
    !.

updateTeammateEntity(Agent, Team, Position, Health, MaxHealth, VisualRange) :-
    k(agentTeam(Agent, Team)),
    !,
    currentStep(Step),
    retractall( k(agentPosition(    Agent, _, _Position        ) )),
    retractall( k(agentHealth(      Agent, _, _Health          ) )),
    retractall( k(agentMaxHealth(   Agent, _, _MaxHealth       ) )),
    retractall( k(agentVisualRange( Agent, _, _VisualRange     ) )),
    assertOnce( k(agentPosition(    Agent, Step, Position      ) )),
    assertOnce( k(agentHealth(      Agent, Step, Health        ) )),
    assertOnce( k(agentMaxHealth(   Agent, Step, MaxHealth     ) )),
    assertOnce( k(agentVisualRange( Agent, Step, VisualRange   ) )).

updateTeammateEntity(Agent, Team, Position, Health, MaxHealth, VisualRange) :-
    currentStep(Step),
    retractall( k(agentTeam(        Agent, _Team               ) )),
    retractall( k(agentPosition(    Agent, _, _Position        ) )),
    retractall( k(agentHealth(      Agent, _, _Health          ) )),
    retractall( k(agentMaxHealth(   Agent, _, _MaxHealth       ) )),
    retractall( k(agentVisualRange( Agent, _, _VisualRange     ) )),
    assertOnce( k(agentTeam(        Agent, Team                ) )),
    assertOnce( k(agentPosition(    Agent, Step, Position      ) )),
    assertOnce( k(agentHealth(      Agent, Step, Health        ) )),
    assertOnce( k(agentMaxHealth(   Agent, Step, MaxHealth     ) )),
    assertOnce( k(agentVisualRange( Agent, Step, VisualRange   ) )).

updateEntityTeamPosition(Agent, Team, _Position, _Status) :-
    (
        var(Agent)
    ;
        var(Team)
    ),
    !.

updateEntityTeamPosition(Agent, Team, Position, Status) :-
    k(agentTeam(Agent, Team)),
    !,
    currentStep(Step),
    retractall( k(agentPosition(    Agent, _, _Position     ) )),
    retractall( k(agentStatus(      Agent, _, _Status       ) )),
    assertOnce( k(agentPosition(    Agent, Step, Position   ) )),
    assertOnce( k(agentStatus(      Agent, Step, Status     ) )).

updateEntityTeamPosition(Agent, Team, Position, Status) :-
    currentStep(Step),
    retractall( k(agentTeam(        Agent, _Team            ) )),
    retractall( k(agentPosition(    Agent, _, _Position     ) )),
    retractall( k(agentStatus(      Agent, _, _Status       ) )),
    assertOnce( k(agentTeam(        Agent, Team             ) )),
    assertOnce( k(agentPosition(    Agent, Step, Position   ) )),
    assertOnce( k(agentStatus(      Agent, Step, Status     ) )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Phases                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initial phase for all agents.
phase(exploration).

% Every turn, we check whether the phase has changed.
updatePhase :-
    phase(exploration),

    % Phase change condition: after a given number of turns.
    currentStep(Step),
    Step > 5,

    % Phase change condition: no more nodes to explore.
    % TODO cambiar esto para que use el codigo para inferir si quedan nodos
    % jamas vistos, no sin sodear.
    %findall(
    %    Node,
    %    k(nodeValue(Node, unknown)),
    %    UnexploredNodes
    %),
    %UnexploresNodes = [],

    write('The phase has changed!'), nl,
    retractall(phase(_)),
    asserta(phase(remaining)).

% Don't you ever fail me!
updatePhase.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Access                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------------------------------%
% Simple access to an agent's own attributes.
myTeam(Team) :-
    myName(Agent),
    k(agentTeam(Agent, Team)).
myPosition(Position) :-
    myName(Agent),
    currentStep(Step),
    k(agentPosition(Agent, Step, Position)).
myRole(Role) :-
    myName(Agent),
    k(agentRole(Agent, Role)).
myEnergy(Energy) :-
    myName(Agent),
    currentStep(Step),
    k(agentEnergy(Agent, Step, Energy)).
myMaxEnergy(MaxEnergy) :-
    myName(Agent),
    currentStep(Step),
    k(agentMaxEnergy(Agent, Step, MaxEnergy)).
myHealth(Health) :-
    myName(Agent),
    currentStep(Step),
    k(agentHealth(Agent, Step, Health)).
myMaxHealth(MaxHealth) :-
    myName(Agent),
    currentStep(Step),
    k(agentMaxHealth(Agent, Step, MaxHealth)).
myStrength(Strength) :-
    myName(Agent),
    currentStep(Step),
    k(agentStrength(Agent, Step, Strength)).
myVisualRange(VisualRange) :-
    myName(Agent),
    currentStep(Step),
    k(agentVisualRange(Agent, Step, VisualRange)).
myStatus(Status) :-
    myName(Agent),
    currentStep(Step),
    k(agentStatus(Agent, Step, Status)).

%------------------------------------------------------------------------------%
% X(?Step, +Agent, -Value)
team(Step, Agent, Team) :-
    lastKnownInfo(agentTeam, Step, Agent, Team).
position(Step, Agent, Position) :-
    lastKnownInfo(agentPosition, Step, Agent, Position).
role(Step, Agent, Role) :-
    lastKnownInfo(agentRole, Step, Agent, Role).
energy(Step, Agent, Energy) :-
    lastKnownInfo(agentEnergy, Step, Agent, Energy).
maxEnergy(Step, Agent, MaxEnergy) :-
    lastKnownInfo(agentMaxEnergy, Step, Agent, MaxEnergy).
health(Step, Agent, Health) :-
    lastKnownInfo(agentHealth, Step, Agent, Health).
maxHealth(Step, Agent, MaxHealth) :-
    lastKnownInfo(agentMaxHealth, Step, Agent, MaxHealth).
strength(Step, Agent, Strength) :-
    lastKnownInfo(agentStrength, Step, Agent, Strength).
visualRange(Step, Agent, VisualRange) :-
    lastKnownInfo(agentVisualRange, Step, Agent, VisualRange).
status(Step, Agent, Status) :-
    lastKnownInfo(agentStatus, Step, Agent, Status).

%------------------------------------------------------------------------------%
% Even simpler access to unchanging information.
team(Agent, Team) :-
    lastKnownInfo(agentTeam, _Step, Agent, Team).
role(Agent, Role) :-
    lastKnownInfo(agentRole, _Step, Agent, Role).

%------------------------------------------------------------------------------%
% Hardcoded assumption regarding the association between an agent's name and
% it's given role.
roleNumber(1, explorer).
roleNumber(2, explorer).
roleNumber(3, repairer).
roleNumber(4, repairer).
roleNumber(5, saboteur).
roleNumber(6, saboteur).
roleNumber(7, sentinel).
roleNumber(8, sentinel).
roleNumber(9, inspector).
roleNumber(0, inspector).

%------------------------------------------------------------------------------%
% lastKnownInfo(+Field, -Step, +Agent, -Value) :-
lastKnownInfo(agentTeam, _Step, Agent, Value) :-
    k(agentTeam(Agent, Value)).
lastKnownInfo(agentRole, _Step, Agent, Value) :-
    k(agentRole(Agent, Value)), !.
lastKnownInfo(agentRole, _Step, Agent, Value) :-
    k(agentTeam(Agent, _T)),
    atom_chars(Agent, String),
    append(_X, [Y], String),
    roleNumber(Y, Value), !.
lastKnownInfo(agentRole, _Step, _Agent, unknown).
lastKnownInfo(Field, Step, Agent, Value) :-
    % The Step argument is instantiatied, so it makes no sense to look into the
    % past.
    nonvar(Step),
    Field \= team,
    Field \= role,
    A =.. [Field, Agent, Step, Value],
    Q =.. [k, A],
    call(Q).
% lastKnownInfo(Field, Step, Agent, Value) :-
    % var(Step),
    % currentStep(InitialStep),
    % lastKnownInfo1(Field, InitialStep, Step, Agent, Value).
lastKnownInfo1(Field, Step, Step, Agent, Value) :-
    % Stop condition, succcessful.
    getInfo(Field, Step, Agent, Value).
lastKnownInfo1(_Field, 0, 0, _Agent, unknown) :-
    % Stop condution, failed.
    !, 
    fail.
lastKnownInfo1(Field, CurrentStep, Step, Agent, Position) :-
    % Recursion.
    NextStep is CurrentStep - 1,
    lastKnownInfo1(Field, NextStep, Step, Agent, Position),
    !.

%------------------------------------------------------------------------------%
getInfo(agentTeam, _Step, Agent, Team) :-
    k(agentTeam(Agent, Team)).
getInfo(agentPosition, Step, Agent, Position) :-
    k(agentPosition(Agent, Step, Position)).
getInfo(agentRole, _Step, Agent, Role) :-
    k(agentRole(Agent, Role)).
getInfo(agentEnergy, Step, Agent, Energy) :-
    k(agentEnergy(Agent, Step, Energy)).
getInfo(agentMaxEnergy, Step, Agent, MaxEnergy) :-
    k(agentMaxEnergy(Agent, Step, MaxEnergy)).
getInfo(agentHealth, Step, Agent, Health) :-
    k(agentHealth(Agent, Step, Health)).
getInfo(agentMaxHealth, Step, Agent, MaxHealth) :-
    k(agentMaxHealth(Agent, Step, MaxHealth)).
getInfo(agentStrength, Step, Agent, Strength) :-
    k(agentStrength(Step, Agent, Strength)).
getInfo(agentVisualRange, Step, Agent, VisualRange) :-
    k(agentVisualRange(Agent, Step, VisualRange)).

%------------------------------------------------------------------------------%
myRechargeEnergy(Recharge) :-
    myStatus(disabled), !,
    myMaxEnergy(MaxEnergy),
    Recharge is round(MaxEnergy * 0.3).
myRechargeEnergy(Recharge) :-
    myMaxEnergy(MaxEnergy),
    Recharge is round(MaxEnergy * 0.5).

%------------------------------------------------------------------------------%
rechargeEnergy(Step, Agent, Recharge) :-
    status(Step, Agent, disabled), !,
    maxEnergy(Step, Agent, MaxEnergy),
    Recharge is round(MaxEnergy * 0.3).
rechargeEnergy(Step, Agent, Recharge) :-
    maxEnergy(Step, Agent, MaxEnergy),
    Recharge is round(MaxEnergy * 0.5).

%------------------------------------------------------------------------------%
checkLastAction :-
    lastActionResult(failed),
    !.
checkLastAction :-
    plan([]),
    !.
checkLastAction :-
    retract(plan([_Action | Actions])),
    assert(plan(Actions)),
    !.

%------------------------------------------------------------------------------%
buyCount(sensor,         0).
buyCount(shield,         0).
buyCount(sabotageDevice, 0).

%------------------------------------------------------------------------------%
setBuyCount :-
    lastAction(buy),
    lastActionResult(successful),
    ultimaCompra(Item),
    retract(buyCount(Item, CurrentCount)),
    NewCount is CurrentCount + 1,
    assert(buyCount(Item, NewCount)),
    !.
setBuyCount :-
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      Run                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------------------------------%
runInitialization:-
    currentStep(Step),
    nl, nl, nl, write('Current Step: '), writeln(Step),
    checkLife,
    calcTime(setMuertos),
    checkLastAction,
    calcTime(setBuyCount),
    !.

%------------------------------------------------------------------------------%
% TimeLimit is in seconds
run(TimeLimit, Action) :-
    write('Time limit: '),writeln(TimeLimit),
    SetBeliefsTimeLimit is TimeLimit - 0.2,
    catch( % try
        call_with_time_limit(TimeLimit, run2(SetBeliefsTimeLimit, Action)),
        % run2(Action),
        E,
        ( % except :
            write('Error caught in run: '),
            writeln(E),
            writeln('Executing dummy now:'),
            retractall(b(_)),
            retractall(b(_) <- true),
            execDummy(Action)
        )
    ),
    !.
run(_TimeLimit, Action) :-
    retractall(b(_)),
    retractall(b(_) <- true),
    retractall(intention(_)),
    assert(intention(quedarse(_))),
    writeln('Run: stop iteration'),
    planning(quedarse(_)),
    exec(Action).

%------------------------------------------------------------------------------%
run2(SetBeliefsTimeLimit, Action) :-
    runInitialization,
    plan([]),
    !,
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(7)),
    !,
    calcTime(argumentation(SetBeliefsTimeLimit, Meta)),
    write('Goal: '), writeln(Meta),
    calcTime(planning(Meta)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
run2(SetBeliefsTimeLimit, Action) :-
    intention(Meta),
    writeln(Meta),
    cutCondition(Meta),
    !,
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(7)),
    !,
    calcTime(argumentation(SetBeliefsTimeLimit, MetaNueva)), 
    !,
    write('New goal: '), writeln(MetaNueva),
    calcTime(planning(MetaNueva)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
run2(_SetBeliefsTimeLimit, Action) :-
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(7)),
    !,
    intention(Meta),
    calcTime(replanning(Meta)),
    !,
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
run2(SetBeliefsTimeLimit, Action) :-
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(argumentation(SetBeliefsTimeLimit, Meta)),
    !,
    write('Goal: '), writeln(Meta),
    calcTime(planning(Meta)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Argumentacion                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

argumentation(SetBeliefsTimeLimit, Meta) :-
    catch(
        call_with_time_limit(SetBeliefsTimeLimit, calcTime(setBeliefs)),
        time_limit_exceeded,
        writeln('\tWarning: set beliefs limit time exceeded, delping now')
    ),
    calcTime(meta(Meta)),
    retractall(intention(_)),
    assert(intention(Meta)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Planning                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plan([]).

planning(explorar(Node)) :-
    assertPlan(Node, [[survey]]).
planning(probear(Node)) :-
    assertPlan(Node, [[probe]]).
planning(reagruparse) :-
    b(pathReagruparse([])),
    !,
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).
planning(reagruparse) :-
    b(pathReagruparse(Actions)),
    retract(plan(_)),
    assert(plan(Actions)).
planning(comprar(Item)) :-
    retractall(ultimaCompra(_)),
    assert(ultimaCompra(Item)),
    retract(plan(_)),
    assert(plan([[buy, Item]])).
planning(atacar(Agent)) :-
    currentStep(Step),
    position(Step, Agent, Node),
    assertPlan(Node, [[attack, Agent]]).
planning(reparar(Agent)) :-
    currentStep(Step),
    position(Step, Agent, Node),
    assertPlan(Node, [[repair, Agent]]).
planning(aumento(Node)) :-
    assertPlan(Node, []).
planning(expansion(Node)) :-
    assertPlan(Node, []).
planning(defensaPropia(MyPos)) :-
    myPosition(MyPos), !,
    retractall(plan(_)),
    assert(plan([[parry]])).
planning(defensaPropia(Node)) :-
    assertPlan(Node, []).
planning(auxilio(Repairer)) :-
    currentStep(Step),
    position(Step, Repairer, Node),
    assertPlan(Node, []).
planning(quedarse(_Node)) :-
    myEnergy(Energy),
    myMaxEnergy(Max),
    Energy < Max,
    !,
    retractall(plan(_)),
    assert(plan([[recharge]])).
planning(quedarse(_Node)) :-
    retractall(plan(_)),
    assert(plan([[skip]])).
assertPlan(_Node, _FinalActions) :-
    myPosition(InitialPosition),
    not(b(path(_, _, _, _, _, _, _, _))),
    !,
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).
assertPlan(Node, FinalActions) :-
    myPosition(InitialPosition),
    b(path(InitialPosition, Node, FinalActions, _, _, [], _, _)),
    !,
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).
assertPlan(Node, FinalActions) :-
    myPosition(InitialPosition),
    b(path(InitialPosition, Node, FinalActions, _, _, Actions, _, _)),
    retract(plan(_)),
    assert(plan(Actions)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Replanning                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculates a new plan every turn.
% This is not a problem because there is now plenty of time for this.
% Once it calculates the new plan, it calls planning to execute it.
replanning(explorar(Node)) :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_, _)),
    searchPath(Position, Node, Energy, [[survey]], 1),
    planning(explorar(Node)).
replanning(reagruparse) :-
    assertReagruparseGoal,
    setPathReagruparse,
    planning(reagruparse).
replanning(atacar(Agent)) :-
    myPosition(Position),
    myEnergy(Energy),
    currentStep(Step),
    position(Step, Agent, EnemyPosition),
    retractall(isFail(_, _)),
    searchPathSaboteur(Position, EnemyPosition, Agent, Energy),
    planning(atacar(Agent)).
replanning(reparar(Agent)) :-
    myPosition(Position),
    myEnergy(Energy),
    currentStep(Step),
    position(Step, Agent, Position2),
    retractall(isFail(_, _)),
    searchPath(Position, Position2, Energy, [[repair, Agent]], 2),
    planning(reparar(Agent)).
replanning(auxilio(Agent)) :-
    myPosition(Position),
    myEnergy(Energy),
    currentStep(Step),
    position(Step, Agent, Position2),
    retractall(isFail(_, _)),
    searchPath(Position, Position2, Energy, [], 0),
    planning(auxilio(Agent)).
replanning(probear(Node)) :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_, _)),
    searchPath(Position, Node, Energy, [[probe]], 1),
    planning(probear(Node)).
replanning(aumento(Node)) :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_, _)),
    searchPath(Position, Node, Energy, [], 0),
    planning(aumento(Node)).
replanning(expansion(Node)) :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_, _)),
    searchPath(Position, Node, Energy, [], 0),
    planning(expansion(Node)).
replanning(atacar(Agent)) :-
    myPosition(Position),
    myEnergy(Energy),
    currentStep(Step),
    position(Step, Agent, EnemyPosition),
    retractall(isFail(_, _)),
    searchPathSaboteur(Position, EnemyPosition, Agent, Energy),
    planning(atacar(Agent)).
replanning(_) :-
    % If some path was not found, plan to stay put.
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Cut Conditions                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate makes sure that the persecution goals are in effect a limited
% number of turns.
cutCondition(_) :-
    countTurns(V),
    V2 is V + 1,
    retractall(countTurns(_)),
    assert(countTurns(V2)),
    fail.
cutCondition(_) :-
    countTurns(10).
cutCondition(Meta) :-
    Meta \= atacar(_),
    myPosition(MyPos),
    currentStep(Step),
    position(Step, Agent, MyPos),
    status(Step, Agent, normal),
    myTeam(MyTeam),
    team(Agent, Team),
    Team \= MyTeam,
    role(Agent, saboteur).
cutCondition(Meta) :-
    Meta \= atacar(_),
    mePegaron.
cutCondition(Meta) :-
    Meta \= reparar(_),
    Meta \= auxilio(_),
    myStatus(disabled).
cutCondition(explorar(Node)) :-
    explored(Node),
    not(hasAtLeastOneUnsurveyedEdge(Node)).
cutCondition(comprar(_)).
cutCondition(quedarse(_)).
cutCondition(probe(Node)) :-
    nodeValue(Node, Value),
    Value \= unknown.
cutCondition(aumento(_Node)) :-
    currentStep(Step),
    myName(Agent),
    myTeam(MyTeam),
    agenteEnZona(Step, Agent, MyTeam).
cutCondition(defensaPropia(_)):-
    countTurns(1),
    !.
cutCondition(reagruparse):-
    countTurns(3).
cutCondition(reagruparse):-
    % myPosition(MyPos),
    currentStep(Step),
    myTeam(Team),
    myName(Agent),
    agenteEnZona(Step, Agent, Team).
cutCondition(atacar(_Agent)) :-
    countTurns(5).
cutCondition(atacar(Agent)) :-
    currentStep(Step),
    status(Step, Agent, disabled).
cutCondition(auxilio(_Agent)) :-
    countTurns(5).
cutCondition(auxilio(_Agent)) :-
    myMaxHealth(Health),
    myHealth(Health).
cutCondition(atacar(Agent)) :-
    myTeam(MyTeam),
    currentStep(Step),
    position(Step, Agent, Node),
    position(Step, Agent2, Node),
    team(Agent2, Team),
    MyTeam \= Team,
    Agent \= Agent2,
    (
        role(Agent, unknown)
    ;
        role(Agent, saboteur)
    ),
    (
        role(Agent2, unknown)
    ;
        role(Agent2, saboteur)
    ).
cutCondition(reparar(_Agent)) :-
    countTurns(5).
cutCondition(reparar(Agent)) :-
    currentStep(Step),
    health(Step, Agent, Value),
    maxHealth(Step, Agent, Value).
cutCondition(reparar(Agent)) :-
    myTeam(MyTeam),
    currentStep(Step),
    status(Step, Agent, normal),
    status(Step, Agent2, disabled),
    Agent \= Agent2,
    team(Agent2, MyTeam).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Exec                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec(Action) :-
    plan([Action | _Actions]).

exec([skip]) :-
    plan([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Auxiliary                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------------------------------%
assertOnce(X) :- call(X), !.
assertOnce(X) :- asserta(X).

%------------------------------------------------------------------------------%
calcTime(Exec) :-
    verbose,
    !,
    write('<predicate name="'),write(Exec), writeln('">'),
    get_time(Before),
    call(Exec),
    get_time(After),
    Time is (After - Before) * 1000,
    write('\t<time value="'), write(Time), writeln('"/>'),
    writeln('</predicate>').
calcTime(Exec) :- 
    call(Exec).

%------------------------------------------------------------------------------%
reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.
reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    myEnergy(Energy),
    Energy >= Cost,
    !.
reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).

%------------------------------------------------------------------------------%
% Succeeds when Node has not been surveyed.
% Un nodo no ha sido surveyed cuando tenes al menos un arco que parte de ese
% nodo, del cual no conoces el costo.
hasAtLeastOneUnsurveyedEdge(Node1) :-
    k(nodeValue(Node1, _V)),
    hasAtLeastOneUnsurveyedEdgeAux(Node1).
hasAtLeastOneUnsurveyedEdgeAux(Node) :-
    k(edge(Node, _Node2, unknown)),
    !.

%------------------------------------------------------------------------------%
redirect_output(Filename) :-
    write('Prolog redirecting output to: '),write(Filename),nl,
    open(Filename, write, S),
    set_output(S).

%------------------------------------------------------------------------------%
% saveMap(+Filename)
% Guarda en Filename en forma de un source de prolog. Terminado todo, vuelve el output a su Stream original
saveMap(Filename) :-
    current_output(Current),
    open(Filename, write, S),
    set_output(S),
    write('% '), writeln(Filename),
    assert(saveMap),
    dumpMap,
    retract(saveMap),
    set_output(Current),
    close(S).

dumpMap :-
    printFindAll('% step', currentStep(_)),
    printFindAll('% k', k(_)),
    printFindAll('% myName', myName(_)),
    printFindAll('% visible', visibleNode(_)),
    printFindAll('% not visible', notVisible(_)),
    printFindAll('% explored', explored(_)),
    printFindAll('% not explored', notExplored(_)),
    printFindAll('% inRange', inRange(_)).

%------------------------------------------------------------------------------%
printList([]).
printList([H | T]) :-
    write('    '), write(H), write('.'), nl,
    printList(T).

%------------------------------------------------------------------------------%

printFindAll(Title, WhatToFind) :-
    (
        verbose
    ;
        saveMap
    ),
    findall(WhatToFind, WhatToFind, L),
    write(Title),
    nl,
    printList(L).
printFindAll(_Title, _WhatToFind).

%------------------------------------------------------------------------------%
saveKB(Append) :-
    myName(Name),
    concat('logs/', Name, S2),
    concat(S2, Append, S3),
    concat(S3, '.pl', File),
    writeln(File),
    saveMap(File).
saveKB(_Append).

%------------------------------------------------------------------------------%
dumpKB :-
    nl,
    write('KB DUMP:'),
    nl,
    nl,
    printFindAll('NODE VALUES:',        k(nodeValue(        _X1,  _X2        ))),
    printFindAll('NODE TEAMS:',         k(nodeTeam(         _X3,  _X4,  _X5  ))),
    printFindAll('EDGES:',              k(edge(             _X6,  _X7,  _X8  ))),
    printFindAll('POSITIONS:',          k(position(         _X9,  _X10, _X11 ))),
    printFindAll('AGENT TEAM:',         k(agentTeam(        _X12, _X13       ))),
    printFindAll('AGENT ROLE:',         k(agentRole(        _X14, _X15       ))),
    printFindAll('AGENT NODE:',         k(agentPosition(    _X16, _X17, _X18 ))),
    printFindAll('AGENT ENERGY:',       k(agentEnergy(      _X19, _X20, _X21 ))),
    printFindAll('AGENT MAX ENERGY:',   k(agentMaxEnergy(   _X22, _X23, _X24 ))),
    printFindAll('AGENT HEALTH:',       k(agentHealth(      _X25, _X26, _X27 ))),
    printFindAll('AGENT MAX HEALTH:',   k(agentMaxHealth(   _X28, _X29, _X30 ))),
    printFindAll('AGENT STRENGTH:',     k(agentStrength(    _X31, _X32, _X33 ))),
    printFindAll('AGENT STATUS:',       k(agentStatus(      _,    _,    _    ))),
    printFindAll('AGENT VISUAL RANGE:', k(agentVisualRange( _X34, _X35, _X36 ))).

%------------------------------------------------------------------------------%
printK :-
    currentStep(Step),
    foreach(
        k(nodeTeam(Step, V, T)),
        (write('Node: '), write(V), write(', Team: '), write(T), nl)
    ).

%------------------------------------------------------------------------------%
printHNodeTeams(Title) :-
    write(Title), nl,
    foreach(
            h(nodeTeam(Node, Team)),
            (write('Node: '), write(Node), write(' : '), write(Team), nl)
           ),nl.

%------------------------------------------------------------------------------%
printAgentHPositions :-
    currentStep(Step),
    foreach(h(position(Step, T, A)),
            (write('Agent: '), write(T), write(' at '), write(A), nl)
        ).

%------------------------------------------------------------------------------%
close_output :-
    current_output(S),
    close(S).

