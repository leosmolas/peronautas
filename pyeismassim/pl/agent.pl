:- dynamic 
           % Private
           myName/1,           %
           myTeam/1,           %
           energy/3,           %
           maxEnergy/3,        %
           health/3,           %
           maxHealth/3,        %
           status/3,                
           lastAction/1,       %
           lastActionResult/1, %
           strength/1,         %
           visRange/1,         %
           zoneScore/1,        %
           lastStepScore/1,    %
           money/1,            %
           score/1,            %
           % Public
           phase/1,
           currentStep/1,      %
           h/1,                %
           k/1,                %
           b/1,
           agentTeam/2,        %
           exploredNode/1,
           visibleNode/1,
           notVisible/1,
           notExplored/1,
           explored/1,
           plan/1,
           intention/1,
           countTurns/1,
           verbose/0,
           myVisionRange/1.

:- [ 'graph/map.pl'
   , 'graph/agents.pl'
   , 'graph/nodes.pl'
   , 'utils.pl'
   , 'beliefs.pl'
   , 'communication.pl'
   , 'ypserver/ypagent.pl'
   ].
    
% :- use_module(library(time)).
    
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

% agent(Step, Name, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange)
%
% |
% v
%
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
   retractall( myName(_) ),
   asserta(    myName(X) ).



%------------------------------------------------------------------------------%
updateStep(X) :-
    retractall( currentStep(_) ),
    asserta(    currentStep(X) ).



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
    % El valor en la percepcion es unknown, y el nodo ya es conocido, luego no
    % hay que hacer nada. 
    k(nodeValue(Name, _)), 
    !.
updateNodeValue(Name, Value) :-
    % El valor en la percepcion es distinto de unknown, luego se aserta
    % independientemente si es conocido o no.
    Value \= unknown,
    !,
    retractall( k(nodeValue(Name,     _)) ),
    asserta(    k(nodeValue(Name, Value)) ).
updateNodeValue(Name, Value) :-
    % El nodo es desconocido.
    asserta( notExplored(Name) ),
    asserta( notVisible(Name) ),
    asserta( k(nodeValue(Name, Value)) ).



%------------------------------------------------------------------------------%
updateNodeTeam(Name, CurrentTeam) :-
    currentStep(Step),
    asserta( k(nodeTeam(Step, Name, CurrentTeam)) ).
    % assertOnce( h(nodeTeam(Name, none)) ). % no tiene sentido despues limpiar todo



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
    % Si el arco ya estaba en la kb (con costo unknown o el real).
    k(edge(Node1, Node2, Cost)), 
    !.
updateEdge(Node1, Node2, unknown) :-
    % Si ya estaba en la kb con su costo final.
    k(edge(Node1, Node2, Cost)),
    Cost \= unknown, 
    !.
updateEdge(Node1, Node2, Cost) :-
    % Si conociamos el arco pero no su valor (es decir, cuando hacemos un survey del arco).
    % En este caso podriamos preguntar si Cost \= unknown.
    k(edge(Node1, Node2, unknown)),
    !,
    deleteEdge(Node1, Node2, unknown),
    insertEdge(Node1, Node2, Cost).
updateEdge(Node1, Node2, Cost) :- 
    % Si es la primera vez que vemos el arco.
    insertEdge(Node1, Node2, Cost).



%------------------------------------------------------------------------------%
updateEntity(Agent, Team, Position, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange, Status) :-
    currentStep(Step),
    assertOnce( k(agentTeam(Agent,        Team))               ),
    assertOnce( k(agentRole(Agent,        Role))               ),
    asserta( k(agentPosition(Agent,    Step, Position))     ),
    asserta( k(agentEnergy(Agent,      Step, Energy))       ),
    asserta( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    asserta( k(agentHealth(Agent,      Step, Health))       ),
    asserta( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    asserta( k(agentStrength(Agent,    Step, Strength))     ),
    asserta( k(agentVisualRange(Agent, Step, VisualRange))  ),
    asserta( k(agentStatus(Agent,      Step, Status))       ).



%------------------------------------------------------------------------------%
updateEntity(Agent, Team, Position, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange) :-
    currentStep(Step),
    assertOnce( k(agentTeam(Agent,        Team))               ),
    assertOnce( k(agentRole(Agent,        Role))               ),
    asserta( k(agentPosition(Agent,    Step, Position))     ),
    asserta( k(agentEnergy(Agent,      Step, Energy))       ),
    asserta( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    asserta( k(agentHealth(Agent,      Step, Health))       ),
    asserta( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    asserta( k(agentStrength(Agent,    Step, Strength))     ),
    asserta( k(agentVisualRange(Agent, Step, VisualRange))  ).


%------------------------------------------------------------------------------%
updateTeammateEntity(Agent, Team, Position, Health, MaxHealth, VisualRange) :-
    k(agentTeam(Agent, Team)), !,
    currentStep(Step),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentHealth(      Agent, Step, Health) )),
    asserta( k(agentMaxHealth(   Agent, Step, MaxHealth) )),
    asserta( k(agentVisualRange( Agent, Step, VisualRange ) )).
updateTeammateEntity(Agent, Team, Position, Health, MaxHealth, VisualRange) :-
    currentStep(Step),
    asserta( k(agentTeam(        Agent, Team) )),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentHealth(      Agent, Step, Health) )),
    asserta( k(agentMaxHealth(   Agent, Step, MaxHealth) )),
    asserta( k(agentVisualRange( Agent, Step, VisualRange ) )).

updateEntityTeamPosition(Agent, Team, Position, Status) :-
    k(agentTeam(Agent, Team)), !,
    currentStep(Step),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentStatus(      Agent, Step, Status) )).
updateEntityTeamPosition(Agent, Team, Position, Status) :-
    currentStep(Step),
    asserta( k(agentTeam(        Agent, Team) )),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentStatus(      Agent, Step, Status) )).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Phases                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initial phase for all agents.
phase(exploracion).



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

    write('The phase has changed!'),nl,
    retractall(phase(_)),
    asserta(phase(remaining)).
% Don't you ever fail me!
updatePhase.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Acceso                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printK :-
    currentStep(Step),
    foreach(
        k(nodeTeam(Step, V, T)),
        (write('Node: '), write(V), write(', Team: '), write(T), nl)
    ).

% agent(Step, Agent, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange)

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

team(Agent, Team) :-
    lastKnownInfo(agentTeam, _Step, Agent, Team).
role(Agent, Role) :-
    lastKnownInfo(agentRole, _Step, Agent, Role).
    

%------------------------------------------------------------------------------%
% lastKnownInfo(+Field, -Step, +Agent, -Value) :-
lastKnownInfo(agentTeam, _Step, Agent, Value) :-
    k(agentTeam(Agent, Value)).
    
lastKnownInfo(agentRole, _Step, Agent, Value) :-
    k(agentRole(Agent, Value)), !.
    
lastKnownInfo(agentRole, _Step, _Agent, unknown).
    
    
% El step viene instanciado, por lo que no tiene sentido ponerse a buscar para atras
lastKnownInfo(Field, Step, Agent, Value) :-
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



%------------------------------------------------------------------------------%
% Condicion de corte, exito.
lastKnownInfo1(Field, Step, Step, Agent, Value) :-
    getInfo(Field, Step, Agent, Value).
    % !.

% Condicion de corte, sin exito.
lastKnownInfo1(_Field, 0, 0, _Agent, unknown) :-
    !, fail.

% Recursion.
lastKnownInfo1(Field, CurrentStep, Step, Agent, Position) :-
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


myRechargeEnergy(Recharge) :-
    myStatus(disabled), !,
    myMaxEnergy(MaxEnergy),
    Recharge is round(MaxEnergy * 0.1).
    
myRechargeEnergy(Recharge) :-
    myMaxEnergy(MaxEnergy),
    Recharge is round(MaxEnergy * 0.2). % TODO: testear si esto es correcto -> DONE: es correcto
    
rechargeEnergy(Step, Agent, Recharge) :-
    status(Step, Agent, disabled), !,
    maxEnergy(Step, Agent, MaxEnergy),
    Recharge is round(MaxEnergy * 0.1).
    
rechargeEnergy(Step, Agent, Recharge) :-
    maxEnergy(Step, Agent, MaxEnergy),
    Recharge is round(MaxEnergy * 0.2).

checkLastAction :-
    lastActionResult(failed), !.

checkLastAction :-
    plan([]), !.
    
checkLastAction :-
    retract(plan([_Action | Actions])),
    assert(plan(Actions)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      Run                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% TimeLimit in seconds
run(TimeLimit, Action) :-
    write('time limit: '),writeln(TimeLimit),
    catch( % try
        call_with_time_limit(TimeLimit, run2(Action)), 
        % run2(Action),
        E, 
        ( % except :
            write('ERROR!!!!!!!!!!!!: '),
            writeln(E),
            writeln('Executing dummy now:'),
            retractall(b(_)),
            retractall(b(_) <- true),
            execDummy(Action)
        )
    ), !.
    
run(_TimeLimit, _Action) :-
    retractall(intention(_)),
    assert(intention(quedarse)),
    planning(quedarse(unknown)).
    
    
run2(Action) :-
    currentStep(Step),
    nl, nl, nl, write('Current Step: '), writeln(Step),
    checkLastAction,
    plan([]),
    !,
    % currentStep(Step),
    % myName(Name),
    % concat('logs/', Name, S2),
    % concat(S2, '-', S3),
    % concat(S3, Step, S0),
    % concat(S0, '.pl', File),
    % writeln(File),
    % saveMap(File),
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(8)),
    calcTime(argumentation(Meta)), !,
    write('Meta: '), writeln(Meta),
    calcTime(planning(Meta)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.

run2(Action) :- 
    intention(Meta),
    writeln(Meta),
    cutCondition(Meta), !, 
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(6)),
    calcTime(argumentation(MetaNueva)), !,
    write('Meta Nueva: '), writeln(MetaNueva),
    calcTime(planning(MetaNueva)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
        
run2(Action) :- 
    calcTime(setExploredAndVisible),
    calcTime(setNodesAtDistance(6)),
    intention(Meta),
    calcTime(replanning(Meta)), !,
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    toogleOffVisibleNodes.  

    
run2(Action) :- 
    retractall(countTurns(_)),
    assert(countTurns(0)),
    calcTime(argumentation(Meta)), !,
    write('Meta: '), writeln(Meta),
    calcTime(planning(Meta)),
    exec(Action),
    writeln(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
    
plan([]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Argumentacion                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

argumentation(Meta) :-
    calcTime(setBeliefs),
    calcTime(meta(MetaConValue)),
    retractall(intention(_)),
    MetaConValue =.. [M, _Value | Args],
    Meta =.. [M | Args],
    assert(intention(MetaConValue)).

calcTime(Exec) :-
    verbose, !,
    write('<predicate name="'),write(Exec), writeln('">'),nl,
    get_time(_Before),
    call(Exec),
    write('</predicate>'),nl.

calcTime(Exec) :- call(Exec).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Planning                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


planning(explorar(Node)) :-
    assertPlan(Node, [[survey]]).

planning(probear(Node)) :-
    assertPlan(Node, [[probe]]).
    
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
    
planning(auxilio(Repairer)) :-
    currentStep(Step),
    position(Step, Repairer, Node),
    assertPlan(Node, []).

planning(quedarse(_Node)) :-
    myEnergy(Energy),
    myMaxEnergy(Max),
    Energy < Max, !,
    retractall(plan(_)),
    assert(plan([[recharge]])).
    
planning(quedarse(_Node)) :-
    retractall(plan(_)),
    assert(plan([[skip]])).

assertPlan(_Node, _FinalActions) :-
    myPosition(InitialPosition),
    not(b(path(_, _, _, _, _, _, _, _))), !,
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).
  
assertPlan(Node, _FinalActions) :-
    myPosition(InitialPosition),
    
    b(path(InitialPosition, Node, _, _, _, [], _, _)), !,
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

% Calcula un nuevo plan todos los turnos. No genera problemas porque sobra el tiempo.
% Genera el nuevo plan, y llama a planning para que lo ejecute.

replanning(explorar(Node)) :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_, _)),
    searchPath(Position, Node, Energy, [[survey]], 1),
    planning(explorar(Node)).
    
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
    position(Step, Agent, EnemyPosition),
    retractall(isFail(_, _)),
    searchPath(Position, EnemyPosition, Energy, [[repair, Agent]], 2),
    planning(reparar(Agent)).
    
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
    
%si algun camino no se encontro, se planea quedarse
replanning(_) :- 
    retractall(intention(_)),
    assert(intention(quedarse(InitialPosition))),
    planning(quedarse(InitialPosition)).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Condicion de corte                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    
cutCondition(_) :-
    countTurns(V), % este predicado se lleva para controlar que las metas que persiguen agentes no se pasen de mambo    
    V2 is V + 1, 
    retractall(countTurns(_)),
    assert(countTurns(V2)),
    fail.
    
cutCondition(Meta) :-
    Meta \= atacar(_),
    myPosition(MyPos),
    currentStep(Step),
    position(Step, Agent, MyPos),
    status(Step, Agent, normal),
    myTeam(MyTeam),
    team(Agent, Team),
    Team \= MyTeam,
    role(Agent, saboteur),
    writeln('hay un enemigo saboteador en mi nodo').
    
cutCondition(Meta) :-
    Meta \= reparar(_),
    Meta \= auxilio(_),
    myStatus(disabled),
    writeln('me mataron').

cutCondition(explorar(Node)) :-
    explored(Node),
    not(hasAtLeastOneUnsurveyedEdge(Node)), 
    writeln('el nodo ya fue explorado').
    
cutCondition(probe(Node)) :- 
    nodeValue(Node, Value),
    Value \= unknown,
    writeln('el nodo ya fue probeado').

cutCondition(atacar(_Agent)) :-
    countTurns(5),
    writeln('pasaron 5 turnos y no le pegue').
    
cutCondition(atacar(Agent)) :-
    currentStep(Step),
    status(Step, Agent, disabled),
    writeln('moli a palos al agente enemigo').
    
cutCondition(atacar(Agent)) :-
    myTeam(MyTeam),
    currentStep(Step),
    position(Step, Agent, Node),
    position(Step, Agent2, Node),
    team(Agent2, Team),
    MyTeam \= Team,
    Agent \= Agent2,
    ( 
        role(Agent, unknown);
        role(Agent, saboteur)
    ),
    ( 
        role(Agent2, unknown);
        role(Agent2, saboteur)
    ),
    writeln('estoy por ser atacado por dos saboteadores').

cutCondition(reparar(_Agent)) :-
    countTurns(5),
    writeln('pase 5 turnos sin reparar a mi amigo').
    
cutCondition(reparar(Agent)) :-
    currentStep(Step),
    health(Step, Agent, Value),
    maxHealth(Step, Agent, Value),
    writeln('ya repare a mi amigo').
    
cutCondition(reparar(Agent)) :-
    myTeam(MyTeam),
    currentStep(Step),
    status(Step, Agent, normal),
    status(Step, Agent2, disabled),
    Agent \= Agent2,
    team(Agent2, MyTeam),
    writeln('hay otro agente que necesita mas ayuda').
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Exec                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
exec(Action) :-
    plan([Action | _Actions]).
    
exec([skip]) :-
    writeln('aca no deberia haber llegado'),
    plan([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Auxiliary                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%-----------------------------------------------------------------------%
assertOnce(X) :- call(X), !.
assertOnce(X) :- asserta(X).



%-----------------------------------------------------------------------%
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
    k(edge(Node, _Node2, unknown)), !.


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
    dumpMap,
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
    verbose,
    findall(WhatToFind, WhatToFind, L),
    % sort(L, SL),
    write(Title),
    nl,
    printList(L).

printFindAll(_Title, _WhatToFind).

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
    printFindAll('AGENT NAME:',         myName(                         _Name )),
    printFindAll('AGENT TEAM:',         k(agentTeam(        _X12, _X13       ))),
    printFindAll('AGENT ROLE:',         k(agentRole(        _X14, _X15       ))),
    printFindAll('AGENT NODE:',         k(agentPosition(    _X16, _X17, _X18 ))),
    printFindAll('AGENT ENERGY:',       k(agentEnergy(      _X19, _X20, _X21 ))),
    printFindAll('AGENT MAX ENERGY:',   k(agentMaxEnergy(   _X22, _X23, _X24 ))),
    printFindAll('AGENT HEALTH:',       k(agentHealth(      _X25, _X26, _X27 ))),
    printFindAll('AGENT MAX HEALTH:',   k(agentMaxHealth(   _X28, _X29, _X30 ))),
    printFindAll('AGENT STRENGTH:',     k(agentStrength(    _X31, _X32, _X33 ))),
    printFindAll('AGENT STATUS:',       k(agentStatus(      _,    _,    _ ))),
    printFindAll('AGENT VISUAL RANGE:', k(agentVisualRange( _X34, _X35, _X36 ))).



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

