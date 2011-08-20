:- dynamic 
           % Private
           myName/1,           %
           myTeam/1,           %
           energy/3,           %
           maxEnergy/3,        %
           health/3,           %
           maxHealth/3,        %
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
           myVisionRange/1.

:- [graph/map, 
    utils, 
    beliefs].
    
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

% ASSERTAR Y BORRAR ESTO
team(a).
team(b).

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



assertOnce(X) :- call(X), !.
assertOnce(X) :- asserta(X).



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
updateEntity(Agent, Team, Position, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange) :-
    currentStep(Step),
    writeln('updateEntity'),
    asserta( k(agentTeam(Agent,        Team))               ),
    asserta( k(agentRole(Agent,        Role))               ),
    asserta( k(agentPosition(Agent,    Step, Position))     ),
    asserta( k(agentEnergy(Agent,      Step, Energy))       ),
    asserta( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    asserta( k(agentHealth(Agent,      Step, Health))       ),
    asserta( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    asserta( k(agentStrength(Agent,    Step, Strength))     ),
    asserta( k(agentVisualRange(Agent, Step, VisualRange))  ).



%------------------------------------------------------------------------------%
updateTeammateEntity(Agent, Team, Position, VisualRange) :-
    k(agentTeam(Agent, Team)), !,
    currentStep(Step),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentVisualRange( Agent, Step, VisualRange ) )).
updateTeammateEntity(Agent, Team, Position, VisualRange) :-
    currentStep(Step),
    asserta( k(agentTeam(        Agent, Team) )),
    asserta( k(agentPosition(    Agent, Step, Position    ) )),
    asserta( k(agentVisualRange( Agent, Step, VisualRange ) )).

updateEntityTeamPosition(Agent, Team, Position) :-
    k(agentTeam(Agent, Team)), !,
    currentStep(Step),
    asserta( k(agentPosition(    Agent, Step, Position    ) )).
updateEntityTeamPosition(Agent, Team, Position) :-
    currentStep(Step),
    asserta( k(agentTeam(        Agent, Team) )),
    asserta( k(agentPosition(    Agent, Step, Position    ) )).



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



% X(?Step, +Agent, -Value)
team(Step, Agent, Team) :-
    lastKnownInfo(team, Step, Agent, Team).
    
position(Step, Agent, Position) :-
    lastKnownInfo(position, Step, Agent, Position).
role(Step, Agent, Role) :-
    lastKnownInfo(role, Step, Agent, Role).
energy(Step, Agent, Energy) :-
    lastKnownInfo(energy, Step, Agent, Energy).
maxEnergy(Step, Agent, MaxEnergy) :-
    lastKnownInfo(maxEnergy, Step, Agent, MaxEnergy).
health(Step, Agent, Health) :- 
    lastKnownInfo(health, Step, Agent, Health).
maxHealth(Step, Agent, MaxHealth) :-
    lastKnownInfo(maxHealth, Step, Agent, MaxHealth).
strength(Step, Agent, Strength) :-
    lastKnownInfo(strength, Step, Agent, Strength).
visualRange(Step, Agent, VisualRange) :-
    lastKnownInfo(visualRange, Step, Agent, VisualRange).

team(Agent, Team) :-
    lastKnownInfo(team, _Step, Agent, Team).
role(Agent, Role) :-
    lastKnownInfo(role, _Step, Agent, Role).
    

%------------------------------------------------------------------------------%
% lastKnownInfo(+Field, -Step, +Agent, -Position) :-
lastKnownInfo(Field, Step, Agent, Value) :-
    currentStep(InitialStep),
    lastKnownInfo1(Field, InitialStep, Step, Agent, Value).



%------------------------------------------------------------------------------%
% Condicion de corte, exito.
lastKnownInfo1(Field, Step, Step, Agent, Value) :-
    getInfo(Field, Step, Agent, Value),
    !.

% Condicion de corte, sin exito.
lastKnownInfo1(_Field, 0, 0, _Agent, unknown) :-
    !.

% Recursion.
lastKnownInfo1(Field, CurrentStep, Step, Agent, Position) :-
    NextStep is CurrentStep - 1,
    lastKnownInfo(Field, NextStep, Step, Agent, Position),
    !.

%------------------------------------------------------------------------------%
getInfo(team, _Step, Agent, Team) :-
    k(agentTeam(Agent, Team)).
getInfo(position, Step, Agent, Position) :-
    k(agentPosition(Agent, Step, Position)).
getInfo(role, _Step, Agent, Role) :-
    k(agentRole(Agent, Role)).
getInfo(energy, Step, Agent, Energy) :-
    k(agentEnergy(Agent, Step, Energy)).
getInfo(maxEnergy, Step, Agent, MaxEnergy) :-
    k(agentMaxEnergy(Agent, Step, MaxEnergy)).
getInfo(health, Step, Agent, Health) :-
    k(agentHealth(Agent, Step, Health)).
getInfo(maxHealth, Step, Agent, MaxHealth) :-
    k(agentMaxHealth(Agent, Step, MaxHealth)).
getInfo(strength, Step, Agent, Strength) :-
    k(agentStrength(Step, Agent, Strength)).
getInfo(visualRange, Step, Agent, VisualRange) :-
    k(agentVisualRange(Agent, Step, VisualRange)).


myRechargeEnergy(Recharge) :-
    myMaxEnergy(MaxEnergy),
    Recharge is round(MaxEnergy * 0.2). % TODO: testear si esto es correcto -> DONE: es correcto
    
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
	
run(Action) :-

    currentStep(Step),
    nl, nl, nl, write('Current Step: '), writeln(Step),
    checkLastAction,
    plan([]),
    % dumpKB, 
    !,
   
    calcTime('setExploredAndVisible',setExploredAndVisible),

	
    calcTime('argumentation',argumentation(Meta)),
    write('Meta: '), writeln(Meta),
    calcTime('planning', planning(Meta)),
    exec(Action),
    retractall(b(_)),
    retractall(b(_) <- true),
    toogleOffVisibleNodes.
    
run(Action) :-
	
    setExploredAndVisible,
    exec(Action),
    toogleOffVisibleNodes.
    
plan([]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Argumentacion                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Intenciones posibles: explore, recharge
% intention(explore).

% argumentation :- 
%    intention(recharge),
%    max_energy(X),
%    energy(X),
%    retract( intention(recharge) ),
%    assert(  intention(explore)  ).
% argumentation :- 
%    last_action_result(failed),
%    retract( intention(_)        ),
%    assert(  intention(recharge) ).


argumentation(Meta) :-

    calcTime('setBeliefs', setBeliefs),

    calcTime('meta', meta(Meta)),
    retractall(intention(_)),
    assert(intention(Meta)).

calcTime(Message, Exec) :-
    write('<predicate name="'),write(Message), writeln('">'),
    get_time(Before),
    call(Exec),
    get_time(After),
    Time is (After - Before) * 1000,
    write('<time value="'),write(Time), writeln('"/>'),
    writeln('</predicate>').
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Planning                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% planning :- 
%    intention(explore),
%    searchNeigh(N),
%    retract( plan(_)         ),
%    assert(  plan([goto(N)]) ).
% planning :- 
%    intention(recharge),
%    retract( plan(_)          ),
%    assert(  plan([recharge]) ).

planning(explorar(Node)) :-
    assertPlan(Node, [[survey]]).

planning(probear(Node)) :-
    assertPlan(Node, [[probe]]).
    
planning(aumento(Node)) :-
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
    
assertPlan(Node, FinalActions) :-
    myPosition(InitialPosition),
    % myEnergy(Energy),
    b(path(InitialPosition, Node, FinalActions, _, _, Actions, _, _)),
    retract(plan(_)),
    assert(plan(Actions)).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Exec                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
exec(Action) :-
    plan([Action | _Actions]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Auxiliary                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
%makeAdjacencyList(Graph) :-
%    findall(
%        Node,
%        k(nodeValue(Node, Cost)),
%        Nodes
%    ),
%    findall(
%        edge(Node, Node2, Value),
%        k(edge(Node1, Node2, Value)),
%        Edges
%    ),
%    makeAdjacencyList(Nodes, Edges, Graph).
%
%makeAdjacencyList(Nodes, Edges, Graph1) :-
%    addNodes(Nodes, Graph0),
%    addEdges(Edges, Graph1).

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
    % printFindAll('% b', b(_)),
    printFindAll('% myName', myName(_)),
    printFindAll('% visible', visibleNode(_)),
    printFindAll('% not visible', notVisible(_)),
    printFindAll('% explored', explored(_)),
    printFindAll('% not explored', notExplored(_)),
    printFindAll('% inRange', inRange(_)).
    % printFindAll('% k', k(_)),

%------------------------------------------------------------------------------%
printList([]).
printList([H | T]) :-
    write('    '), write(H), write('.'), nl,
    printList(T).



%------------------------------------------------------------------------------%

printFindAll(Title, WhatToFind) :-
    findall(WhatToFind, WhatToFind, L),
    sort(L, SL),
    write(Title),
    nl,
    printList(SL).



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

