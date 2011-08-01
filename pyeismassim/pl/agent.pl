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
           currentStep/1,      %
           h/1,                %
           k/1,                %
           agentTeam/2.        %

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
% k(agentNode(Agent, Step, Node))
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
    asserta( k(nodeValue(Name, Value)) ).



%------------------------------------------------------------------------------%
updateNodeTeam(Name, CurrentTeam) :-
    currentStep(Step),
    asserta( k(nodeTeam(Step, Name, CurrentTeam)) ).

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
updateEntity(Agent, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange) :-
    currentStep(Step),
    asserta( k(agentTeam(Agent,        Team))               ),
    asserta( k(agentRole(Agent,        Role))               ),
    asserta( k(agentNode(Agent,        Step, Node))         ),
    asserta( k(agentEnergy(Agent,      Step, Energy))       ),
    asserta( k(agentMaxEnergy(Agent,   Step, MaxEnergy))    ),
    asserta( k(agentHealth(Agent,      Step, Health))       ),
    asserta( k(agentMaxHealth(Agent,   Step, MaxHealth))    ),
    asserta( k(agentStrength(Agent,    Step, Strength))     ),
    asserta( k(agentVisualRange(Agent, Step, VisualRange))  ).



%------------------------------------------------------------------------------%
updateEntityTeamPosition(Agent, Team, Position) :-
    k(agentTeam(Agent, Team)),
    currentStep(Step),
    asserta( k(agentPosition(Agent, Step, Position) )).
updateEntityTeamPosition(Agent, Team, Position) :-
    currentStep(Step),
    asserta( k(agentTeam(Agent, Team) )),
    asserta( k(agentPosition(Agent, Step, Position) )).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                     Acceso                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% agent(Step, Agent, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange)

myTeam(Team) :-
    myName(Agent),
    k(agentTeam(Agent, Team)).
myPosition(Node) :-
    myName(Agent),
    currentStep(Step),
    k(agentPosition(Agent, Step, Node)).
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
getInfo(team, Step, Agent, Team) :-
    k(agentTeam(Agent, Step, Team)).
getInfo(position, Step, Agent, Position) :-
    k(agentNode(Agent, Step, Position)).
getInfo(role, Step, Agent, Role) :-
    k(agentRole(Agent, Step, Role)).
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Argumentacion                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Intenciones posibles: explore, recharge
intention(explore).

%argumentation :- 
%    intention(recharge),
%    max_energy(X),
%    energy(X),
%    retract( intention(recharge) ),
%    assert(  intention(explore)  ).
%argumentation :- 
%    last_action_result(failed),
%    retract( intention(_)        ),
%    assert(  intention(recharge) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                   Planning                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%planning :- 
%    intention(explore),
%    searchNeigh(N),
%    retract( plan(_)         ),
%    assert(  plan([goto(N)]) ).
%planning :- 
%    intention(recharge),
%    retract( plan(_)          ),
%    assert(  plan([recharge]) ).



searchNeigh(N) :-
    myName(A),
    k(position(A, Pos)),
    k(edge(Pos, N, _)).



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
    findall(
        Node2, 
        k(edge(Node1, Node2, unknown)), 
        L),
    L \= [].



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
printList([]).
printList([H | T]) :-
    write('    '),write(H),nl,
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
    printFindAll('AGENT NODE:',         k(agentNode(        _X16, _X17, _X18 ))),
    printFindAll('AGENT ENERGY:',       k(agentEnergy(      _X19, _X20, _X21 ))),
    printFindAll('AGENT MAX ENERGY:',   k(agentMaxEnergy(   _X22, _X23, _X24 ))),
    printFindAll('AGENT HEALTH:',       k(agentHealth(      _X25, _X26, _X27 ))),
    printFindAll('AGENT MAX HEALTH:',   k(agentMaxHealth(   _X28, _X29, _X30 ))),
    printFindAll('AGENT STRENGTH:',     k(agentStrength(    _X31, _X32, _X33 ))),
    printFindAll('AGENT VISUAL RANGE:', k(agentVisualRange( _X34, _X35, _X36 ))).



%------------------------------------------------------------------------------%
close_output :-
    current_output(S),
    close(S).
