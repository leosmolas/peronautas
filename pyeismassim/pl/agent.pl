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
           agentTeam/2,        %
           myVisionRange/1.


:- ['pl/graph/map.pl', 'pl/utils.pl'].
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Percept Processing                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%------------------------------------------------------------------------------%
updateMyName(X) :- 
   retractall( myName(_) ),
   assertz(    myName(X) ).



%------------------------------------------------------------------------------%
updateStep(X) :-
    retractall( currentStep(_) ),
    assertz(    currentStep(X) ).



%------------------------------------------------------------------------------%
updateMaxEnergyDisabled(X) :- 
    retractall( maxEnergyDisabled(_) ),
    assertz(    maxEnergyDisabled(X) ).



%------------------------------------------------------------------------------%
updateLastAction(X) :- 
    retractall( lastAction(_) ),
    assertz(    lastAction(X) ).



%------------------------------------------------------------------------------%
updateLastActionResult(X) :- 
    retractall( lastActionResult(_) ),
    assertz(    lastActionResult(X) ).



%------------------------------------------------------------------------------%
updateMoney(X) :- 
    retractall( money(_) ), 
    assertz(    money(X) ).



%------------------------------------------------------------------------------%
updateScore(X) :- 
    retractall( score(_) ), 
    assertz(    score(X) ).



%------------------------------------------------------------------------------%
updateZoneScore(X) :- 
    retractall( zoneScore(_) ), 
    assertz(    zoneScore(X) ).



%------------------------------------------------------------------------------%
updateLastStepScore(X) :- 
    retractall( lastStepScore(_) ), 
    assertz(    lastStepScore(X) ).



%------------------------------------------------------------------------------%
% agent(Step, Name, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange)
% TODO:
% Asi como esta, no esta bueno, porque depende de que desde python se actualize
% primero la informacion menos valiosa (las entidades visibles) antes que la
% informacion mas valiosa (las entidades inspeccionadas).
% El uso de currentStep/1 tambien implica que se espera que se actualize el
% valor del turno actual antes de actualizar cualquier otra cosa, solo por
% ahorrar un parametro.
% Name
% Team
% Node
% Role
% Energy
% MaxEnergy
% Health
% MaxHealth
% Strength
% VisualRange
updateEntity(A, T, N, R, E, ME, H, MH, S, V) :-
    currentStep(Step),
    retractall( k(agent(Step, A, _, _, _, _,  _, _,  _, _, _)) ),
    assertz(    k(agent(Step, A, T, N, R, E, ME, H, MH, S, V)) ).



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
    assertz(    k(nodeValue(Name, Value)) ).
updateNodeValue(Name, Value) :-
    % El nodo es desconocido.
    assertz( k(nodeValue(Name, Value)) ).



%------------------------------------------------------------------------------%
updateNodeTeam(Name, CurrentTeam) :-
    currentStep(Step),
    assertz( k(nodeTeam(Step, Name, CurrentTeam)) ).

%------------------------------------------------------------------------------%
insertEdge(Node1, Node2, Cost) :-
    assertz( k(edge(Node1, Node2, Cost)) ),
    assertz( k(edge(Node2, Node1, Cost)) ).



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

markExploredNodes :-
    currentStep(Step),
    write('MARCA, CRISTO '), nl,
    myName(Name),
    write('MARCA2, CRISTO '), nl,
    position(Step, Name, CurrentPosition),
    write('MARCA3, CRISTO '), nl,
    assert(isGoal(_, Cost) :- myVisionRange(Range), Cost < Range),
    write(' Marking... '), nl,
    foreach(
            bfs([bfsNode(CurrentPosition, [CurrentPosition], 0)], [], Node),
            (write(' Marking node as explored: '),write(Node),nl, assert(explored(Node)))
           ).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                     Acceso                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% agent(Step, Agent, Team, Node, Role, Energy, MaxEnergy, Health, MaxHealth, Strength, VisualRange)
team(Step, Agent, Team) :-
    k(agent(Step, Agent,  Team, _Node, _Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange)).
position(Step, Agent, Node) :-
    k(agent(Step, Agent, _Team,  Node, _Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange)).
role(Step, Agent, Role) :-
    k(agent(Step, Agent, _Team, _Node,  Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange)).
energy(Step, Agent, Energy) :-
    k(agent(Step, Agent, _Team, _Node, _Role,  Energy, _MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange)).
maxEnergy(Step, Agent, MaxEnergy) :-
    k(agent(Step, Agent, _Team, _Node, _Role, _Energy,  MaxEnergy, _Health, _MaxHealth, _Strength, _VisualRange)).
health(Step, Agent, Health) :- 
    k(agent(Step, Agent, _Team, _Node, _Role, _Energy, _MaxEnergy,  Health, _MaxHealth, _Strength, _VisualRange)).
maxHealth(Step, Agent, MaxHealth) :-
    k(agent(Step, Agent, _Team, _Node, _Role, _Energy, _MaxEnergy, _Health,  MaxHealth, _Strength, _VisualRange)).
strength(Step, Agent, Strength) :-
    k(agent(Step, Agent, _Team, _Node, _Role, _Energy, _MaxEnergy, _Health, _MaxHealth,  Strength, _VisualRange)).
visualRange(Step, Agent, VisualRange) :-
    k(agent(Step, Agent, _Team, _Node, _Role, _Energy, _MaxEnergy, _Health, _MaxHealth, _Strength,  VisualRange)).

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
    
    printFindAll('NODE VALUES:', k(nodeValue(_X1, X2))),
    printFindAll('NODE TEAMS:',  k(nodeTeam(_X1, _X2, _X3))),
    printFindAll('EDGES:',       k(edge(_X1, _X2, _X3))),
    printFindAll('POSITIONS:',   k(position(_X1, _X2, _X3))),
    printFindAll('AGENTS:',      k(agent(_X1, _X2, _X3, _X4, _X5, _X6, _X7, _X8, _X9, _X10, _X11))).



%------------------------------------------------------------------------------%
close_output :-
    current_output(S),
    close(S).
