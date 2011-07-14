:- dynamic 
           kposition/2,        %
           currentStep/1,      %
           % Private
           myName/1,           %
           myTeam/1,           %
           energy/1,           %
           maxEnergy/1,        %
           health/1,           %
           maxHealth/1,        %
           lastAction/1,       %
           lastActionResult/1, %
           strength/1,         %
           visRange/1,         %
           zoneScore/1,        %
           lastStepScore/1,    %
           money/1,            %
           score/1,            %
           % Public
           h/1,                %
           k/1,                %
           agentTeam/2.        %

:- ['pl/graph/map.pl'].

% Representation of actions:
%   Actions are represented by lists. 
%   The 1st element of the list is an atom representing the type of action.
%   This may be one of: 
%   The following elements are terms representing the action parameters, such as
%   node names, integers, etc.

% Representation of nodes:
%
% Known Information:
%   k(nodeValue(Name, Value))
%       Value es 'unknown' si no sabemos cuanto vale el nodo. 
%       Cuando no conocemos el valor de un nodo, simplemente actualizamos su owner.
%   k(nodeTeam(Name, Team))
%   k(edge(Node1, Node2, Cost))

% Hypothetical Information:
%   h(nodeTeam(Name, Team))

% Beliefs:
%   b(...)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Beliefs                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

myTeam(d3lp0r).

% lastAction/1
%   1st argument is a term representing the agent's last action.

% Indicates the known position of an agent.
% kposition/2
%   1st argument is the name of the agent that is located at that position.
%   2nd argument is the vertex name.

% hposition/2
%   1st argument is...
%   2nd argument is...

% myName/1
%   1st argument is an atom representing the agent's name.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Percept Processing                               %
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
updateMyName(X) :- 
   retractall( myName(OldName)         ),
   retractall( hposition(OldName, Op1) ),
   retractall( kposition(OldName, Op2) ),
   assertz(    myName(X)               ),
   assertz(    hposition(X, Op1)       ),
   assertz(    kposition(X, Op2)       ).



%------------------------------------------------------------------------------%
updateStep(S) :-
    retractall( currentStep(_) ),
    assertz(    currentStep(S) ).



%------------------------------------------------------------------------------%
updateEnergy(X) :- 
    retractall( energy(_) ),
    assertz(    energy(X) ).



%------------------------------------------------------------------------------%
updateMaxEnergy(X) :- 
    retractall( maxEnergy(_) ),
    assertz(    maxEnergy(X) ).



%------------------------------------------------------------------------------%
updateHealth(H) :- 
    retractall( myHealth(_) ),
    assertz(    myHealth(H) ).



%------------------------------------------------------------------------------%
updateMaxHealth(X) :- 
    retractall( maxHealth(_) ),
    assertz(    maxHealth(X) ).



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
% The position should keep track of which turn the agent was seen.
updatePosition(X) :-
    myName(A),
    retractall( hposition(A, _) ),
    retractall( kposition(A, _) ),
    assertz(    hposition(A, X) ),
    assertz(    kposition(A, X) ).



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
updateEdge(k(edge(Node1, Node2, Cost))) :-
    % Si el arco ya estaba en la kb (con costo unknown o el real).
    k(edge(Node1, Node2, Cost)), 
    !.
updateEdge(k(edge(Node1, Node2, unknown))) :-
    % Si ya estaba en la kb con su costo final.
    k(edge(Node1, Node2, Cost)),
    Cost \= unknown, 
    !.
updateEdge(k(edge(Node1, Node2, Cost))) :-
    % Si conociamos el arco pero no su valor (es decir, cuando hacemos un survey del arco).
    % En este caso podriamos preguntar si Cost \= unknown.
    k(edge(Node1, Node2, unknown)),
    !,
    deleteEdge(Node1, Node2, unknown),
    insertEdge(Node1, Node2, Cost).
updateEdge(k(edge(Node1, Node2, Cost))) :- 
    % Si es la primera vez que vemos el arco.
    insertEdge(Node1, Node2, Cost).



%------------------------------------------------------------------------------%
updateNodeValue(k(nodeValue(Name, unknown))) :-
    % El valor en la percepcion es unknown, y el nodo ya es conocido, luego no
    % hay que hacer nada. 
    k(nodeValue(Name, _)), 
    !.
updateNodeValue(k(nodeValue(Name, Value))) :-
    % El valor en la percepcion es distinto de unknown, luego se aserta
    % independientemente si es conocido o no.
    Value \= unknown,
    !,
    retractall( k(nodeValue(Name,     _)) ),
    assertz(    k(nodeValue(Name, Value)) ).
updateNodeValue(X) :-
    % El nodo es desconocido.
    assertz( X ).



%------------------------------------------------------------------------------%
updateNodeTeam(k(nodeTeam(Name, CurrentTeam))) :-
    currentStep(Step),
    retractall( k(nodeTeam(Name,           _,    _)) ),
    assertz(    k(nodeTeam(Name, CurrentTeam, Step)) ).



%------------------------------------------------------------------------------%
updateEntityPosition(Name, Position) :-
    assertz( kposition(Name, Position) ),
    assertz( hposition(Name, Position) ).



%------------------------------------------------------------------------------%
updateEntityTeam(Name, Team) :-
    agentTeam(Name, Team).
updateEntityTeam(Name, Team) :-
    assertz( agentTeam(Name, Team) ).



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
    kposition(A, Pos),
    k(edge(Pos, N, _)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Auxiliary                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

