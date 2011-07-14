:- dynamic kposition/2,
           energy/1,
           maxHealth/1,
           maxEnergy/1,
           lastAction/1,
           lastActionResult/1,
           money/1,
           knode/3,
           kedge/3,
           intention/1,
           plan/1,
           myName/1,
           myTeam/1,
           agentTeam/2.

:- ['pl/graph/map.pl'].

% Representation of actions:
%   Actions are represented by lists. 
%   The 1st element of the list is an atom representing the type of action.
%   This may be one of: 
%   The following elements are terms representing the action parameters, such as
%   node names, integers, etc.

% Representation of nodes:
%   knode(Name, Team, Value)
%   Value es 'unknown' si no sabemos cuanto vale el nodo. 
%   Cuando no conocemos el valor de un nodo, simplemente actualizamos su owner.

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
        kedge(Node1, Node2, unknown), 
        L),
    L \= [].



%------------------------------------------------------------------------------%
updateMyName(X) :- 
   retractall( myName(OldName)         ),
   retractall( hposition(OldName, Op1) ),
   retractall( kposition(OldName, Op2) ),
   assertz(    hposition(X, Op1)       ),
   assertz(    kposition(X, Op2)       ),
   assertz(    myName(X)               ).



%------------------------------------------------------------------------------%
updatePosition(X) :-
    myName(A),
    retractall( hposition(A, _) ),
    retractall( kposition(A, _) ),
    assertz(    hposition(A, X) ),
    assertz(    kposition(A, X) ).



%------------------------------------------------------------------------------%
updateEnergy(X) :- 
    retractall( energy(_) ),
    assertz(    energy(X) ).



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
updateMaxHealth(X) :- 
    retractall( maxHealth(_) ),
    assertz(    maxHealth(X) ).



%------------------------------------------------------------------------------%
updateMaxEnergy(X) :- 
    retractall( maxEnergy(_) ),
    assertz(    maxEnergy(X) ).



%------------------------------------------------------------------------------%
insertEdge(Node1, Node2, Cost) :-
    assertz( kedge(Node1, Node2, Cost) ),
    assertz( kedge(Node2, Node1, Cost) ),
    assertz( hedge(Node1, Node2, Cost) ),
    assertz( hedge(Node2, Node1, Cost) ).



%------------------------------------------------------------------------------%
deleteEdge(Node1, Node2, Cost) :-
    retract( kedge(Node1, Node2, Cost) ),
    retract( kedge(Node2, Node1, Cost) ),
    retract( hedge(Node1, Node2, Cost) ),
    retract( hedge(Node2, Node1, Cost) ).



%------------------------------------------------------------------------------%
% Succeeds if the edge must be updated.
updateEdge(kedge(Node1, Node2, Cost)) :-
    % Si el arco ya estaba en la kb (con costo unknown o el real).
    kedge(Node1, Node2, Cost), 
    !.
updateEdge(kedge(Node1, Node2, unknown)) :-
    % Si ya estaba en la kb con su costo final.
    kedge(Node1, Node2, Cost),
    Cost \= unknown, 
    !.
updateEdge(kedge(Node1, Node2, Cost)) :-
    % Si conociamos el arco pero no su valor (es decir, cuando hacemos un survey del arco).
    % En este caso podriamos preguntar si Cost \= unknown.
    kedge(Node1, Node2, unknown),
    !,
    deleteEdge(Node1, Node2, unknown),
    insertEdge(Node1, Node2, Cost).
updateEdge(kedge(Node1, Node2, Cost)) :- 
    % Si es la primera vez que vemos el arco.
    insertEdge(Node1, Node2, Cost).



%------------------------------------------------------------------------------%
updateNodeValue(knode(Name, Value, _Team), Value) :-
    knode(Name, unknown, _), 
    !.
updateNodeValue(knode(Name, unknown, _Team), NewValue) :-
    knode(Name, NewValue, _).



%------------------------------------------------------------------------------%
updateNode(knode(Name, Value, CurrentTeam)) :-
    knode(Name, _OldValue, _OldTeam), !,
    updateNodeValue(knode(Name, Value, CurrentTeam), NewValue),
    retractall( knode(Name, _,        _)           ),
    retractall( hnode(Name, _,        _)           ),
    assertz(    knode(Name, NewValue, CurrentTeam) ),
    assertz(    hnode(Name, NewValue, CurrentTeam) ).
updateNode(X) :-
    assertz( X ).



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
    kedge(Pos, N, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Auxiliary                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dumpKB :-
%    %findall(
%    %    knode(N1, T, C),
%    %    (
%    %        knode(N1, T, C)
%    %    ),
%    %    Nodes
%    %),
%    %findall(
%    %    kedge(N1, N2, C),
%    %    (
%    %        kedge(N1, N2, C)
%    %    ),
%    %    Edges
%    %),
%    %write('KNOWN NODES:'),nl,
%    %write_list(Nodes),nl,
%    %write('KNOWN EDGES:'),nl,
%    %write_list(Edges),nl.
%    tell('kb.txt'),
%    write('hello'),nl,
%    listing(knode),
%    listing(kedge),
%    told.
%
%write_list([]).
%write_list([X | Xs]) :-
%    write('    '),write(X),write(','),nl,
%    write_list(Xs).
