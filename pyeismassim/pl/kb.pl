:- dynamic kposition/1,
           energy/1,
           last_action/1,
           last_action_result/1,
           money/1,
           knode/3,
           kedge/3,
           intention/1,
           max_health/1,
           plan/1,
           max_energy/1.

% Beliefs
last_action(a).
action(skip).
kposition(pete).

insertEdge(Node1, Node2, Cost) :-
    assertz(kedge(Node1, Node2, Cost),
    assertz(kedge(Node2, Node1, Cost),
    assertz(hedge(Node1, Node2, Cost),
    assertz(hedge(Node2, Node1, Cost).

deleteEdge(Node1, Node2, Cost) :-
    retract(kedge(Node1, Node2, Cost),
    retract(kedge(Node2, Node1, Cost),
    retract(hedge(Node1, Node2, Cost),
    retract(hedge(Node2, Node1, Cost).

%lista vacia
updateEdges([]).
% si el arco ya estaba en la kb (con costo unknown o el real)
updateEdges([X|Xs]) :-
    X = kedge(Node1, Node2, Cost),
    kedge(Node1, Node2, Cost), !,
    updateEdges(Xs).
% si ya estaba en la kb con su costo final
updateEdges([X|Xs]) :-
    X = kedge(Node1, Node2, unknown),
    kedge(Node1, Node2, Cost),
    Cost \= unknown, !,
    updateEdges(Xs).
%si conociamos el arco pero no su valor
% (es decir, cuando hacemos un survey del arco)
% en este caso podriamos preguntar si Cost \= unknown.
updateEdges([X|Xs]) :-
    X = kedge(Node1, Node2, Cost),
    kedge(Node1, Node2, unknown), !,
    deleteEdge(Node1, Node2, unknown),
    insertEdge(Node1, Node2, Cost),
    updateEdges(Xs).  
% si es la primera vez que vemos el arco
updateEdges([X|Xs]) :- 
    X = kedge(Node1, Node2, Cost),
    insertEdge(Node1, Node2, Cost),
    updateEdges(Xs).

updateValue(knode(Name, Value, Team), Value) :-
    knode(Name, unknown, _), !.
updateValue(knode(Name, unknown, Team), NewValue) :-
    knode(Name, NewValue, _).

% Formato de los nodos:
% knode(Name, Team, Value)
% Value es unknown si no sabemos cuanto vale el nodo
updateNodes([]).
% cuando no conocemos el valor de un nodo, simplemente
% actualizamos su owner
updateNodes([X|Xs]) :-
    X = knode(Name, Value, CurrentTeam),
    knode(Name, OldValue, OldTeam), !,
    updateValue(X, NewValue),
    retract(knode(Name, _, _)),
    retract(hnode(Name, _, _)),
    assertz(knode(Name, NewValue, CurrentTeam)),
    assertz(hnode(Name, NewValue, CurrentTeam)),
    updateNodes(Xs).
updateNodes([X|Xs]) :-
    assertz(X),
    updateNodes(Xs).

%member(X, [X | _]).
%member(X, [Y | Ys]) :- 
%    X \= Y, 
%    member(X, Ys).
actualizarListas([],_).
actualizarListas([X|Xs],Func) :- 
    F =.. [Func, V], 
    F,
    member(X, V),
    actualizarListas(Xs, Func).
actualizarListas([X | Xs], Func) :- 
    F =.. [Func, V],
    F,
    not(member(X, V)),
    retract( F ),
    F2 =.. [Func, [X | V]],
    assert(F2),
    actualizarListas(Xs, Func).

% Argumentacion.

intention(explore). % Intenciones posibles: explore, recharge

argumentation :- 
    intention(recharge),
    max_energy(X),
    energy(X),
    retract( intention(recharge) ),
    assert(  intention(explore)  ).
argumentation :- 
    last_action_result(failed),
    retract( intention(_)        ),
    assert(  intention(recharge) ).

% Planning.

planning :- 
    intention(explore),
    searchNeigh(N),
    retract( plan(_)         ),
    assert(  plan([goto(N)]) ).
planning :- 
    intention(recharge),
    retract( plan(_)          ),
    assert(  plan([recharge]) ).

searchNeigh(N) :- 
    kposition(Pos),
    kedge(Pos, N, _).

% Ejecutar.

plan([]).

exec(skip) :-
    plan([]).
exec(Action) :- 
    plan([X | Xs]),
    X =.. Action,
    retract( plan(_)  ),
    assert(  plan(Xs) ).

