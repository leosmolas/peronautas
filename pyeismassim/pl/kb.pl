:- dynamic position/1,
           energy/1,
           last_action/1,
           last_action_result/1,
           money/1,
           node/3,
           edge/3,
           intention/1,
           max_health/1,
           plan/1,
           max_energy/1.

% Beliefs
last_action(a).
action(skip).
position(pete).

%lista vacia
updateEdges([]).
% si el arco ya estaba en la kb (con costo unknown o el real)
updateEdges([X|Xs]) :-
    X = edge(Node1, Node2, Cost),
    edge(Node1, Node2, Cost), !,
    updateEdges(Xs).
% si ya estaba en la kb con su costo final
updateEdges([X|Xs]) :-
    X = edge(Node1, Node2, unknown),
    edge(Node1, Node2, Cost),
    Cost \= unknown, !,
    updateEdges(Xs).
%si conociamos el arco pero no su valor
% (es decir, cuando hacemos un survey del arco)
% en este caso podriamos preguntar si Cost \= unknown.
updateEdges([X|Xs]) :-
    X = edge(Node1, Node2, Cost),
    edge(Node1, Node2, unknown), !,
    retract(edge(Node1, Node2, unknown)),
    assertz(X),
    updateEdges(Xs).  
% si es la primera vez que vemos el arco
updateEdges([X|Xs]) :- 
    assertz(X),
    updateEdges(Xs).

updateValue(node(Name, Value, Team), Value) :-
    node(Name, unknown, _), !.
updateValue(node(Name, unknown, Team), NewValue) :-
    node(Name, NewValue, _).

% Formato de los nodos:
% node(Name, Team, Value)
% Value es unknown si no sabemos cuanto vale el nodo
updateNodes([]).
% cuando no conocemos el valor de un nodo, simplemente
% actualizamos su owner
updateNodes([X|Xs]) :-
    X = node(Name, Value, CurrentTeam),
    node(Name, OldValue, OldTeam), !,
    updateValue(X, NewValue),
    retract(node(Name, _, _)),
    assertz(node(Name, NewValue, CurrentTeam)),
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
    position(Pos),
    edge(Pos, N, _), !.
searchNeigh(N) :- 
    position(Pos),
    edge(N, Pos, _).

% Ejecutar.

plan([]).

exec(skip) :-
    plan([]).
exec(Action) :- 
    plan([X | Xs]),
    X =.. Action,
    retract( plan(_)  ),
    assert(  plan(Xs) ).

