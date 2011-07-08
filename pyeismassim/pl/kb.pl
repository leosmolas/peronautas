:- dynamic kposition/2,
           energy/1,
           last_action/1,
           last_action_result/1,
           money/1,
           knode/3,
           kedge/3,
           intention/1,
           max_health/1,
           plan/1,
           max_energy/1,
           my_name/1.

:- ['pl/graph/map.pl'].
% Beliefs
last_action(skip).
kposition(pete, etep).
hposition(pete, etep).
my_name(jesucristo).

isNotSurveyed(Node) :-
    findall(Node, kedge(Node, _, unknown), []).

replace_myName(X) :- 
   retractall(my_name(OldName)),
   retractall(hposition(OldName, Op1)),
   retractall(kposition(OldName, Op2)),
   assertz(hposition(X, Op1)),
   assertz(kposition(X, Op2)),
   assertz(my_name(X)).


replace_position(X) :-
    my_name(A),
    retractall(hposition(A, _)),
    retractall(kposition(A, _)),
    assertz(hposition(A, X)),
    assertz(kposition(A, X)).

replace_energy(X) :- 
    retractall(energy(_)),
    assertz(energy(X)).

replace_last_action(X) :- 
    retractall(last_action(_)),
    assertz(last_action(X)).

replace_last_action_result(X) :- 
    retractall(last_action_result(_)),
    assertz(last_action_result(X)).

replace_money(X) :- 
    retractall(money(_)), 
    assertz(money(X)).

replace_max_health(X) :- 
    retractall(max_health(_)),
    assertz(max_health(X)).

replace_max_energy(X) :- 
    retractall(max_energy(_)),
    assertz(max_energy(X)).

insertEdge(Node1, Node2, Cost) :-
    assertz(kedge(Node1, Node2, Cost)),
    assertz(kedge(Node2, Node1, Cost)),
    assertz(hedge(Node1, Node2, Cost)),
    assertz(hedge(Node2, Node1, Cost)).

deleteEdge(Node1, Node2, Cost) :-
    retract(kedge(Node1, Node2, Cost)),
    retract(kedge(Node2, Node1, Cost)),
    retract(hedge(Node1, Node2, Cost)),
    retract(hedge(Node2, Node1, Cost)).

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

updateValue(knode(Name, Value, _Team), Value) :-
    knode(Name, unknown, _), !.
updateValue(knode(Name, unknown, _Team), NewValue) :-
    knode(Name, NewValue, _).

% Formato de los nodos:
% knode(Name, Team, Value)
% Value es unknown si no sabemos cuanto vale el nodo
updateNodes([]).
% cuando no conocemos el valor de un nodo, simplemente
% actualizamos su owner
updateNodes([X|Xs]) :-
    X = knode(Name, _Value, CurrentTeam),
    knode(Name, _OldValue, _OldTeam), !,
    updateValue(X, NewValue),
    retractall(knode(Name, _, _)),
    retractall(hnode(Name, _, _)),
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
actualizarListas([X | Xs], Func) :- 
    % genera la consulta ?- Func(V)
    F =.. [Func, V], 
    F,
    % es X miembro de Func(V) ?
    member(X, V),
    % si lo es, lo salteamos
    actualizarListas(Xs, Func).
actualizarListas([X | Xs], Func) :- 
    F =.. [Func, V],
    F,
    % si X no es miembro de Func(V)
    not(member(X, V)),
    % entonces lo retracto para asegurar que lo reemplazo
    retract( F ),
    F2 =.. [Func, [X | V]], % -> Func([X, ... miembros de V... ])
    % y lo aserto
    assert(F2),
    actualizarListas(Xs, Func).

% Argumentacion.

intention(explore). % Intenciones posibles: explore, recharge

%   argumentation :- 
%       intention(recharge),
%       max_energy(X),
%       energy(X),
%       retract( intention(recharge) ),
%       assert(  intention(explore)  ).
%   
%   argumentation :- 
%       last_action_result(failed),
%       retract( intention(_)        ),
%       assert(  intention(recharge) ).
%   
%   % Planning.
%   
%   planning :- 
%       intention(explore),
%       searchNeigh(N),
%       retract( plan(_)         ),
%       assert(  plan([goto(N)]) ).
%   planning :- 
%       intention(recharge),
%       retract( plan(_)          ),
%       assert(  plan([recharge]) ).
   
searchNeigh(N) :-
    my_name(A),
    kposition(A, Pos),
    kedge(Pos, N, _).

% Ejecutar.

