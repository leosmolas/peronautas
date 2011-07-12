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

my_team(d3lp0r).

% last_action/1
%   1st argument is a term representing the agent's last action.

% kposition/2
%   1st argument is...
%   2nd argument is...

% hposition/2
%   1st argument is...
%   2nd argument is...

% my_name/1
%   1st argument is an atom representing the agent's name.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Percept Processing                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%------------------------------------------------------------------------------%
% Succeeds when Node has not been surveyed. 
is_not_surveyed(Node) :-
    findall(Node, kedge(Node, _, unknown), []).



%------------------------------------------------------------------------------%
replace_my_name(X) :- 
   retractall(my_name(OldName)),
   retractall(hposition(OldName, Op1)),
   retractall(kposition(OldName, Op2)),
   assertz(hposition(X, Op1)),
   assertz(kposition(X, Op2)),
   assertz(my_name(X)).



%------------------------------------------------------------------------------%
replace_position(X) :-
    my_name(A),
    retractall(hposition(A, _)),
    retractall(kposition(A, _)),
    assertz(hposition(A, X)),
    assertz(kposition(A, X)).



%------------------------------------------------------------------------------%
replace_energy(X) :- 
    retractall(energy(_)),
    assertz(energy(X)).



%------------------------------------------------------------------------------%
replace_last_action(X) :- 
    retractall(last_action(_)),
    assertz(last_action(X)).



%------------------------------------------------------------------------------%
replace_last_action_result(X) :- 
    retractall(last_action_result(_)),
    assertz(last_action_result(X)).



%------------------------------------------------------------------------------%
replace_money(X) :- 
    retractall(money(_)), 
    assertz(money(X)).



%------------------------------------------------------------------------------%
replace_max_health(X) :- 
    retractall(max_health(_)),
    assertz(max_health(X)).



%------------------------------------------------------------------------------%
replace_max_energy(X) :- 
    retractall(max_energy(_)),
    assertz(max_energy(X)).



%------------------------------------------------------------------------------%
insert_edge(Node1, Node2, Cost) :-
    assertz(kedge(Node1, Node2, Cost)),
    assertz(kedge(Node2, Node1, Cost)),
    assertz(hedge(Node1, Node2, Cost)),
    assertz(hedge(Node2, Node1, Cost)).



%------------------------------------------------------------------------------%
delete_edge(Node1, Node2, Cost) :-
    retract(kedge(Node1, Node2, Cost)),
    retract(kedge(Node2, Node1, Cost)),
    retract(hedge(Node1, Node2, Cost)),
    retract(hedge(Node2, Node1, Cost)).



%------------------------------------------------------------------------------%
update_edges([]).
update_edges([X | Xs]) :-
    % Si el arco ya estaba en la kb (con costo unknown o el real).
    X = kedge(Node1, Node2, Cost),
    kedge(Node1, Node2, Cost), 
    !,
    update_edges(Xs).
update_edges([X | Xs]) :-
    % Si ya estaba en la kb con su costo final.
    X = kedge(Node1, Node2, unknown),
    kedge(Node1, Node2, Cost),
    Cost \= unknown, 
    !,
    update_edges(Xs).
update_edges([X | Xs]) :-
    % Si conociamos el arco pero no su valor (es decir, cuando hacemos un survey del arco).
    % En este caso podriamos preguntar si Cost \= unknown.
    X = kedge(Node1, Node2, Cost),
    kedge(Node1, Node2, unknown), 
    !,
    delete_edge(Node1, Node2, unknown),
    insert_edge(Node1, Node2, Cost),
    update_edges(Xs).
update_edges([X|Xs]) :- 
    % Si es la primera vez que vemos el arco.
    X = kedge(Node1, Node2, Cost),
    insert_edge(Node1, Node2, Cost),
    update_edges(Xs).



%------------------------------------------------------------------------------%
update_value(knode(Name, Value, _Team), Value) :-
    knode(Name, unknown, _), !.
update_value(knode(Name, unknown, _Team), NewValue) :-
    knode(Name, NewValue, _).



%------------------------------------------------------------------------------%
update_node(knode(Name, Value, CurrentTeam)) :-
    knode(Name, _OldValue, _OldTeam), !,
    update_value(knode(Name, Value, CurrentTeam), NewValue),
    retractall(knode(Name, _, _)),
    retractall(hnode(Name, _, _)),
    assertz(knode(Name, NewValue, CurrentTeam)),
    assertz(hnode(Name, NewValue, CurrentTeam)).
update_node(X) :-
    assertz(X).



%------------------------------------------------------------------------------%
update_lists([],_).
update_lists([X | Xs], Func) :- 
    % genera la consulta ?- Func(V)
    F =.. [Func, V], 
    F,
    % es X miembro de Func(V) ?
    member(X, V),
    % si lo es, lo salteamos
    update_lists(Xs, Func).
update_lists([X | Xs], Func) :- 
    F =.. [Func, V],
    F,
    % si X no es miembro de Func(V)
    not(member(X, V)),
    % entonces lo retracto para asegurar que lo reemplazo
    retract( F ),
    F2 =.. [Func, [X | V]], % -> Func([X, ... miembros de V... ])
    % y lo aserto
    assert(F2),
    update_lists(Xs, Func).



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
    my_name(A),
    kposition(A, Pos),
    kedge(Pos, N, _).

