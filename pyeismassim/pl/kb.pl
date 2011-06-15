:- dynamic position/1,
           energy/1,
           last_action/1,
           last_action_result/1,
           money/1,
           verts/1,
           edges/1,
           intention/1,
           max_health/1,
           plan/1,
           max_energy/1.

% Percept terms
% timestamp(T)
% deadline(D)
% id(ID)
% step(S)
% energy(E)
% health(H)
% last_action(LA)
% last_action_result(LAR)
% max_energy(ME)
% max_energy_disabled(MED)
% max_health(MH)
% position(P)
% strength(STR)
% vis_range(VR)
% zone_score(ZS)
% last_step_score(LSS)
% money(M)
% score(SCR)
% achievements(ACH)
% vis_vert(Name, Team)
% vis_edge(Node1, Node2)
% vis_ent(Name, Node, Team)
% probed_vert(Name, Value)
% surveyed_edge(Node1, Node2, Weight)
% inspected_ent(Energy, Health, Max_energy, Max_health, Name, Node, Role, Strength, Team, Vis_range)

% Beliefs
last_action(skip).
action(skip).
position(unknown).
verts([]).
edges([]). % Lista de functores de la forma edge(vert1, vert2).

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
    edges(E),
    member( edge(Pos, N), E). % Inferencia mapa.
searchNeigh(N) :- 
    position(Pos),
    edges(E),
    member( edge(N, Pos), E).

% Ejecutar.

plan([]).

exec(skip) :-
    plan([]).
exec(Action) :- 
    plan([X | Xs]),
    X =.. Action,
    retract( plan(_)  ),
    assert(  plan(Xs) ).

