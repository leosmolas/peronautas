
%------------------------------------------------------------------------------%
exec(Action) :- 
    write(1),nl,
    action(Action).

%------------------------------------------------------------------------------%
reachable_node(_Node, []) :-
    fail.
reachable_node(Node, [[Node, unknown] | T]) :-
    reachable_node(_, T).
reachable_node(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    energy(X),
    X >= Cost.
reachable_node(_, [_ | T]) :-
    reachable_node(_, T).



%------------------------------------------------------------------------------%
% si tenemos suficiente energia y no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    write(2),nl,
    energy(X),
    write('energy:'),write(X),nl,
    X > 0,
    my_name(Name),
    write('name:'),write(Name),nl,
    kposition(Name, Position),
    write('position:'),write(Position),nl,
    knode(Position, unknown, _), 
    write(2.1),nl,
    !.

%------------------------------------------------------------------------------%
action([survey, Position]) :-
    write(3),nl,
    energy(X),
    write(3.1),nl,
    X > 0,
    write(3.2),nl,
    my_name(Name),
    write(3.3),nl,
    kposition(Name, Position),
    write(3.4),nl,
    is_not_surveyed(Position), 
    write(3.5),nl,
    !.

%------------------------------------------------------------------------------%
action([goto, X]) :-
    write(4),nl,
    my_name(Name),
    write(4.1),nl,
    kposition(Name, Position),
    write(4.2),nl,
    findall(
        [Node, Cost], 
        (
            kedge(Position, Node, Cost), 
            knode(Node, unknown, _)
        ), 
        L),
    write(4.3),nl,
    write('L: '),write(L),nl,
    reachable_node(X, L), 
    write(4.4),nl,
    !.
     
%------------------------------------------------------------------------------%
action([goto, X]) :-
    write(5),nl,
    my_name(Name),
    write(5.1),nl,
    kposition(Name, Position),
    write(5.2),nl,
    energy(E),
    write(5.3),nl,
    kedge(Position, X, Cost),
    Cost \= unknown,
    write(5.4),nl,
    E >= Cost, 
    write(5.5),nl,
    !.

%------------------------------------------------------------------------------%
action([recharge]) :-
    write(6),nl.

