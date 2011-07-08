exec(Action) :- 
    write(1),nl,
    action(Action).

reachableNode(Node, [[Node, Cost] | _T]) :-
    energy(X),
    X >= Cost.

reachableNode(_Node, [ _ | T]) :-
    reachableNode(T).

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

action([survey, Position]) :-
    write(3),nl,
    energy(X),
    X > 0,
    my_name(Name),
    kposition(Name, Position),
    isNotSurveyed(Position), !.

action([goto, X]) :-
    write(4),nl,
    my_name(Name),
    kposition(Name, Position),
    findall([Node, Cost], (kedge(Position, Node, Cost), knode(Node, unknown, _)), L),
    reachableNode(X, L), !.
     
action([goto ,X]) :-
    write(5),nl,
    my_name(Name),
    write(5.1),nl,
    kposition(Name, Position),
    write(5.2),nl,
    energy(E),
    write(5.3),nl,
    kedge(Position, X, Cost),
    write(5.4),nl,
    E >= Cost, 
    write(5.5),nl,
    !.

action([recharge]) :-
    write(6),nl.

