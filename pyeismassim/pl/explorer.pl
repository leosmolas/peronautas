exec(Action) :- action(Action).

reachableNode(Node, [[Node, Cost] | T]) :-
    energy(X),
    X >= Cost.

reachableNode(Node, [ _ | T]) :-
    reachableNode(T).

% si tenemos suficiente energia y no conocemos el valor del nodo, hacemos probe
action(probe) :-
    energy(X),
    X > 0,
    my_name(Name),
    kposition(Name, Position),
    knode(Position, unknown, _), !.

action(survey) :-
    energy(X),
    X > 0,
    my_name(Name),
    kposition(Name, Position),
    isNotSurveyed(Position), !.

action(goto(X)) :-
    my_name(Name),
    kposition(Name, Position),
    findall([Node, Cost], (kedge(Position, Node, Cost), knode(Node, unknown, _)), L),
    reachableNode(X, L), !.
     
action(goto(X)) :-
    my_name(Name),
    kposition(Name, Position),
    energy(E),
    kedge(Position, X, Cost),
    E >= Cost, !.

action(recharge).

