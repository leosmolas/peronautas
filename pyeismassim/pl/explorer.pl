
exec(Action) :- 
    write(1),nl,
    action(Action).

%------------------------------------------------------------------------------%
reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.
reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    energy(X),
    X >= Cost,
    !.
reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).



%------------------------------------------------------------------------------%
% si tenemos suficiente energia y no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    write(2),nl,
    energy(X),
    write(2.1),write(' energy: '),write(X),nl,
    X > 0,
    write(2.2),nl,
    myName(Name),
    write(2.3),write(' name: '),write(Name),nl,
    kposition(Name, Position),
    write(2.4),write(' position: '),write(Position),nl,
    knode(Position, unknown, _), 
    write(2.5),nl,
    !.

%------------------------------------------------------------------------------%
action([survey, Position]) :-
    write(3),nl,
    energy(X),
    write(3.1),nl,
    X > 0,
    write(3.2),nl,
    myName(Name),
    write(3.3),nl,
    kposition(Name, Position),
    write(3.4),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.5),nl,
    !.

%------------------------------------------------------------------------------%
action([goto, X]) :-
    write(4),nl,
    myName(Name),
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
    reachableNode(X, L), 
    write(4.4),nl,
    !.
     
%------------------------------------------------------------------------------%
action([goto, X]) :-
    write(5),nl,
    myName(Name),
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

