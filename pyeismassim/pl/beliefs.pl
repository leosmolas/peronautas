% posibleExpansion(Nodo)

% estoyEnLaFrontera

:- [kmap].
:- dynamic b/1.

myName(vasco).
kposition(vasco, vertex0).

my_team(d3lp0r).



setEstoyEnLaFrontera :-
    myName(A),
    kposition(A, X),
    my_team(T),
    knode(X, T, _V),
    findall(Neigh, kedge(X,Neigh,_), Neighbors),
    chequearFrontera(Neighbors, T),
    assert(b(estoyEnLaFrontera)).

chequearFrontera(Neigh, T) :-
    member(X, Neigh),
    knode(X, T, _), !,
    member(Y, Neigh),
    knode(Y, none, _), !.

setPosibleExpansion :-
    b(estoyEnLaFrontera),
    myName(A),
    kposition(A, X),
    my_team(T),
    knode(X, T, _V),
    foreach((kedge(X,Neigh,_), knode(Neigh, none, _V2)), assert(b(posibleExpansion(Neigh)))).


    