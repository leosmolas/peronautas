% posibleExpansion(Nodo)

% estoyEnLaFrontera

:- [kmap], [graph/map], [graph/nodes], [graph/edges], [graph/agents], [delp/arg].
:- dynamic b/1.



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
    
setDifPuntos :-
    myName(A),
    % kposition(A, X),
    my_team(T),
    teamPoints(T, ActualPoints),
    foreach(
        b(posibleExpansion(Node)),
        (
            assertHMap,
            moveAgent(A, Node),
            coloringAlgorithm,
            teamHPoints(T, Points),
            DifPuntos is Points - ActualPoints,
            assert(b(difPuntosZona(Node, DifPuntos)) <- true),
            retractall(hnode(_, _, _)),
            retractall(hedge(_, _, _)),
            retractall(hposition(_, _))
        )
    ).
    
test :-
    setEstoyEnLaFrontera,
    setPosibleExpansion,
    setDifPuntos.
   
    