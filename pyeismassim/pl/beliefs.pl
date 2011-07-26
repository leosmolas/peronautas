% posibleExpansion(Nodo)

% estoyEnLaFrontera

:- [utils], [kmap], [graph/map], [graph/nodes], [graph/edges], [graph/agents], [delp/arg].
:- dynamic b/1.

% Expansion

setEstoyEnLaFrontera :-
    myName(A),
	currentStep(CurrentStep),
    lastKnownPosition(CurrentStep, A, X),
    my_team(T),	
    k(nodeTeam(_S2, X, T)),
    findall(Neigh, k(edge(X, Neigh, _V), Neighbors),
    chequearFrontera(Neighbors, T),
    assert(b(estoyEnLaFrontera)).

chequearFrontera(Neigh, T) :-
	currentStep(Step),
    member(X, Neigh),
    k(nodeTeam(Step, X, T)), !,
    member(Y, Neigh),
    k(nodeTeam(Step, Y, Other)), 
	Other \= T, !.

setPosibleExpansion :-
    b(estoyEnLaFrontera),
    myName(A),
	currentStep(CurrentStep),
    position(CurrentStep, A, X),
    my_team(T),	
    k(nodeTeam(_S2, X, T)),
    foreach((k(edge(X, Neigh, _)), k(nodeTeam(CurrentStep, Neigh, none))), assert(b(posibleExpansion(Neigh)))).
    
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
    ),
	foreach(
        b(posibleProbear(Node)),
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
   
% Probear

probed(vertex0).
probed(vertex1).
probed(vertex3).

setPosibleProbear :- 
	foreach((k(nodeValue(Node, _Value)), not(probed(Node))), assert(b(posibleProbear(Node)))).
	
setInZone :-
	myTeam(MyTeam),
	foreach(k(nodeTeam(_Step, Node, MyTeam)), assert(inZone(Node) <- true)).
	
% Aumento

setPosibleAumento :-
	myTeam(MyTeam),
	currentStep(Step),
	assert(isGoal(_Node, Cost):- Cost < 3),
	findall(Node, (k(nodeTeam(Step, Node, MyTeam)), kneighbors(Node, Neighbors), chequearFrontera(Neighbors, MyTeam)), FrontierNodes),
	foreach((member(Node, FrontierNodes), bfs(Node, [Node], [Node], [First | Path], 0, _Cost)), assert(b(posibleAumento(First)))),
	retract(isGoal(_Node, Cost):- Cost < 3).