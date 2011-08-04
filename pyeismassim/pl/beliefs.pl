% posibleExpansion(Nodo)

% estoyEnLaFrontera

:-  [utils], 
    % [kmap], 
    [graph/map], 
    [delp/arg].
    
:- dynamic b/1.

% setBeliefs :-
    % currentStep(0), !.
    
    
setBeliefs :-
    
    % write('setBeliefs'),nl,
    rolSetBeliefs,
    setEstoyEnLaFrontera,
    % write('estoy'),nl,
    setPosibleExpansion, !,
    % write('expansion'),nl,
    % printFindAll('estoyEnLaFrontera', b(estoyEnLaFrontera)),
    % printFindAll('frontera', b(frontera(_B))),
    % printFindAll('posibleExpansion', b(posibleExpansion(_E))),
    setPosibleAumento, !,
    % printFindAll('posibleAumento', b(posibleAumento(_A))),
    % write('voy'),nl,
    setPosibleExplorar,
    % printFindAll('posibleExplorar', b(posibleExplorar(_))),
    
    % printFindAll('setPosibleProbear', b(setPosibleProbear(_))),
    setDifPuntos,
    % printFindAll('setDifPuntos', b(difPuntosZona(_N, _D)) <- true),
    % printFindAll('k', k(_)),
    setDistancia.
    % printFindAll('setDistancia', b(distancia(Node, PathCost)) <- true).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expansion
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setFrontera :-
    currentStep(Step),
    myTeam(T),
    % T \= none, % ???????
    foreach(
        (
            k(nodeTeam(Step, Node, T)),
            
            k(edge(Node, Neigh, _V)),
            k(nodeTeam(Step, Neigh, T2)),
            T2 \= T
        ),
        assertOnce(b(frontera(Node)))
    ).

setEstoyEnLaFrontera :-
    setFrontera,
    myName(A),
    % writeln('1'),nl,
    lastKnownPosition(_Step, A, X),
    % myTeam(T),	
    % writeln('2'),nl,
    % k(nodeTeam(_S2, X, T)),
    % findall(Neigh, k(edge(X, Neigh, _V)), Neighbors),
    % % writeln('3'),nl,
    % chequearFrontera(Neighbors, T),
    % % writeln('4'),nl,
    b(frontera(X)), !,
    assert(b(estoyEnLaFrontera)).
    
setEstoyEnLaFrontera.

chequearFrontera(Neigh, T) :-
	currentStep(Step),
    % member(X, Neigh),
    % k(nodeTeam(Step, X, T)), !,
    member(Y, Neigh),
    k(nodeTeam(Step, Y, Other)), 
	Other \= T, !.

setPosibleExpansion :-
    b(estoyEnLaFrontera),
    myName(A),
	currentStep(CurrentStep),
    position(CurrentStep, A, X),
    myTeam(T),	
    k(nodeTeam(_S2, X, T)),
    foreach((k(edge(X, Neigh, _)), k(nodeTeam(CurrentStep, Neigh, none))), assert(b(posibleExpansion(Neigh)))).
    
setPosibleExpansion.
    
setDifPuntos :-
    myName(A),
    myTeam(T),
    teamPoints(T, ActualPoints),
    foreach(
        b(posibleExpansion(Node)),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    coloringAlgorithm,
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ),
    % printFindAll('', b(difPuntosZona(Node, DifPuntos)) <- true),
    foreach(
        (
            b(posibleExplorar(Node)),
            not(b(posibleExpansion(Node)))
        ),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    coloringAlgorithm,
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ),
    foreach(
        (
            b(posibleAumento(Node)),
            not(b(posibleExplorar(Node))),
            not(b(posibleExpansion(Node)))
        ),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    coloringAlgorithm,
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ).
    

	
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aumento
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleAumento :-
	% myTeam(MyTeam),
	currentStep(Step),
    myName(MyName),
    visualRange(Step, MyName, VisualRange),
	assert((isGoal(_Node, Cost) :- !, Cost < VisualRange, Cost > 0)),
    setof(
        FinalNode,
        Node^ _Path^ _Cost^ Team^(
            b(frontera(Node)),
            breadthFirst(Node, FinalNode, _Path, _Cost),
            k(nodeTeam(Step, FinalNode, Team)), 
            Team \= peronismo
        ), 
        Aumento
    ),
    foreach(
        member(X, Aumento),
        assert(b(posibleAumento(X)))
    ),
    
	retract((isGoal(_Node2, Cost):- !, Cost < VisualRange, Cost > 0)).
    
setPosibleAumento.

setDistancia :-
    % writeln('setDistancia'),nl,
    % writeln('1'),nl,
    myName(Name),
    % writeln('2'),nl,
    currentStep(Step),
    % writeln('3'),nl,
    position(Step, Name, Position),
    % writeln('4'),nl,
    energy(Step, Name, Energy),
    % writeln('5'),nl,
    foreach(
        b(posibleAumento(Node)),
        (
            % writeln('6.1'),nl,
            % writeln(Node),nl,
            searchPath(Position, Node, Energy, [], 0)
        )
    ), 
    foreach(
        b(posibleExplorar(Node)),
        (
            % writeln('6.1'),nl,
            % writeln(Node),nl,
            searchPath(Position, Node, Energy, [[survey]], 1)
        )
    ), !.
    
setDistancia.

searchPath(Position, Node, Energy, ActionToBeDone, CostOfAction) :-
    pathSearch(Position, Node, Energy, ActionToBeDone, CostOfAction, _Path, _Actions, PathCost), !,
    % printFindAll('paths', b(path(_X1,_X2,_X3,_X4,_X5,_X6))),
    % writeln('6.2'),nl,
    assert(b(distancia(Node, PathCost)) <- true).
    % writeln('6.3'),nl.
    
searchPath(_Position, _Node, _Energy, _ActionToBeDone, _CostOfAction).

    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explorar
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleExplorar :-
    foreach(
        hasAtLeastOneUnsurveyedEdge(Node),
        assertOnce(b(posibleExplorar(Node)))
    ).
