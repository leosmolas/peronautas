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
    calcTime('rolSetBeliefs',
    rolSetBeliefs),
    calcTime('setEstoyEnLaFrontera',
    setEstoyEnLaFrontera),
    % write('estoy'),nl,
    calcTime('setPosibleExpansion',
    setPosibleExpansion), !,
    % write('expansion'),nl,
    % printFindAll('estoyEnLaFrontera', b(estoyEnLaFrontera)),
    % printFindAll('frontera', b(frontera(_B))),
    % printFindAll('posibleExpansion', b(posibleExpansion(_E))),
    calcTime('setPosibleAumento',
    setPosibleAumento), !,
    % printFindAll('posibleAumento', b(posibleAumento(_A))),
    % write('voy'),nl,
    calcTime('setPosibleExplorar',
    setPosibleExplorar),
    calcTime('setDistancia',
    setDistancia),
    calcTime('setDifPuntos',
    setDifPuntos),
    printFindAll('setDifPuntos', b(difPuntosZona(_N, _D)) <- true),
    % printFindAll('edge', k(edge(N1, N2, V))),
    printFindAll('b(path(InitialNode, FinalNode, Energy, Path, Plan, NewTurns2, RemainingEnergy1))', b(path(InitialNode, FinalNode, Energy, Path, Plan, NewTurns2, RemainingEnergy1))),
    printFindAll('setDistancia', b(distancia(Node, A, PathCost)) <- true).

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

writeLenght(Name, Node, Pattern) :-
    findall(
        Node, 
        Pattern,
        ListExpansion
    ),
    length(ListExpansion, L1),
   
    write('<cant name="'), write(Name),write('" value='), write(L1), writeln('/>').
    
setDifPuntos :-
    myName(A),
    myTeam(T),
    % teamPoints(T, ActualPoints),
    setHypotheticalMap,
    calcTime('coloringAlgorithm',
    coloringAlgorithm),
    teamHPoints(T, ActualPoints),
    writeLenght(
        'posibleExpansion', 
        Node1, 
        (
            b(posibleExpansion(Node1)),
            not(b(difPuntosZona(Node1, _DifPuntos1)) <- true)
        )
    ),
    write('ActualPoints: '), writeln(ActualPoints),
    foreach(
        (
            b(posibleExpansion(Node)),
            not(b(difPuntosZona(Node, _DifPuntos2)) <- true)
        ),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    calcTime('coloringAlgorithm',
                    coloringAlgorithm),
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    write('Node: '), writeln(Node),
                    write('Points: '), writeln(Points),
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ),
    % printFindAll('', b(difPuntosZona(Node, DifPuntos)) <- true),
    writeLenght(
        'posibleExplorar', 
        Node1, 
        (
            b(posibleExplorar(Node1)),
            not(b(difPuntosZona(Node1, _DifPuntos1)) <- true)
        )
    ),
    foreach(
        (
            b(posibleExplorar(Node)),
            not(b(difPuntosZona(Node, _DifPuntos2)) <- true)
        ),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    calcTime('coloringAlgorithm',
                    coloringAlgorithm),
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    write('Node: '), writeln(Node),
                    write('Points: '), writeln(Points),
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ),
    writeLenght(
        'posibleAumento', 
        Node1, 
        (
            b(posibleAumento(Node1)),
            not(b(difPuntosZona(Node1, _DifPuntos1)) <- true)
        )
    ),
    foreach(
        (
            b(posibleAumento(Node)),
            not(b(difPuntosZona(Node, _DifPuntos3)) <- true)
        ),
        (
            doNotFail(
                (
                    setHypotheticalMap,
                    moveAgent(A, Node),
                    calcTime('coloringAlgorithm',
                    coloringAlgorithm),
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    write('Node: '), writeln(Node),
                    write('Points: '), writeln(Points),
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
	writeLenght(
        'posibleExplorar', 
        Node1, 
        (
            b(posibleExplorar(Node1))
        )
    ),
	printFindAll('b(posibleExplorar(Node))', b(posibleExplorar(Node))),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        (
            b(posibleExplorar(Node))
            % not(b(distancia(Node, [[survey]], _PathCost2)) <- true)
        ),
        (
            % writeln('6.1'),nl,
            % writeln(Node),nl,

            searchPath(Position, Node, Energy, [[survey]], 1)
        )
    ),
    writeLenght(
        'posibleAumento', 
        Node1, 
        (
            b(posibleAumento(Node1))
        )
    ),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        (
            b(posibleAumento(Node))
            % not(b(distancia(Node, _PathCost)) <- true)
        ),
        (
            % writeln('6.1'),nl,
            % writeln(Node),nl,

            searchPath(Position, Node, Energy, [], 0)
        )
    ), 
    !.
    
setDistancia.

searchPath(Position, Node, Energy, ActionToBeDone, CostOfAction) :-
	% writeln('pathSearch'),
    pathSearch(Position, Node, Energy, ActionToBeDone, CostOfAction, _Path, _Actions, PathCost), !,
	% printFindAll('paths', b(path(_X1,_X2,_X3,_X4,_X5,_X6,_X7))),
    % writeln('6.2'),nl,
    assert(b(distancia(Node, ActionToBeDone, PathCost)) <- true).
    % writeln('6.3'),nl.
    
searchPath(_Position, _Node, _Energy, _ActionToBeDone, _CostOfAction).

    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explorar
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleExplorar :-
	% printFindAll('hasAtLeastOneUnsurveyedEdge(Node)', hasAtLeastOneUnsurveyedEdge(_)),
    currentStep(Step),
    myName(Name),
    position(Step, Name, Position),
    % writeln('1'),
    retractall(isGoal(_, _)),
    assert((isGoal(Node, Cost) :- hasAtLeastOneUnsurveyedEdge(Node), Cost < 2)),
    % writeln('2'),
	foreach(
        (
            breadthFirst(Position, FinalNode, _Path, _Cost)
        ), 
        assertOnce(b(posibleExplorar(FinalNode)))
    ),
    chequearPosibleExplorar(2).
    
chequearPosibleExplorar(6) :- !.
    
chequearPosibleExplorar(_) :-
    b(posibleExplorar(Node)), !.
    
chequearPosibleExplorar(X) :-
    currentStep(Step),
    myName(Name),
    position(Step, Name, Position),
    % writeln('1'),
    retractall(isGoal(_, _)),
    NewCost is X + 2,
    assert((isGoal(Node, Cost) :- hasAtLeastOneUnsurveyedEdge(Node), Cost >= X, Cost < NewCost)),
    % writeln('2'),
	foreach(
        (
            breadthFirst(Position, FinalNode, _Path, _Cost)
        ), 
        assertOnce(b(posibleExplorar(FinalNode)))
    ),
    chequearPosibleExplorar(NewCost).