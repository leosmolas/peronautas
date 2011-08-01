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
    write('setBeliefs'),nl,
    setEstoyEnLaFrontera,
    write('estoy'),nl,
    setPosibleExpansion, !,
    write('expansion'),nl,
    printFindAll('', b(estoyEnLaFrontera)),
    printFindAll('', b(frontera(_B))),
    printFindAll('', b(posibleExpansion(_E))),
    setPosibleAumento, !,
    printFindAll('', b(posibleAumento(_A))),
    % write('voy'),nl,
    setDifPuntos,
    printFindAll('setDifPuntos', b(difPuntosZona(_N, _D)) <- true).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expansion
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setFrontera :-
    currentStep(Step),
    myTeam(T),
    foreach(
        (
            k(nodeTeam(Step, Node, T)),
            T \= none,
            findall(Neigh, k(edge(Node, Neigh, _V)), Neighbors),
            chequearFrontera(Neighbors, T)
        ),
        assert(b(frontera(Node)))
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
    % write('entre'),nl,
    myName(A),
    % write('0'),nl,
    % kposition(A, X),
    myTeam(T),
    % writeln('1'),nl,
    teamPoints(T, ActualPoints),
    % writeln('2'),nl,
    foreach(
        b(posibleExpansion(Node)),
        (
            % writeln('3'),nl,
            setHypotheticalMap,
            % writeln('4'),nl,
            moveAgent(A, Node),
            % writeln('5'),nl,
            % printFindAll('position', h(_S)),
            % printFindAll('visible', visibleNode(_N)),
            % printFindAll('not visible', notVisible(_N2)),
            % printFindAll('explored', explored(_N3)),
            % printFindAll('not explored', notExplored(_N4)),
            coloringAlgorithm,
            % writeln('6'),nl,
            teamHPoints(T, Points),
            % writeln('7'),nl,
            DifPuntos is Points - ActualPoints,
            assert(b(difPuntosZona(Node, DifPuntos)) <- true)
            % writeln('8'),nl
        )
    ).
	% foreach(
        % b(posibleProbear(Node)),
        % (
            % assertHMap,
            % moveAgent(A, Node),
            % coloringAlgorithm,
            % teamHPoints(T, Points),
            % DifPuntos is Points - ActualPoints,
            % assert(b(difPuntosZona(Node, DifPuntos)) <- true),
            % retractall(hnode(_, _, _)),
            % retractall(hedge(_, _, _)),
            % retractall(hposition(_, _))
        % )
    % ).
    

    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Probear
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% probed(vertex0).
% probed(vertex1).
% probed(vertex3).

setPosibleProbear :- 
	foreach((k(nodeValue(Node, _Value)), not(probed(Node))), assert(b(posibleProbear(Node)))).
	
setInZone :-
	myTeam(MyTeam),
	foreach(k(nodeTeam(_Step, Node, MyTeam)), assert(inZone(Node) <- true)).
	
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aumento
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleAumento :-
	% myTeam(MyTeam),
	currentStep(Step),
    myName(MyName),
    visualRange(Step, MyName, VisualRange),
	assert((isGoal(_Node, Cost) :- !, Cost < VisualRange)),
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
    
	retract((isGoal(_Node2, Cost):- !, Cost < VisualRange)).
    
setPosibleAumento.