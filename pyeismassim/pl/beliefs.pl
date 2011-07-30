% posibleExpansion(Nodo)

% estoyEnLaFrontera

:- [utils], [kmap], [graph/map], [delp/arg].
:- dynamic b/1.

% Expansion

setFrontera :-
    currentStep(Step),
    myTeam(T),
    foreach(
        (
            
            k(nodeTeam(Step, Node, T)),
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
    % % writeln('2'),nl,
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
    myTeam(T),	
    k(nodeTeam(_S2, X, T)),
    foreach((k(edge(X, Neigh, _)), k(nodeTeam(CurrentStep, Neigh, none))), assert(b(posibleExpansion(Neigh)))).
    
% setDifPuntos :-
    % myName(A),
    kposition(A, X),
    % myTeam(T),
    % teamPoints(T, ActualPoints),
    % foreach(
        % b(posibleExpansion(Node)),
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
    
test :-
    setEstoyEnLaFrontera,
    setPosibleExpansion,
    
    setPosibleAumento.
    % setDifPuntos.
   
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
	% myTeam(MyTeam),
	currentStep(Step),
	assert((isGoal(_Node, Cost) :- !, Cost < 3)),
	% findall(
        % Node, 
        % (
            % k(nodeTeam(Step, Node, peronismo)), 
            % % kneighbors(Node, Neighbors), 
            % findall(Node2, k(edge(Node, Node2, _V)), Neighbors),
            % chequearFrontera(Neighbors, peronismo)
        % ), 
        % FrontierNodes
    % ),
	% foreach((member(Node, FrontierNodes), bfs(Node, [Node], [Node], [First | _Path], 0, _Cost)), assert(b(posibleAumento(First)))),
    % breadthFirst(+InitialNode, -FinalNode, -Path, -Cost)
	% foreach(
        % (
            % k(nodeTeam(Step, Node, peronismo)), 
            % % write('1'),nl,
            % % kneighbors(Node, Neighbors), 
            % findall(Node2, k(edge(Node, Node2, _V)), Neighbors),
            % % write('2'),nl,
            % chequearFrontera(Neighbors, peronismo),
            % % write('3'),nl,
            % breadthFirst(Node, FinalNode, _Path, _Cost),
            % % write('4'),nl,
            % k(nodeTeam(Step, FinalNode, Team)), 
            % Team \= peronismo
        % ), 
        % assert(b(posibleAumento(FinalNode)))
    % ),
    setof(
        FinalNode,
        Node^ _Path^ _Cost^Team^(
        % (
            % k(nodeTeam(Step, Node, peronismo)), 
            % % write('1'),nl,
            % % kneighbors(Node, Neighbors), 
            % findall(Node2, k(edge(Node, Node2, _V)), Neighbors),
            % % write('2'),nl,
            % chequearFrontera(Neighbors, peronismo),
            b(frontera(Node)),
            % write('3'),nl,
            breadthFirst(Node, FinalNode, _Path, _Cost),
            % write('4'),nl,
            k(nodeTeam(Step, FinalNode, Team)), 
            Team \= peronismo
        ), 
        Aumento
    ),
    foreach(
        member(X, Aumento),
        assert(b(posibleAumento(X)))
    ),
    
	retract((isGoal(_Node2, Cost):- !, Cost < 3)).