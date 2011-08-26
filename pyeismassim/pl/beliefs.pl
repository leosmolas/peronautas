% posibleExpansion(Nodo)

% estoyEnLaFrontera

:-  [utils], 
    % [kmap], 
    [graph/map], 
    [delp/arg].
    
:- dynamic b/1.

% setBeliefs :-
    % currentStep(0), !.
    
setNodesAtDistance(Distance) :-
	myPosition(Node),
	retractall(isGoal(_, _)),
	retractall(isFail(_, _)),
	assert(isGoal(_Result, _Cost)),
	assert(isFail(_Result2, Cost2) :- Cost2 > Distance),
	foreach(
		breadthFirst(Node, Dest, _Path, Cost3),
		assert(b(nodeAtDistance(Dest, Cost3)))
	).
	
    
setBeliefs :-
    
    write('setBeliefs'),nl,
    myTeam(T),
    teamPoints(T, ActualPoints),
    assert(b(actualPoints(ActualPoints))), !,
	myPosition(MyPosition),
	assert(b(myPosition(MyPosition)) <- true), !,
    myStatus(Status),
    assert(b(myStatus(Status)) <- true), !,
	calcTime(setEsSeguro), !,
    printFindAll('b', b(_)),
    calcTime(rolSetBeliefs), !,
    calcTime(setEstoyEnLaFrontera), !,
    calcTime(setPosibleExpansion), !,
    calcTime(setPosibleAumento), !,
    calcTime(setPosibleExplorar), !,
    calcTime(setPosibleAuxilio), !,
    printFindAll('setDifPuntos', b(difPuntosZona(_N, _D)) <- true),
    printFindAll('b', b(_)),
    printFindAll('setDistancia', b(distancia(_Node, _A, _PathCost, _)) <- true).

saboteurPosition(Position) :-
	myTeam(MyTeam),
	currentStep(Step),
	position(Step, Agent, Position),
	team(Agent, Team),
	Team \= MyTeam,
	(
		role(Agent, saboteur) ;
		role(Agent, unknown)
	).
	
setEsSeguro :-
	foreach(
		saboteurPosition(Position),
		assert(b(~esSeguro(Position)) -<  equal(1,1)) % truchada para que la especifidad agarre este
	).
	
    
% searchPath(_Position, Node, _Energy, ActionToBeDone, _CostOfAction) :-
    % (b(distancia(Node, ActionToBeDone, _PathCost, _RemainingEnergy)) <- true), !.
    
searchPath(Position, Node, Energy, ActionToBeDone, CostOfAction) :-
	% writeln('pathSearch'),
    pathSearch(Position, Node, Energy, ActionToBeDone, CostOfAction, _Path, _Actions, PathCost, RemainingEnergy), !,
	% printFindAll('paths', b(path(_X1,_X2,_X3,_X4,_X5,_X6,_X7,_X8))),
    % writeln('6.2'),nl,
    assert(b(distancia(Node, ActionToBeDone, PathCost, RemainingEnergy)) <- true).
    % writeln('6.3'),nl.
    
searchPath(_Position, _Node, _Energy, _ActionToBeDone, _CostOfAction).
    
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
    % writeln('1'),nl,
	myPosition(X),
    % lastKnownPosition(_Step, A, X),
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
    myPosition(X),
    myTeam(T),	
    myStatus(normal),
    k(nodeTeam(_S2, X, T)),
    currentStep(Step),
    foreach(
        (
            k(edge(X, Neigh, _)), 
            k(nodeTeam(Step, Neigh, none))
        ), 
        assert(b(posibleExpansion(Neigh)))
    ),
    calcTime(setDistanciaExpansion),
    calcTime(setDifPuntosExpansion).
    
setPosibleExpansion.

setDistanciaExpansion :-
    myPosition(Position),
    myEnergy(Energy),
	writeLenght(
        'posibleExpansion', 
        Node12, 
        (
            b(posibleExpansion(Node12))
        )
    ),
	printFindAll('b(posibleExpansion(Node))', b(posibleExpansion(_Node))),
    retractall(isFail(_)),
    foreach(
        (
            b(posibleExpansion(Node))
        ),
        (
            searchPath(Position, Node, Energy, [], 0)
        )
    ), !.
    
setDistanciaExpansion.

writeLenght(Name, Node, Pattern) :-
    findall(
        Node, 
        Pattern,
        ListExpansion
    ),
    length(ListExpansion, L1),
   
    write('<cant name="'), write(Name),write('" value='), write(L1), writeln('/>').
    
setDifPuntosNode(Node, A, T) :-
    
    % write('Node: '), writeln(Node),
    

    % currentStep(Step),
    % myName(Name),
    % concat('logs/', Name, S2),
    % concat(S2, '-', S3),
    % concat(S3, Step, S0),
    % concat(S0, Node, S1),
    % concat(S1, '.pl', File),
    % writeln(File),
    % saveMap(File),
    b(actualPoints(ActualPoints)),

    setHypotheticalMap,
    moveAgent(A, Node),
    coloringAlgorithm,
    teamHPoints(T, Points),
    DifPuntos is Points - ActualPoints,                    
    % write('Points: '), writeln(Points),
    assert(b(difPuntosZona(Node, DifPuntos)) <- true).
    
setDifPuntosNode(_Node, _A, _T).

setDifPuntosSinMi :-
	(b(difPuntosSinMi(_DifPuntos)) <- true), !.

setDifPuntosSinMi :-
    
    b(actualPoints(ActualPoints)),

    setHypotheticalMap,
	currentStep(Step),
    myName(Agent),
    retract(h(position(Step, Agent, _))),
    coloringAlgorithm,
	myTeam(T),
    teamHPoints(T, Points),
    DifPuntos is Points - ActualPoints,                    
    % write('Points: '), writeln(Points),
    assert(b(difPuntosSinMi(DifPuntos)) <- true).
	
setDifPuntosSinMi(_A, _T).

    
setDifPuntosExpansion :-
    myName(A),
    myTeam(T),
    writeLenght(
        'posibleExpansion', 
        Node4, 
        (
            b(posibleExpansion(Node4)),
            not(b(difPuntosZona(Node4, _DifPuntos1)) <- true)
        )
    ),
    foreach(
        (
            b(posibleExpansion(Node)),
            not(b(difPuntosZona(Node, _DifPuntos2)) <- true)
        ),
        setDifPuntosNode(Node, A, T)
    ).


	
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Aumento
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleAumento :-
    myStatus(normal),
    myTeam(Team),
	currentStep(Step),
    retractall(isGoal(_, _)),
	retractall(isFail(_, _)),
	assert((isGoal(_Node, Cost) :- !, Cost =< 2, Cost > 0)),
    setof(
        FinalNode,
        setPosibleAumentoAux(FinalNode, Step, Team),
        Aumento
    ),
    myPosition(Position), !,
    foreach(
        (
            member(X, Aumento),
            Position \= X,
			b(nodeAtDistance(X, _))
        ),
        assert(b(posibleAumento(X)))
    ), !,
    calcTime(setPosibleAumentoDifPuntos), !,
    calcTime(setPosibleAumentoDistancia).
    
setPosibleAumento.

setPosibleAumentoAux(FinalNode, Step, MyTeam) :-
    b(frontera(Node)),	
    breadthFirst(Node, FinalNode, _Path, _Cost),
    k(nodeTeam(Step, FinalNode, Team)), 
    Team \= MyTeam.
    
setPosibleAumentoDifPuntos :-
    myName(A),
    myTeam(T),
    foreach(
        (
            b(posibleAumento(Node5)),
            not(b(difPuntosZona(Node5, _DifPuntos3)) <- true)
        ),
        setDifPuntosNode(Node5, A, T)
    ).
    
setPosibleAumentoDistancia :-
    writeLenght(
        'posibleAumento', 
        Node13, 
        (
            b(posibleAumento(Node13)),
            (b(difPuntosZona(Node13, DifPuntos13)) <- true),
            DifPuntos13 > 0
        )
    ),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    myEnergy(Energy),
    myPosition(Position),
    foreach(
        (
            b(posibleAumento(Node1)),
            (b(difPuntosZona(Node1, DifPuntos1)) <- true),
            DifPuntos1 > 0
        ),
        (
            searchPath(Position, Node1, Energy, [], 0)

        )
    ).
    



    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explorar
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleExplorar :-
    myStatus(normal),
    chequearPosibleExplorar(0),
    b(posibleExplorar(_Node)), !,
    calcTime(setDistanciaExplorar),
    calcTime(setDifPuntosExplorar).
    
    
setPosibleExplorar. 
    
chequearPosibleExplorar(6) :- !.
    
chequearPosibleExplorar(_) :-
    b(posibleExplorar(_Node)), !.
    

    
chequearPosibleExplorar(X) :-
    NewCost is X + 2,
	foreach(
        posibleExplorar(X, NewCost, FinalNode), 
        assertOnce(b(posibleExplorar(FinalNode)))
    ),
    chequearPosibleExplorar(NewCost).
    
posibleExplorar(X, NewCost, FinalNode) :-
    b(nodeAtDistance(FinalNode, Cost)),
    notExplored(FinalNode),
    Cost >= X, 
    Cost < NewCost.
    
posibleExplorar(X, NewCost, FinalNode) :-
    b(nodeAtDistance(FinalNode, Cost)),
    hasAtLeastOneUnsurveyedEdge(FinalNode),
    Cost >= X, 
    Cost < NewCost.

setDifPuntosExplorar :-
    myName(A),
    myTeam(T),
    writeLenght(
        'posibleExplorar', 
        Node3, 
        (
            b(posibleExplorar(Node3)),
            not(b(difPuntosZona(Node3, _DifPuntos1)) <- true)
        )
    ),
    foreach(
        (
            b(posibleExplorar(Node1)),
            not(b(difPuntosZona(Node1, _DifPuntos2)) <- true),
            (b(distancia(Node1, [[survey]], _PathCost, _E)) <- true)
        ),
        setDifPuntosNode(Node1, A, T)
    ).
    

setDistanciaExplorar :-
    myPosition(Position),
    myEnergy(Energy),
	writeLenght(
        'posibleExplorar', 
        Node12, 
        (
            b(posibleExplorar(Node12))
        )
    ),
	printFindAll('b(posibleExplorar(Node))', b(posibleExplorar(_Node))),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        (
            b(posibleExplorar(Node))
        ),
        (
            searchPath(Position, Node, Energy, [[survey]], 1)
        )
    ), !.
    
setDistanciaExplorar.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxilio
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setPosibleAuxilio :-
    myHealth(Health),
    myMaxHealth(Max),
    Health < Max,
    writeln(1),
    foreach(
        (
            myTeam(MyTeam),
            team(Agent, MyTeam),
            role(Agent, repairer)
        ),
        assert(b(posibleAuxilio(Agent)))
    ),
    writeLenght(
        'posibleAuxilio', 
        Agent1, 
        (
            b(posibleAuxilio(Agent1))
        )
    ),
    b(posibleAuxilio(_)), !,
    setDifPuntosSinMi,
    writeln(2),
    setDistanciaAuxilio.
    
setPosibleAuxilio.
    
setDistanciaAuxilio :-
    myPosition(Position),
    myEnergy(Energy),
    writeln(3),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)), !,
    writeln(4),
    foreach(
        (
            currentStep(Step),
            b(posibleAuxilio(Agent)),
            position(Step, Agent, FinalNode)
        ),
        (
            searchPath(Position, FinalNode, Energy, [], 0)
        )
    ),
    writeln(5), !.
    
setDistanciaAuxilio.