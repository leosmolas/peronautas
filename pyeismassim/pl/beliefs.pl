:-  [utils], 
    % [kmap], 
    [graph/map], 
    [delp/arg].
    
:- dynamic b/1.

setBeliefs :-
    calcTime(setHaySaboteador),
    not(myRole(saboteur)),
    estoyEnPeligro, 
    writeln('estoy en Peligro!!!! :O'), !,
    myTeam(T),
    teamPoints(T, ActualPoints),
    assert(b(actualPoints(ActualPoints))),
    myPosition(Pos),
    myName(A),
    myTeam(T),
    myEnergy(Energy),
    foreach(
        k(edge(Pos, Neigh, _)),
        (
            calcTime(setDifPuntosNode(Neigh, A, T)),
            searchPath(Pos, Neigh, Energy, [], 0)
        )
    ),
    printFindAll('b', b(_)),
    printFindAll('b <- true', b(_) <- true).
    
setBeliefs :-
    
    myTeam(T),
    teamPoints(T, ActualPoints),
    assert(b(actualPoints(ActualPoints))), !,
	myPosition(MyPosition),
	assert(b(myPosition(MyPosition)) <- true), !,
    myStatus(Status),
    assert(b(myStatus(Status)) <- true), !,
	calcTime(setEsSeguro), !,
		
    % printFindAll('b', b(_)),
    calcTime(rolSetBeliefs), !,
    calcTime(setEstoyEnLaFrontera), !,
    % calcTime(setPosibleExpansion), !,
    calcTime(setAumento), !,
    calcTime(setPosibleExplorar), !,
    calcTime(setPosibleAuxilio), !,
	calcTime(setReagruparse), !,
    printFindAll('b', b(_)),
    printFindAll('b <- true', b(_) <- true).
    

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utils
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

estoyEnPeligro :-
    myStatus(disabled), !, fail.

estoyEnPeligro :-
    myPosition(Position),
    b(haySaboteador(Position)).
    
estoyEnPeligro :-
    mePegaron.

mePegaron :-
    b(meBajaronLaVida),
    not((
        lastAction(goto(_)),
        lastActionResult(successful)
    )),
    writeln(mePegaron).
    
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
    
notSecurePosition(Position) :-
	myTeam(MyTeam),
	currentStep(Step),
	team(Agent, Team),
	Team \= MyTeam,
	position(Step, Agent, Position),
	(
		role(Agent, saboteur) ;
		role(Agent, unknown)
	).
	
setEsSeguro :-
	foreach(
		notSecurePosition(Position),
		assertOnce(b(~esSeguro(Position)) <-  true) 
	).
    
saboteurPosition(Position) :-
	myTeam(MyTeam),
	currentStep(Step),
    team(Agent, Team),
	Team \= MyTeam,
	position(Step, Agent, Position),

	role(Agent, saboteur).

setHaySaboteador:-
	foreach(
		saboteurPosition(Position),
		assertOnce(b(haySaboteador(Position))) 
	).

setMuertos :-
	currentStep(Step),
	firstPerceivedStep(Step),
	writeln('setMuertos: caso firstPerceivedStep'),
	myTeam(MyTeam),
	assert(muertos(MyTeam, 0)),
	assert(muertos(enemigo, 0)), !.	
	
% setMuertos :-
	% currentStep(Step),
	% TotalMuertosEnemigos is Step // 5,
	% TotalMuertos is TotalMuertosEnemigos * 2,
	% myTeam(MyTeam),
	% retract(muertos(MyTeam, _MuertosActuales)),
	% retract(muertos(enemigo, _MuertosActualesEnemigos)),
	% assert(muertos(MyTeam, TotalMuertos)),
	% assert(muertos(enemigo, TotalMuertosEnemigos)), !.
	
	
setMuertos :-
	currentStep(Step),
	PreviousStep is Step - 1,
	myTeam(MyTeam),
	findall(
		Muerto,
		(
			team(Muerto, MyTeam),
			status(Step, Muerto, disabled),
			status(PreviousStep, Muerto, normal)			
		),
		Muertos
	),
	findall(
		MuertoEnemigo,
		(
			team(MuertoEnemigo, Team),
			Team \= MyTeam,
			status(Step, MuertoEnemigo, disabled),
			status(PreviousStep, MuertoEnemigo, normal)			
		),
		MuertosEnemigos
	),
	length(Muertos, MuertosEsteTurno),
	retract(muertos(MyTeam, MuertosActuales)),
	TotalMuertos is MuertosActuales + MuertosEsteTurno,
	assert(muertos(MyTeam, TotalMuertos)),
	length(MuertosEnemigos, MuertosEnemigosEsteTurno),
	retract(muertos(enemigo, MuertosActualesEnemigos)),
	TotalMuertosEnemigos is MuertosActualesEnemigos + MuertosEnemigosEsteTurno,
	assert(muertos(enemigo, TotalMuertosEnemigos)).

setMuertos.
    
% searchPath(_Position, Node, _Energy, ActionToBeDone, _CostOfAction) :-
    % (b(distancia(Node, ActionToBeDone, _PathCost, _RemainingEnergy)) <- true), !.
    
searchPath(Position, Node, Energy, ActionToBeDone, CostOfAction) :-
    pathSearch(Position, Node, Energy, ActionToBeDone, CostOfAction, _Path, _Actions, PathCost, RemainingEnergy), !,
    assert(b(distancia(Node, ActionToBeDone, PathCost, RemainingEnergy)) <- true).
    
searchPath(_Position, _Node, _Energy, _ActionToBeDone, _CostOfAction).
    

visibleNode(N) :-
    explored(N),
    inRange(N),
    foreach(
        k(edge(N, N1, _)),
        inRange(N1)
    ), !,
    retract(notVisible(N)),
    asserta(visibleNode(N)).
    

writeLenght(Name, Node, Pattern) :-
    verbose, !,
    findall(
        Node, 
        Pattern,
        ListExpansion
    ),
    length(ListExpansion, L1),
   
    write('<cant name="'), write(Name),write('" value='), write(L1), writeln('/>').
    
writeLenght(_Name, _Node, _Pattern).
    
% toogleOnVisibleNode(+Node)
% si el nodo ya estÃ¡ marcado como visible, no hace nada
% sino, hace el toogle
toogleOnVisibleNode(Node) :-
    visibleNode(Node), !.
    
toogleOnVisibleNode(Node) :-
    retractall(notVisible(Node)),
    asserta(visibleNode(Node)).
    
toogleOffVisibleNodes :-
    foreach(
        visibleNode(N),
        (
            retract(visibleNode(N)),
            assert(notVisible(N))
        )
   ).
    
agentsRangeVision(Step, MyTeam, Range, Position) :-
    team(Agent, MyTeam),
    visualRange(Step, Agent, Range),
    position(Step, Agent, Position),
    Range \= unknown.            % esto es un parche para cuando se corre sin servidor de percepciones, porque sino el rango del compañero es un dato que se deberña tener
    
% setExploredAndVisible
% predicado que setea como "exploredNode" a los nodos para los cuales conozco todos sus vecinos,
% y como visibleNode(Node) a los nodos a los que marque como explorados ESTE TURNO.
setExploredAndVisible :-
    currentStep(Step),
    myTeam(MyTeam),
    foreach(
        (
            agentsRangeVision(Step, MyTeam, Range, Position)
        ),
        (
            setExploredAndVisibleAux1(Range, Position)
        )
    ).

setExploredAndVisibleAux1(Range, Position) :-	
	retractall(isGoal(_, _)),
	retractall(isFail(_, _)),
	assert((isGoal(_Node2, Cost) :- !, Cost < Range)),
	assert((isFail(_Node3, Cost3) :- Cost3 >= Range)),	
	foreach(
		breadthFirst(Position, Node, _Path, _Cost),
		setExploredAndVisibleAux2(Node)		
	).
	% write('termine agente '),write(Agent),nl	
	
setExploredAndVisibleAux2(Node) :- 
	% write(' Marking node as explored: '),write(Node),nl,
	retractall(notExplored(Node)),
	assertOnce(explored(Node)),
	toogleOnVisibleNode(Node).
    
    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expansion
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setFrontera :-
    
    foreach(
        esFrontera(Node),
        assertOnce(b(frontera(Node)))
    ).
    
esFrontera(Node) :-
    currentStep(Step),
    myTeam(T),
    k(nodeTeam(Step, Node, T)),
    esFrontera2(Node).
    
esFrontera2(Node) :-
    currentStep(Step),
    myTeam(T),
    k(edge(Node, Neigh, _V)),
    k(nodeTeam(Step, Neigh, T2)),
    T2 \= T, !.

setEstoyEnLaFrontera :-
    setFrontera,
	myPosition(X),
    b(frontera(X)), !,
    assert(b(estoyEnLaFrontera)).
    
setEstoyEnLaFrontera.

% chequearFrontera(Neigh, T) :-
	% currentStep(Step),
    % member(Y, Neigh),
    % k(nodeTeam(Step, Y, Other)), 
	% Other \= T, !.

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

setDifPuntosNode(Node, A, T) :-
    
    b(actualPoints(ActualPoints)),
    
    setHypotheticalMap,
    moveAgent(A, Node),
    coloringAlgorithm,
    teamHPoints(T, Points),
    DifPuntos is Points - ActualPoints,                    
    assert(b(difPuntosZona(Node, DifPuntos)) <- true).
    
setDifPuntosNode(_Node, _A, _T).


setDifPuntosSinMi :-
	(b(difPuntosSinMi(_DifPuntos)) <- true), !.

setDifPuntosSinMi :-
    myStatus(disabled), !,
    assert(b(difPuntosSinMi(0)) <- true).

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
	
setDifPuntosSinMi.

    
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

setAumento :-
    myStatus(normal),
    myTeam(Team),
	currentStep(Step),
    retractall(isGoal(_, _)),
	retractall(isFail(_, _)),
    assert((
        isGoal(Node, Cost) :- 
            !, 
            Cost =< 2, 
            Cost > 0, 
            currentStep(Step),
            k(nodeTeam(Step, Node, none)),
            b(nodeAtDistance(Node, Distance)),
            Distance < 4
    )),
	assert((
        isFail(_, Cost2) :- 
            Cost2 > 2
    )),
    assert((
        isFail(Node, _) :- 
            currentStep(Step),
            myTeam(MyTeam),
            k(nodeTeam(Step, Node, MyTeam))
    )),
    setof(
        FinalNode,
        setPosibleAumentoAux(FinalNode, Step, Team),
        Aumento
    ),
    setPosibleAumento(0, Aumento),
    calcTime(setPosibleAumentoDistancia), !, 
    calcTime(setPosibleAumentoDifPuntos), !.
    
setAumento.

setPosibleAumento(6, _Aumento) :- !.

setPosibleAumento(_, _Aumento) :- 
    b(posibleAumento(_)), !.

setPosibleAumento(X, Aumento) :-
    NewCost is X + 2,
    foreach(
        posibleAumento(X, Aumento, Nodo),
        assert(b(posibleAumento(Nodo)))
    ), !,
    setPosibleAumento(NewCost, Aumento).
    
posibleAumento(X, Aumento, Nodo) :-
    myPosition(MyPosition),
    NewCost is X + 2,
    member(Nodo, Aumento),
    MyPosition \= Nodo,
    b(nodeAtDistance(Nodo, Dist)),
    Dist >= X,
    Dist < NewCost.

setPosibleAumentoAux(FinalNode, Step, MyTeam) :-
    currentStep(Step),	
	b(frontera(Node)),	
	position(Step, Agent, Node),
	team(Agent, MyTeam),
    b(nodeAtDistance(Node, Distance)),
	Distance =< 5,
    breadthFirst(Node, FinalNode, _Path, _Cost),
    k(nodeTeam(Step, FinalNode, Team)), 
    Team \= MyTeam.
    
setPosibleAumentoDifPuntos :-
    myName(A),
    myTeam(T),
    writeLenght(
        'posibleAumento', 
        Node, 
        (
            b(posibleAumento(Node)),
            not(b(difPuntosZona(Node, _)) <- true),
            (b(distancia(Node, [], _, _)) <- true)
        )
    ),
    foreach(
        (
            b(posibleAumento(Node5)),
            not(b(difPuntosZona(Node5, _DifPuntos3)) <- true),
            (b(distancia(Node5, [], _PathCost, _RemainingEnergy)) <- true)
        ),
        setDifPuntosNode(Node5, A, T)
    ).
    
setPosibleAumentoDistancia :-
    writeLenght(
        'posibleAumento', 
        Node13, 
        (
            b(posibleAumento(Node13))

        )
    ),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 7)),
    myEnergy(Energy),
    myPosition(Position),
    foreach(
        (
            b(posibleAumento(Node1)),
            not(b(distancia(Node1, [], _, _))<-true)
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
    
chequearPosibleExplorar(4) :- !.
    
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
            not(b(difPuntosZona(Node3, _DifPuntos1)) <- true),
            (b(distancia(Node3, [[survey]], _PathCost, _E)) <- true)
        )
    ),
    foreach(
        (
            b(posibleExplorar(Node1)),
            not(b(difPuntosZona(Node1, _DifPuntos2)) <- true),
            (b(distancia(Node1, [[survey]], Cost, _E2)) <- true),
            Cost < 3
        ),
        setDifPuntosNode(Node1, A, T)
    ),
    (b(distancia(_Node, [[survey]], Cost2, _E3)) <- true),
    Cost2 >= 3,
    setDifPuntosSinMi.

setDifPuntosExplorar.
    

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
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 7)),
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
    foreach(
        posibleAuxilio(Agent),
        assert(b(posibleAuxilio(Agent)))
    ),
    b(posibleAuxilio(_)), !,
    calcTime(setDifPuntosSinMi),
    calcTime(setDistanciaAuxilio).
    
setPosibleAuxilio.

posibleAuxilio(Agent) :-
    myName(MyName),
    myTeam(MyTeam),
    role(Agent, repairer),
    team(Agent, MyTeam),    
    Agent \= MyName.
    
setDistanciaAuxilio :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_)),
    assertAuxilioIsFail,
    foreach(
        (
            currentStep(Step),
            b(posibleAuxilio(Agent)),
            position(Step, Agent, FinalNode)
        ),
        (
            searchPath(Position, FinalNode, Energy, [], 0)
        )
    ).
    
assertAuxilioIsFail :-
    myStatus(normal),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 7)), !.
    
assertAuxilioIsFail.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reagruparse
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

equipoVecino(Step, MyPos, Team) :-
	k(edge(MyPos, Neigh, _)),
	k(nodeTeam(Step, Neigh, Team)).

setReagruparse :-
	currentStep(Step),
	firstPerceivedStep(Step), 
	writeln('setReagruparse: caso firstPerceivedStep'), !.
	
setReagruparse :-
	not(b(posibleAumento(_))),
	myPosition(MyPos),
	currentStep(Step),
	myTeam(MyTeam),
	foreach(
		equipoVecino(Step, MyPos, Team),
		Team \= MyTeam
	),
	% k(nodeTeam(Step, MyPos, Team)),
	% MyTeam \= Team, 
	assertReagruparseGoal,
	setPathReagruparse,
	setAgentesEnZona.
	
setReagruparse.

% Hay alguna zona, el goal es un nodo de mi color.
assertReagruparseGoal :- 
	currentStep(Step),
	myTeam(MyTeam),
	k(nodeTeam(Step, _Node, MyTeam)), !,
	retractall(isGoal(_)),
    assert((isGoal(ucsNode(FinalNode, _, _, _, _)) :- 
		currentStep(Step),
		myTeam(MyTeam),
		k(nodeTeam(Step, FinalNode, MyTeam)),
		equipoVecino(Step, FinalNode, MyTeam)
	)).

% No hay ninguna zona, el goal es un agente de mi equipo.
assertReagruparseGoal :- 
	retractall(isGoal(_)),
    assert((isGoal(ucsNode(FinalNode, _, _, _, _)) :- 
		currentStep(Step),
		myName(MyName),
		myTeam(MyTeam),
		position(Step, Agent, FinalNode),
		Agent \= MyName,
		team(Agent, MyTeam)		
	)).
	
setPathReagruparse :-
	myPosition(InitialNode),
	myEnergy(Energy),
    singleton_heap(InitialFrontier, 0, ucsNode(InitialNode, Energy, [], [], 0)),
    write('pathSearchReagruparse'),
    ucsAux(InitialFrontier, [], _Path, Actions, PathCost, _RemainingEnergy),     
	assert(b(distanciaAZona(PathCost)) <- true),
	write('distaciaAZona:'), writeln(PathCost),
    assert(b(pathReagruparse(Actions))).    

setAgentesEnZona :-
	currentStep(Step),
	myTeam(MyTeam),
	findall(
		Agent,
		agenteEnZona(Step, Agent, MyTeam),
		AgentesEnZona
	),
	length(AgentesEnZona, Cantidad),
	write('Agentes en zona:'), writeln(Cantidad),
	assert(b(agentesEnZona(Cantidad)) <- true).

agenteEnZona(Step, Agent, MyTeam) :-
	team(Agent, MyTeam),
	position(Step, Agent, Node),
	k(nodeTeam(Step, Node, MyTeam)),
	agenteEnZonaAux(Step, Node, MyTeam).
	
agenteEnZonaAux(Step, Node, MyTeam) :-
	equipoVecino(Step, Node, MyTeam), !.
		

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Defensa
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkLife :-
    currentStep(Step),
    PreviousStep is Step - 1,
    myName(MyName),
    myHealth(HealthNow),
    health(PreviousStep, MyName, HealthBefore),
    HealthNow < HealthBefore,
    assert(b(meBajaronLaVida)),
    assertSaboteurs, !.
    
checkLife :- !.

assertSaboteurs :-  
    lastActionResult(successful),
    lastAction(goto(_)), !.

assertSaboteurs :-  
    myPosition(Pos),
    currentStep(Step),
    position(Step, Saboteur, Pos),
    role(Saboteur, saboteur), !.
    
assertSaboteurs :-
    myPosition(Pos),
    currentStep(Step),
    findall(
        Agent,
        (
            position(Step, Agent, Pos),
            role(Agent, Role),
            Role = unknown
        ),
        [Saboteur]
    ),
    
    assertOnce(k(agentRole(Saboteur, saboteur))).
    
