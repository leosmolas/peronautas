%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Explorer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['utils/explorer.delp'].

rolMetas:-
    foreach(
        b(posibleProbear(N)), 
        doNotFail(calcMeta(probear(N)))
    ).

    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Probear
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rolSetBeliefs :-
    myStatus(normal), !,
    calcTime(setPosibleProbear),
    calcTime(setInZone),
    calcTime(rolSetDistancia),
    calcTime(rolSetDifPuntos),
    calcTime(setPromedioValorVecinos).
    
rolSetBeliefs.

setPosibleProbear :- 
    chequearPosibleProbear(0).
 
chequearPosibleProbear(6) :- !.
 
chequearPosibleProbear(_) :-
    b(posibleProbear(_FinalNode)), !.
    
chequearPosibleProbear(X) :-
    NewCost is X + 2,
	foreach(
        posibleProbear(X, NewCost, FinalNode), 
        assertOnce(b(posibleProbear(FinalNode)))
    ),
    chequearPosibleProbear(NewCost).

posibleProbear(X, NewCost, FinalNode) :-
    
    b(nodeAtDistance(FinalNode, Cost)),
    k(nodeValue(FinalNode, unknown)), 
    Cost >= X, 
    Cost < NewCost.
	
setInZone :-
	myTeam(MyTeam),
    currentStep(Step),
	foreach(k(nodeTeam(Step, Node, MyTeam)), assert(b(inZone(Node)) <- true)).
    
rolSetDifPuntos:-
    myName(A),
    myTeam(T),
    writeLenght(
        'posibleProbear dif puntos', 
        Node1, 
        posibleProbearDif(Node1)
    ),
    foreach(
        posibleProbearDif(Node),
        setDifPuntosNode(Node, A, T)
    ),
    rolSetDifPuntosSinMi.
    
rolSetDifPuntosSinMi :-
    b(posibleProbear(Node)),
    (b(distancia(Node, [[probe]], PathCost, _RemainingEnergy)) <- true),
    PathCost >= 3, 
    setDifPuntosSinMi, !.
    
rolSetDifPuntosSinMi.
    
posibleProbearDif(Node) :-
    b(posibleProbear(Node)),
    (b(distancia(Node, [[probe]], PathCost, _RemainingEnergy)) <- true),
    PathCost < 3.
        
rolSetDistancia :-
    myPosition(Position),
    myEnergy(Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        b(posibleProbear(Node)),
        searchPath(Position, Node, Energy, [[probe]], 1)

    ).

setPromedioValorVecinos :-
    foreach(
        (
            b(posibleProbear(Node)),
            (b(distancia(Node, [[probe]], _Cost, _E2)) <- true)
        ),
        calcPromedioValor(Node)
    ).
    
calcPromedioValor(Node) :-
    findall(
        Value,
        (
            k(edge(Node, Neigh, _)),
            k(nodeValue(Neigh, Value)),
            Value \= unknown
        ),
        [L | List]
    ),
    
    average([L | List], Promedio),
    assert(b(promedioValorVecinos(Node, Promedio)) <- true), !.
    
calcPromedioValor(_Node).
            
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -prove
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action).

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    myStatus(normal),
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(1.2),nl,
    myPosition(Position),
    k(nodeValue(Position, unknown)), 
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    myStatus(normal),
    write(2.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    !.

%-----------------------------  Keep zone  ------------------------------%
action([recharge]) :-
    myStatus(normal),
    zoneScore(X),
    X > 40, !.

%-------------------------------  Goto  ---------------------------------%

%-- Goto First Reachable Node --%

action([goto, X]) :-
    myEnergy(Energy),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    Energy >= Cost,
    k(nodeValue(X, unknown)), !.
     
%-- Goto First Node --%

action([goto, X]) :-
    write(4.1),nl,
    myEnergy(Energy),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    write(4.2),nl,
    Cost \= unknown,
    write(4.3),nl,
    Energy >= Cost, 
    write(4.4),nl, !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(5),nl.

