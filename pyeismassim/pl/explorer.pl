%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Explorer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['delp/explorer.delp'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parte de argumentacion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    calcTime(rolSetDifPuntos).
    
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
    PathCost >= 3, !,
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
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -prove
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action),
    nl.

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    write(1.1),write(', '),
    myEnergy(Energy),
    Energy > 0,
    write(1.2),write(', '),
    myPosition(Position),
    k(nodeValue(Position, unknown)), 
    write(1.3),
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(2.1),write(', '),
    myEnergy(Energy),
    Energy > 0,
    write(2.2),write(', '),
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position),
    write(2.3),
    !.

%-------------------------------  Goto  ---------------------------------%
%-- Goto First Node --%

action([goto, X]) :-
    write(4.1),write(', '),
    myEnergy(Energy),
    myPosition(Position),
    k(edge(Position, X, Cost)),
    write(4.2),write(', '),
    Cost \= unknown,
    write(4.3),write(', '),
    Energy >= Cost, 
    write(4.4),
    !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(5).

