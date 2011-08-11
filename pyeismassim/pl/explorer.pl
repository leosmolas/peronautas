%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Explorer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['delp/explorer.delp'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parte de argumentacion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rolMetas:-
    foreach(b(posibleProbear(N)), doNotFail(calcMeta(probear(N)))).

    
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Probear
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% probed(vertex0).
% probed(vertex1).
% probed(vertex3).

rolSetBeliefs :-
    calcTime('setPosibleProbear',
    setPosibleProbear),
    calcTime('rolSetDifPuntos',
    rolSetDifPuntos),
    calcTime('rolSetDistancia',
    rolSetDistancia).

setPosibleProbear :- 
    currentStep(Step),
    myName(Name),
    position(Step, Name, Position),
    % writeln('1'),
    retractall(isGoal(_, _)),
    assert((isGoal(Node, Cost) :- k(nodeValue(Node, unknown)), Cost < 2)),
    % writeln('2'),
	foreach(
        (
            % writeln('2.5'),
            breadthFirst(Position, FinalNode, _Path, _Cost)
        ), 
        assert(b(posibleProbear(FinalNode)))
    ),
    % writeln('3'),
    chequearPosibleProbear(2).
    
chequearPosibleProbear(_) :-
    b(posibleProbear(_FinalNode)), !.
    
chequearPosibleProbear(X) :-
    currentStep(Step),
    myName(Name),
    position(Step, Name, Position),
    writeln('4'),
    retractall(isGoal(_, _)),
    NewCost is X + 2,
    assert((isGoal(Node, Cost) :- k(nodeValue(Node, unknown)), Cost >= X, Cost < NewCost)),
    writeln('5'),
	foreach(
        (
            breadthFirst(Position, FinalNode, _Path, _Cost)
        ), 
        assert(b(posibleProbear(FinalNode)))
    ),
    writeln('6'),
    chequearPosibleProbear(NewCost).

	
setInZone :-
	myTeam(MyTeam),
    currentStep(Step),
	foreach(k(nodeTeam(Step, Node, MyTeam)), assert(b(inZone(Node)) <- true)).
    
rolSetDifPuntos:-

    myName(A),
    myTeam(T),
    teamPoints(T, ActualPoints),
    writeLenght(
        'posibleProbear', 
        Node1, 
        (
            b(posibleProbear(Node1))
        )
    ),
    foreach(
        b(posibleProbear(Node)),
        (
            doNotFail(
                (   setHypotheticalMap,
                    moveAgent(A, Node),
                    coloringAlgorithm,
                    teamHPoints(T, Points),
                    DifPuntos is Points - ActualPoints,
                    assert(b(difPuntosZona(Node, DifPuntos)) <- true)
                )
            )
        )
    ).
    
rolSetDistancia :-
    myName(Name),
    % writeln('2'),nl,
    currentStep(Step),
    % writeln('3'),nl,
    position(Step, Name, Position),
    % writeln('4'),nl,
    energy(Step, Name, Energy),
    foreach(
        b(posibleProbear(Node)),
        (
            % writeln('6.1'),nl,
            % writeln(Node),nl,
			retractall(isFail(_)),
			assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
            searchPath(Position, Node, Energy, [[probe]], 1)
        )
    ),
    printFindAll('paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1))).
    
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
    action(Action).

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(1.2),nl,
    myPosition(Position),
    k(nodeValue(Position, unknown)), 
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(2.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    !.

%-------------------------------  Goto  ---------------------------------%

%-- Goto First Reachable Node --%

%action([goto, X]) :-
%    write(3.1),nl,
%    myPosition(Position),
%    k(nodeValue(Position, Cost)),
%    write(3.2),nl,
%    myEnergy(Energy),
%    write([myEnergy,Energy,cost,Cost]),nl,
%    Energy >= Cost,
%    write(3.3),nl,
%    findall(
%        [Node, Cost], 
%        (
%            k(edge(Position, Node, Cost)), 
%            k(nodeValue(Node, unknown))
%        ), 
%        Nodes),
%    write(3.4),nl,
%    reachableNode(X, Nodes), 
%    write(3.5),nl,
%    !.
     
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
    write(4.4),nl,
    !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(5),nl.

