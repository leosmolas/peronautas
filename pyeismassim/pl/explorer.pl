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
	foreach(
        (
            k(nodeValue(Node, unknown))
        ), 
        assert(b(posibleProbear(Node)))
    ).
	
setInZone :-
	myTeam(MyTeam),
    currentStep(Step),
	foreach(k(nodeTeam(Step, Node, MyTeam)), assert(b(inZone(Node)) <- true)).
    
rolSetDifPuntos:-
    myName(A),
    myTeam(T),
    teamPoints(T, ActualPoints),
    foreach(
        b(posibleProbear(Node)),
        (
            doNotFail(
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
            searchPath(Position, Node, Energy, [[probe]], 1)
        )
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
    write(1),nl,
    action(Action).

%----------------------------------------------------------------------%

reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.

reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    myName(Name),
    currentStep(Step),
    energy(Step, Name, Energy),
    Energy >= Cost,
    !.

reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    currentStep(Step),
    myName(Name),
    write(2),write(' name: '),write(Name),nl,
    energy(Step, Name, Energy),
    write(2.1),write(' energy: '),write(Energy),nl,
    Energy > 0,
    write(2.3),write(' name: '),write(Name),nl,
    position(Step, Name, Position),
    write(2.4),write(' position: '),write(Position),nl,
    k(nodeValue(Position, unknown)), 
    write(2.5),nl,
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    currentStep(Step),
    write(3),nl,
    myName(Name),
    write(3.1),nl,
    energy(Step, Name, Energy),
    write(3.2),nl,
    Energy > 0,
    write(3.3),nl,
    position(Step, Name, Position),
    write(3.4),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.5),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Reachable Goto --%

action([goto, X]) :-
    currentStep(Step),
    write(4),nl,
    myName(Name),
    write(4.1),nl,
    position(Step, Name, Position),
    write(4.2),nl,
    k(nodeValue(Name, Cost)),
    write(4.3),nl,
    energy(Step, Name, Energy),
    write(4.5),nl,
    Energy >= Cost,
    findall(
        [Node, Cost], 
        (
            k(edge(Position, Node, Cost)), 
            k(nodeValue(Node, unknown))
        ), 
        L),
    write(4.4),nl,
    write(4.6),nl,
    reachableNode(X, L), 
    write(4.7),nl,
    !.
     
%-- First Node Goto --%

action([goto, X]) :-
    currentStep(Step),
    write(5),nl,
    myName(Name),
    write(5.1),nl,
    position(Step, Name, Position),
    write(5.2),nl,
    energy(Step, Name, Energy),
    write(5.3),nl,
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(5.4),nl,
    Energy >= Cost, 
    write(5.5),nl,
    !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(6),nl.

