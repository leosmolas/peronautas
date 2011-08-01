%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Explorer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -prove
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%----------------------------------------------------------------------%

exec(Action) :- 
    listing(k),
    action(Action).

%----------------------------------------------------------------------%

reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.

reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    myEnergy(Energy),
    Energy >= Cost,
    !.

reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    currentStep(Step),
    write(2),nl,
    myEnergy(Energy),
    write(2.1),write(' energy: '),write(Energy),nl,
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    write(2.3),write(' position: '),write(Position),nl,
    k(nodeValue(Position, unknown)), 
    write(2.4),nl,
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    currentStep(Step),
    write(3),nl,
    myEnergy(Energy),
    write(3.1),nl,
    Energy > 0,
    write(3.2),nl,
    myPosition(Position),
    write(3.3),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.4),nl,
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
    write(4.4),nl,
    Energy >= Cost,
    write(4.5),nl,
    findall(
        [Node, Cost], 
        (
            k(edge(Position, Node, Cost)), 
            k(nodeValue(Node, unknown))
        ), 
        L),
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
    write(5.4),nl,
    Cost \= unknown,
    write(5.5),nl,
    Energy >= Cost, 
    write(5.6),nl,
    !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(6),nl.

