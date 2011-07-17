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
    write(1),nl,
    action(Action).

%----------------------------------------------------------------------%

reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.
reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    energy(X),
    X >= Cost,
    !.
reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).

%------------------------------  Probe  --------------------------------%

% si tenemos suficiente energia y 
% no conocemos el valor del nodo, hacemos probe
action([probe, Position]) :-
    currentStep(Step),
    write(2),nl,
    energy(X),
    write(2.1),write(' energy: '),write(X),nl,
    X > 0,
    write(2.2),nl,
    myName(Name),
    write(2.3),write(' name: '),write(Name),nl,
    k(position(Step, Name, Position)),
    write(2.4),write(' position: '),write(Position),nl,
    k(nodeValue(Position, unknown)), 
    write(2.5),nl,
    !.

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    currentStep(Step),
    write(3),nl,
    energy(X),
    write(3.1),nl,
    X > 0,
    write(3.2),nl,
    myName(Name),
    write(3.3),nl,
    k(position(Step, Name, Position)),
    write(3.4),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.5),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

action([goto, X]) :-
    currentStep(Step),
    write(4),nl,
    myName(Name),
    write(4.1),nl,
    k(position(Step, Name, Position)),
    write(4.2),nl,
    findall(
        [Node, Cost], 
        (
            k(edge(Position, Node, Cost)), 
            k(nodeValue(Node, unknown))
        ), 
        L),
    write(4.3),nl,
    reachableNode(X, L), 
    write(4.4),nl,
    !.
     
%-------------------------------  Goto  ---------------------------------%

action([goto, X]) :-
    currentStep(Step),
    write(5),nl,
    myName(Name),
    write(5.1),nl,
    k(position(Step, Name, Position)),
    write(5.2),nl,
    energy(E),
    write(5.3),nl,
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(5.4),nl,
    E >= Cost, 
    write(5.5),nl,
    !.

%-------------------------------  Recharge  ------------------------------%

action([recharge]) :-
    write(6),nl.

