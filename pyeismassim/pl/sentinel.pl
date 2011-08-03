%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Sentinel                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

exec(Action) :- 
    action(Action).

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(1.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(1.3),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%

action([goto, X]) :-
    write(2.1),nl,
    position(Step, Name, Position),
    k(edge(Position, X, Cost)),
    write(2.2),nl,
    Cost \= unknown,
    write(2.3),nl,
    energy(Step, Name, Energy),
    Energy >= Cost, 
    write(2.4),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(3),nl.

