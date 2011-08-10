%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Repairer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -repair
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    action(Action).
    

        
rolMetas.



rolSetBeliefs.

%------------------------------  Repair  --------------------------------%

action([repair, Ally]):-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Ally, Position),
    write(1.3),nl,
    myName(Name),
    Ally \= Name,
    write(1.4),nl,
    myTeam(Team),
    team(Step, Ally, Team),
    write(1.5),nl,
    !.
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(2.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(2.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(2.3),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%

action([goto, X]) :-
    write(3.1),nl,
    myPosition(Position),
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(3.2),nl,
    myEnergy(Energy),
    Energy >= Cost, 
    write(3.3),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(4),nl.

