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

execDummy(Action) :- 
    action(Action).


        
rolMetas.



rolSetBeliefs.
    
%-----------------------------------------------------------------------%

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
    
% %-------------------------------  Parry  --------------------------------%
% action([parry]) :-
%     myStatus(normal),
%     myPosition(Position),
%     myTeam(MyTeam),
%     currentStep(Step),
%     myEnergy(Energy),
%     Energy >= 2,
%     position(Step, Enemy, Position),
%     team(Enemy, EnemyTeam),
%     MyTeam \= EnemyTeam.
% 
    

%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    myStatus(normal),
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 0,
    write(1.2),nl,
    myPosition(Position),
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(1.3),nl,
    !.

%-----------------------------  Keep zone  ------------------------------%
action([recharge]) :-
    myStatus(normal),
    zoneScore(X),
    writeln('Keep calm and keep the zone! :D'),
    X > 40, !.

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

