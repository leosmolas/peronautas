%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -attack
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    write(1),nl,
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

%------------------------------  Attack  --------------------------------%

action([attack, Enemy]):-
    currentStep(Step),
    %obtengo mi nombre
    myName(Name),
    write(2),nl,
    %obtengo mi energia
    energy(Step, Name, Energy),
    %chequeo si puedo realizar la accion de ataque { cost(attack)=2 }
    write(2.1),write(' energy: '),write(X),nl,
    Energy > 1,
    %obtengo mi posicion
    write(2.2),nl,
    position(Step, Name, Position),
    %obtengo cual es mi equipo
    write(2.3),nl,
    team(Step, Name, Team),
    %obtengo el nombre de un agente que se encuentra en mi posicion
    write(2.4),nl,
    position(Step, Enemy, Position),
    Enemy \= Name,
    %veo que sea del otro equipo
    write(2.5),nl,
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
    %le saco la peluca
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

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    currentStep(Step),
    write(4),nl,
    %obtengo mi nombre, equipo, posicion y energia
    myName(Name),
    team(Step, Name, Team),
    write(4.1),nl,
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    %veo si las posiciones perimetrales existen agentes y son enemigos
    write(4.2),
    k(edge(Position, NeighborNode, Cost)), 
    Cost \= unknown, 
    Cost < Energy, write(' energy: '),write(Energy),nl,
    write(4.3),nl,
    position(Step, EnemyAgent, NeighborNode),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    write(4.4), write(' enemy: '),write(EnemyAgent),nl,
    %en caso de ser del equipo contrario, me muevo a esa posicion
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

