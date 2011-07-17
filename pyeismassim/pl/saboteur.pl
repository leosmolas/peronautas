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

exec(Action) :- 
    write(1),nl,
    action(Action).

%-----------------------------------------------------------------------%

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

%------------------------------  Attack  --------------------------------%

action([attack, Enemy]):-
    currentStep(Step),
    write(2),nl,
    %obtengo mi energia
    energy(X),
    %chequeo si puedo realizar la accion de ataque { cost(attack)=2 }
    write(2.1),write(' energy: '),write(X),nl,
    X>1,
    %obtengo mi nombre
    myName(Name),
    %obtengo mi posicion
    write(2.2),nl,
    k(position(Step, Name, Position)),
    %obtengo cual es mi equipo
    write(2.3),nl,
    agentTeam(Name, Team),
    %obtengo el nombre de un agente que se encuentra en mi posicion
    write(2.4),nl,
    k(position(Step, Enemy, Position)),
    Enemy \= Name,
    %veo que sea del otro equipo
    write(2.5),nl,
    agentTeam(Enemy, EnemyTeam),
    EnemyTeam \= Team,
    %le saco la peluca
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

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    currentStep(Step),
    write(4),nl,
    %obtengo mi nombre, equipo, posicion y energia
    myName(Name),
    agentTeam(Name, Team),
    write(4.1),nl,
    k(position(Step, Name, Position)),
    energy(E),
    %veo si las posiciones perimetrales existen agentes y son enemigos
    write(4.2),
    k(edge(Position, NeighborNode, Cost)), 
    Cost \= unknown, 
    Cost<E, write(' energy: '),write(E),nl,
    write(4.3),nl,
    k(position(Step, EnemyAgent, NeighborNode)),
    agentTeam(EnemyAgent, EnemyTeam),
    EnemyTeam \=Team,
    write(4.4), write(' enemy: '),write(EnemyAgent),nl,
    %en caso de ser del equipo contrario, me muevo a esa posicion
    !.

%-- First Node Goto --%

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

