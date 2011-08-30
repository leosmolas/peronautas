%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Saboteur                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -inspect
%                   -survey
%                   -goto
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

execDummy(Action) :- 
    write(1),nl,
    action(Action).

        
rolMetas.

rolSetBeliefs :-
    myStatus(normal), !,
    calcTime(setPosibleInspectar),
    rolSetDistancia,
    setDifPuntosSinMi,
    printFindAll('beliefs inspector', b(posibleInspectar(Agent, PosicionAgente))).

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

%-----------------------------------------------------------------------%

vecinos(Node,Vecinos):-
  findall(
  NuevoVecino,
  k(edge(Node, NuevoVecino, _Valor)),
  Vecinos
  ).    

buscarEnemigos(Posicion, CantEnemigos):-
  currentStep(Step),
  myTeam(MyTeam),
  findall(
  NuevoVecino
  ,(
    k(edge(Posicion, NuevoVecino, _Valor))
    position(Step, Agent, Posicion),
    team(Agent, Team),
    MyTeam \= Team
  ),
  Vecinos
  ),
  length(Vecinos, CantEnemigos).


%-----------------------------------------------------------------------%

setPosibleInspectar :-
    myStatus(normal),
    currentStep(Step),
    enemigosPosicion(Step, Posiciones),
    foreach(
      (
        member(par(PosicionAgente, Agent), Posiciones)
      )
      ,
      (
        vecinos(PosicionAgente, LVecinosPosicion),
        buscarEnemigos(PosicionAgente, CantEnemigos),
        CantEnemigosConAgenteVisible is CantEnemigos + 1,
        foreach(
            member(Vecino, LVecinosPosicion)
            ,(
              buscarEnemigos(Vecino, CantEnemigosVecino),
              CantEnemigosConAgenteVecino is CantEnemigosVecino + 1,
              assert(b(posibleInspectar(Agent, Vecino))),
              assert(b(posibleInspectarConEnemigos(Agent, Vecino, CantEnemigosConAgenteVecino))<-true)
            )
        ),
        assert(b(posibleInspectar(Agent, PosicionAgente, CantEnemigosConAgenteVisible)))
      )
    ).
    
setPosibleInspectar.

enemigosPosicion(Step, Posiciones):-
    myTeam(MyTeam),
    findall(
      par(Position, Agent)
    ,
    (
      team(Agent, Team),
      MyTeam \= Team,
      position(Step, Agent, Position)
    )
    ,
      Posiciones
    ).

rolSetDistancia :-
    myName(Name),
    currentStep(Step),
    position(Step, Name, Position),
    energy(Step, Name, Energy),
    retractall(isFail(_)),
    assert((isFail(ucsNode(_, _, _, _, Path_Cost)) :- Path_Cost > 10)),
    foreach(
        b(posibleInspectar(_Enemy, Node)),
        (
            searchPath(Position, Node, Energy, [[inspect]], 2)
        )
    ),
    printFindAll('paths', b(path(_InitialNode, _FinalNode, _Energy, _Path, _Plan, _NewTurns2, _RemainingEnergy1, _))).

%------------------------------  Attack  --------------------------------%

action([inspect]):-
    write(1.1),nl,
    myEnergy(Energy),
    Energy > 1,
    write(1.2),nl,
    currentStep(Step),
    myPosition(Position),
    position(Step, Enemy, Position),
    write(1.3),nl,
    myName(Name),
    Enemy \= Name,
    write(1.4),nl,
    myTeam(Team),
    team(Step, Enemy, EnemyTeam),
    EnemyTeam \= Team,
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

%-- Barbarian Goto --%

action([goto, NeighborNode]) :-
    write(3.1),nl,
    myPosition(Position),
    k(edge(Position, NeighborNode, Cost)), 
    write(3.2),nl,
    Cost \= unknown, 
    write(3.3),nl,
    myEnergy(Energy),
    Cost < Energy, 
    write(3.4),nl,
    currentStep(Step),
    position(Step, EnemyAgent, NeighborNode),
    write(3.5),nl,
    myTeam(Team),
    team(Step, EnemyAgent, EnemyTeam),
    EnemyTeam \= Team,
    write(3.6),nl,
    !.

%-- First Node Goto --%

action([goto, X]) :-
    write(4.1),nl,
    myPosition(Position),
    k(edge(Position, X, Cost)),
    write(4.2),nl,
    Cost \= unknown,
    write(4.3),nl,
    myEnergy(Energy),
    Energy >= Cost, 
    write(4.4),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(5),nl.

%-------------------------------  Old Code  ------------------------------%

% exec(Action) :- action(Action).

% action(inspect(Agent)) :-
   % energy(X),
   % X > 1,
   % my_name(Name),
   % k(position(Name,  Position)),
   % k(position(Agent, Position)),
   % teamOfAgent(Agent, Team),
   % Team \= d3lp0r, !.

% action(goto(Vertex)) :-
   % % Random walking
   % % select a neighbouring vertex
   % Vertex = something.

% action(recharge).

