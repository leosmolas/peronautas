:- [graph/map], [kmap].

% ucs(-Frontier, -Visited, +Path, +Actions, +Path_Cost)
% Frontier: frontera del recorrido.
% Visited: conjunto de visitados.
% Energy : energia actual
% Path: camino retornado.
% Actions: lista de acciones necesarias para llegar al nodo.
% Path_Cost: costo del camino encontrado.
ucs(Frontier, _Visited, _Energy, [Position | Path], Actions, Path_Cost) :-
    ucsSelect(Frontier, ucsNode(Position, Path, Actions, Path_Cost), _Frontier1),
    isGoal(Position).

ucs(Frontier, Visited, Energy, SolutionPath, SolutionActions, Cost) :-
    ucsSelect(Frontier, SelectedNode, Frontier1),
    ucsNeighbors(SelectedNode, Energy, Neighbors),
    % add_paths(Neighbours, Selected_Node, Neighbours1),
    addToFrontier(Neighbors, Frontier1, FrontierNew, Visited, NewVisited),
    ucs(FrontierNew, [SelectedNode | NewVisited], Energy, SolutionPath, SolutionActions, Cost).

ucsSelect([Node | Frontier], Node, Frontier).
    
% addToFrontier(-Neighbors, -Frontier, +FrontierNew, -Visited, +VisitedNew).

addToFrontier([], Frontier, Frontier, Visited, Visited).

addToFrontier([Neighbor | Neighbors], OldFrontier, Frontier, OldVisited, Visited) :-
     check(Neighbor, OldFrontier, NewFrontier, OldVisited, NewVisited), !,
     addToFrontier(Neighbors, NewFrontier, Frontier, NewVisited, Visited).
        
addToFrontier([Neighbor | Neighbors], OldFrontier, Frontier, OldVisited, Visited) :-
    insert_pq(Neighbor, Frontier, NewFrontier),
    addToFrontier(Neighbors, NewFrontier, Frontier, OldVisited, Visited).
    
% insert_list_pq([],Q,Q).
% insert_list_pq([H|T],Q,NewQ) :-
    % insert_pq(H,Q,Q2),
    % insert_list_pq(T,Q2,NewQ).

    
% check/5
% check(+Node, Old_Frontier, New_Frontier, Old_Visited, New_Visited)
%
% Verifica si se da alguna de las siguientes condiciones para un nodo dado:
%
% (1) Si existe en F un nodo N etiquetado con una posicion P, y generamos P por un mejor camino que 
% el representado por N, % entonces reemplazamos N en F por un nodo N' para P con este nuevo camino.
% (2) Si existe en V un nodo N etiquetado con una posicion P, y generamos P por un mejor camino que 
% el representado por N, entonces N es eliminado de V y se agrega a la frontera un nuevo nodo N' 
% para P con este nuevo camino.

check(ucsNode(Position, Path, Actions, Path_Cost), OldFrontier, NewFrontier, Visited, Visited) :-
    member(ucsNode(Position, _, _, Path_Cost2), OldFrontier),
    Path_Cost < Path_Cost2, !,
    delete(OldFrontier, ucsNode(Position, _, _, Path_Cost2), Frontier),
    insert_pq(ucsNode(Position, Path, Actions, Path_Cost), Frontier, NewFrontier).
    
check(ucsNode(Position, Path, Actions, Path_Cost), OldFrontier, NewFrontier, OldVisited, NewVisited) :-
    member(ucsNode(Position, _, _, Path_Cost2), OldVisited),
    Path_Cost < Path_Cost2, !,
    delete(OldVisited, ucsNode(Position, _, _, Path_Cost2), NewVisited),
    insert_pq(ucsNode(Position, Path, Actions, Path_Cost), OldFrontier, NewFrontier).
    

insert_pq(State, [], [State]) :- !.
insert_pq(State, [H | T], [State, H | T]) :-
    precedes(State, H), !.
insert_pq(State, [H|T], [H | T_new]) :-
    insert_pq(State, T, T_new).
    
precedes(ucsNode(_Position, _Path, _Actions, PathCost), ucsNode(_Position2, _Path2, _Actions2, PathCost2)) :-
    PathCost < PathCost2, !.


% ucsNeighbors(-UcsNode, -Energy, +Neighbors)
% UcsNode: el nodo actual.
% Energy: energia actual.
% Neighbors: lista con un ucsNodes por cada vecino.
ucsNeighbors(ucsNode(Position, Path, Actions, Path_Cost), Energy, Neighbors) :-
    kneighbors(Position, Neigh),
    calcActions(Position, Energy, Neigh, ListOfActions),
    calcUcsNodes(Position, Path, Actions, Path_Cost, ListOfActions, Neighbors).

% calcUcsNodes(-Position, -Path, -Actions, -PathCost, -ListOfCalcActions, +ListOfUcsNodes)
% Position: posicion actual
% Path: camino a la posicion actual
% Actions: lista de acciones hasta la posicion actual
% PathCost: costo del camino hasta la posicion actual
% ListOfCalcActions: lista que contiene, para cada vecino, una lista de la forma [nombre, turnos que lleva llegar, energía restante, lista de acciones necesarias].
% ListOfUcsNodes: lista con la informacion de cada nodo vecino con la forma ucsNode(_).
calcUcsNodes(_Position, _Path, _Actions, _PathCost, [], []).

calcUcsNodes(Position, Path, Actions, PathCost, [[Neigh, Turns, RemainingEnergy, ListOfActions] | ListOfListOfActions], [ucsNode(Neigh, [Position | Path], NewActions, NewPathCost) | Neighbors]):-
    append(Actions, ListOfActions, NewActions),
    NewPathCost is PathCost + Turns,
    calcUcsNodes(Position, Path, Actions, PathCost, ListOfListOfActions, Neighbors).

% calcActions(-Pos, -Energy, -NeighborsList, +ListOfActions)
% Pos: posicion actual.
% Energy: energía actual.
% NeighborsList: lista de vecinos de la posición actual.
% ListOfActions: lista que contiene, para cada vecino, una lista de la forma [nombre, turnos que lleva llegar, energía restante, lista de acciones necesarias].
calcActions(_Pos, _Energy, [], []).

calcActions(Pos, Energy, [Neigh | Neighs], [[Neigh, Turns, RemainingEnergy, ListOfActions] | ListOfListOfActions]) :-
    k(edge(Pos, Neigh, Value)),
    calcRecharge(Energy, Value, [goto(Neigh)], ListOfActions, 1, Turns, RemainingEnergy), !,
    calcActions(Pos, Energy, Neighs, ListOfListOfActions).


% calcRecharge(-Energy, -Value, -OldList, +NewList, -Turns, +NewTurns, +RemainingEnergy)
% Energy: energia actual del agente en la suposicion que se movio
% Value: valor del arco
% OldList: acciones actualmente calculadas
% NewList: acciones a devolver.
% Turns: turnos usados actualmente.
% NewTurns: turnos usados por todas las acciones.
% RemainingEnergy: Energia restante al terminar ed ejecutar las acciones.
calcRecharge(Energy, _Value, _OldList, _OldList2, _Turns, _Turns2, _RemainingEnergy) :-
    maxEnergy(Max),
    Energy > Max, !,
    fail.

calcRecharge(Energy, Value, OldList, OldList, Turns, Turns, RemainingEnergy) :-
    Energy >= Value, !,
    RemainingEnergy is Energy - Value.

calcRecharge(Energy, Value, OldList, NewList, OldTurns, NewTurns2, RemainingEnergy) :-
    rechargeEnergy(RechargeEnergy),
    NewTurns is OldTurns + 1,
    NewEnergy is Energy + RechargeEnergy,
    calcRecharge(NewEnergy, Value, [recharge | OldList], NewList, NewTurns, NewTurns2, RemainingEnergy).

rechargeEnergy(2).
maxEnergy(10).
isGoal(vertex9).