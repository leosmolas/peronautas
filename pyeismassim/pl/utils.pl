ucs(Frontier, _Visited, [Position | Path], Actions, Path_Cost) :-
    ucsSelect(ucsNode(Position, Path, Actions, Path_Cost), Frontier, _Frontier1),
    is_goal(Position).

ucs(Frontier, Visited, Energy, Solution_Path, Solution_Actions, Cost) :-
    ucsSelect(Selected_Node, Frontier, Frontier1),
    ucsNeighbors(Selected_Node, Energy, Neighbours),
    add_paths(Neighbours, Selected_Node, Neighbours1),
    astar_add_to_frontier(Neighbours1, Frontier1, Frontier_New, Visited, Visited_New),
    astar_search1(Frontier_New, [Selected_Node | Visited_New], Solution_Path, Solution_Actions, Cost).

ucsNeighbors(ucsNode(Position, Path, Actions, Path_Cost), Energy, Neighbors) :-
    neighbors(Position, Neigh),
    calcActions(Position, Energy, Neigh, ListOfActions),
    calcUcsNodes(Position, Path, Actions, Path_Cost, ListOfActions, Neighbors).

calcUcsNodes(_Position, _Path, _Actions, _PathCost, [], []) :-

calcUcsNodes(Position, Path, Action, PathCost, [[Neigh, Turns, RemainingEnergy, ListOfActions] | ListOfListOfActions], ucsNode(Neigh, [Position | Path], NewActions, NewPathCost):-
    append(Action, ListOfActions, NewActions),
    NewPathCost is PathCost + Turns,
    calcUcsNodes(Position, Path, Action, PathCost, ListOfListOfActions, Neighbors).

calcActions(_Pos, _Energy, [], []).

calcActions(Pos, Energy, [Neigh | Neighs], [[Neigh, Turns, RemainingEnergy, ListOfActions] | ListOfListOfActions]) :-
    k(edge(Pos, Neigh, Value)),
    calcRecharge(Energy, Value, [goto(Neigh)], ListOfActions, 1, Turns, RemainingEnergy), !,
    calcActions(Pos, Energy, Neighs, ListOfListOfActions).


% calcRecharge(-Energy, -Value, -OldList, +NewList, -Turns, +NewTurns, +RemainingEnergy)
% Energy: energia actual del agente en la suposicion que se movio
% Value: valor del arco
% OldList: acciones actualmente calculadas
% NewList: acciones a devolver
% Turns: turnos usados actualmente
% NewTurns: turnos usados por todas las acciones
% RemainingEnergy: Energia restante al terminar ed ejecutar las acciones

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