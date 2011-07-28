:- [graph/map], [kmap].
:- dynamic isGoal/1.

% ucs(-Frontier, -Visited, +Path, +Actions, +Path_Cost)
% Frontier: frontera del recorrido.
% Visited: conjunto de visitados.
% Energy : energia actual
% Path: camino retornado.
% Actions: lista de acciones necesarias para llegar al nodo.
% Path_Cost: costo del camino encontrado.
ucs(Frontier, _Visited, [Position | Path], Actions, Path_Cost) :-
    ucsSelect(Frontier, ucsNode(Position, Energy, Path, Actions, Path_Cost), _Frontier1),
    isGoal(Position).

ucs(Frontier, Visited, SolutionPath, SolutionActions, Cost) :-
    ucsSelect(Frontier, SelectedNode, Frontier1),
    ucsNeighbors(SelectedNode, Neighbors),
    % add_paths(Neighbours, Selected_Node, Neighbours1),
    addToFrontier(Neighbors, Frontier1, FrontierNew, Visited, NewVisited),
    ucs(FrontierNew, [SelectedNode | NewVisited], SolutionPath, SolutionActions, Cost).

ucsSelect([Node | Frontier], Node, Frontier).
    
% addToFrontier(-Neighbors, -Frontier, +FrontierNew, -Visited, +VisitedNew).

addToFrontier([], Frontier, Frontier, Visited, Visited).

addToFrontier([Neighbor | Neighbors], OldFrontier, Frontier, OldVisited, Visited) :-
     check(Neighbor, OldFrontier, NewFrontier, OldVisited, NewVisited), !,
     addToFrontier(Neighbors, NewFrontier, Frontier, NewVisited, Visited).
        
addToFrontier([Neighbor | Neighbors], OldFrontier, Frontier, OldVisited, Visited) :-
    % not(member(ucsNode(Neighbor, _, _, _, _), OldVisited)),
	foreach(member(ucsNode(N, _, _, _, _), OldVisited), Neighbor \= N),
	insert_pq(Neighbor, OldFrontier, NewFrontier),
    addToFrontier(Neighbors, NewFrontier, Frontier, OldVisited, Visited).
    
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

check(ucsNode(Position, Energy, Path, Actions, Path_Cost), OldFrontier, NewFrontier, Visited, Visited) :-
    member(ucsNode(Position, _, _, _, Path_Cost2), OldFrontier),
    Path_Cost < Path_Cost2, !,
    delete(OldFrontier, ucsNode(Position, _, _, _, Path_Cost2), Frontier),
    insert_pq(ucsNode(Position, Energy, Path, Actions, Path_Cost), Frontier, NewFrontier).
    
check(ucsNode(Position, Energy, Path, Actions, Path_Cost), OldFrontier, NewFrontier, OldVisited, NewVisited) :-
    member(ucsNode(Position, _, _, _, Path_Cost2), OldVisited),
    Path_Cost < Path_Cost2, !,
    delete(OldVisited, ucsNode(Position, _, _, _, Path_Cost2), NewVisited),
    insert_pq(ucsNode(Position, Energy, Path, Actions, Path_Cost), OldFrontier, NewFrontier).
    
% Insercion ordenada
insert_pq(State, [], [State]) :- !.
insert_pq(State, [H | T], [State, H | T]) :-
    precedes(State, H), !.
insert_pq(State, [H|T], [H | T_new]) :-
    insert_pq(State, T, T_new).
    
precedes(ucsNode(_Position, _Energy, _Path, _Actions, PathCost), ucsNode(_Position2, _Energy2, _Path2, _Actions2, PathCost2)) :-
    PathCost < PathCost2, !.

% ucsNeighbors(-UcsNode, -Energy, +Neighbors)
% UcsNode: el nodo actual.
% Energy: energia actual.
% Neighbors: lista con un ucsNodes por cada vecino.
ucsNeighbors(ucsNode(Position, Energy, Path, Actions, Path_Cost), Neighbors) :-
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

calcUcsNodes(Position, Path, Actions, PathCost, [[Neigh, Turns, RemainingEnergy, ListOfActions] | ListOfListOfActions], [ucsNode(Neigh, RemainingEnergy, [Position | Path], NewActions, NewPathCost) | Neighbors]):-
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

testUcs(P, A, C) :-
	ucs([ucsNode(vertex11, 5, [], [], 0)], [], P, A, C).
	
/*	
bfs(Node, _, Path, Path, Cost, Cost) :- isGoal(Node, Cost).
bfs(Node, Visited, OldPath, NewPath, OldCost, NewCost) :-
	k(edge(Node, Neighbor, _)),
	not(member(Neighbor, Visited)),
	Cost is OldCost + 1,
	bfs(Neighbor, [Neighbor | Visited], [Neighbor | OldPath], NewPath, Cost, NewCost).


testBfs(P, C):-
	bfs(vertex11, [vertex11], [vertex11], P, 0, C).
*/	
	
% path([], _).

bfs([bfsNode(Node, Path, Cost) | RestOfFrontier], Visited, bfsNode(Node, Path, Cost)) :- 
    % remove_from_queue(bfsNode(Node, Path, Cost), Frontier, _RestOfFrontier),
    isGoal(Node, Cost).
    
bfs([bfsNode(Node, Path, Cost) | RestOfFrontier], Visited, Result) :- 
    % remove_from_queue(bfsNode(Node, Path, Cost), Frontier, RestOfFrontier),
    %(bagof(Child, moves(Next_record, Open, Closed, Child), Children);Children = []),
	%kneighbors(Node, Neighbors),
    findall(Neighbor,
            k(edge(Node, Neighbor, _)),
            Neighbors),
	filter(Neighbors, RestOfFrontier, Visited, FilteredNeighbors),
    add_list_to_queue(FilteredNeighbors, Path, Cost, RestOfFrontier, NewFrontier), 
    add_to_set(bfsNode(Node, Path, Cost), Visited, NewVisited),
    bfs(NewFrontier, NewVisited, Result).

filter(Nodes, Frontier, Visited, FilteredNodes) :-
	findall(Node, (
		member(Node, Nodes), 
		not(member(bfsNode(Node, _P, _C), Frontier)), 
		not(member(bfsNode(Node, _P2, _C2), Visited))),
	FilteredNodes).
	
/*	
moves(State_record, Open, Closed, Child_record) :-
    state_record(State, _, State_record),
    mov(State, Next),
    % not (unsafe(Next)),
    state_record(Next, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record(Next, State, Child_record).
*/

add_list_to_queue([], _Path, _Cost, Queue, Queue).
add_list_to_queue([H|T], Path, Cost, Queue, NewQueue) :-
	NewCost is Cost + 1,
    add_to_queue(bfsNode(H, [H | Path], NewCost), Queue, TempQueue),
    add_list_to_queue(T, Path, Cost, TempQueue, NewQueue).

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :- 
    append(First, Second, Concatenation).
	
add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).	

testBfs(R) :-
    assert((isGoal(Node2, Cost) :- !, myVisionRange(Range), Cost < Range)),
	bfs([bfsNode(vertex0, [vertex0], 0)], [], R).
	
% lastKnownPosition(-Step, +Agent, -Position)
lastKnownPosition(Step, Agent, Position) :-
	currentStep(currentStep),
	lastKnownPosition(currentStep, Step, Agent, Position).

% Condicion de corte, exito.
lastKnownPosition(Step, Step, Agent, Position) :-
	k(agent(Step, Agent, Position)).

% Condicion de corte, sin exito.
lastKnownPosition(0, 0, Agent, unknown).

lastKnownPosition(CurrentStep, Step, Agent, Position) :-
	PreviousStep is CurrentStep - 1,
	lastKnownPosition(PreviousStep, Step, Agent, Position).

markExploredNodes :-
    currentStep(Step),
    write('MARCA, CRISTO '), nl,
    myName(Name),
    write('MARCA2, CRISTO '), nl,
    position(Step, Name, CurrentPosition),
    write('MARCA3, CRISTO '), nl,
    assert((isGoal(Node2, Cost) :- myVisionRange(Range), Cost < Range)),
    write(' Marking... '), nl,
    foreach(
                bfs([bfsNode(CurrentPosition, [CurrentPosition], 0)], [], Node),
                (
                    write(' Marking node as explored: '),
                    write(Node),nl, 
                    assert(explored(Node))
                )
           ).
