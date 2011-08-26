:- dynamic neighborOwner/2.

% paths(+Start, +Finish, -Path)
% Pretty obvius actually but returns a path in Path with start point at node Start and endpoint in node Finish.
paths(Start, Finish, Path) :- 
    path(Start, Finish, [Start], Path).



% path(+Start, +Finish, +Visited, +Path).
path(Finish, Finish, _, [Finish]).
path(Start, Finish, Visited, [Start|Path]) :- 
    k(edge(Start, Next, _)), 
    not(member(Next, Visited)), 
    path(Next, Finish, [Next | Visited], Path).



% neighbors(+Node, -Neighbors)
% returns in Neighbors a list of neighbors of node Node.
neighbors(Node, Neighbors) :- 
    findall(
        Neigh, 
        k(edge(Node,Neigh,_)), 
        Neighbors
    ).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% neighbors(+Node, -Neighbors)
% returns in Neighbors a list of neighbors of node Node.
kneighbors(Node, Neighbors) :- findall(Neigh, k(edge(Node,Neigh,_)), Neighbors).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% agentsInNode(+Node, -Agents)
% returns in Agents a list of agents which are at Node.
agentsInNode(Node, Agents) :- 
    currentStep(Step),
    findall(
        Agent,         
        h(position(Step, Agent, Node)), 
        Agents
    ).



% teamsInNode(+Node, -Teams)
% returns in Teams a list of Teams which are present in a Node.
teamsInNode(Node, Teams) :- 
    currentStep(Step),
    findall(
        Team, 
        (
            h(position(Step, Agent, Node)), 
            team(Agent, Team)
        ), 
        Teams
    ).



% doNotFail(X) :- call(X), !.
% doNotFail(_).              ._.

% teamsInNeighbors(+Neighbors, -TeamsNeighborsCount)
% returns in TeamsNeighborsCount a list of pairs (Owner, Count), representing count of neighbor nodes owned by the teams.
teamsInNeighbors([], TeamsNeighborsCount) :- 
    !, 
    findall(
        [Owner,Count], 
        neighborOwner(Owner, Count), 
        TeamsNeighborsCount
    ),
    retractall( neighborOwner(_, _) ).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- 
    currentStep(Step),
    h(nodeTeam(Neighbor, Owner)),
    Owner \= none,
    h(position(Step, Agent, Neighbor)),
    team(Agent, Owner),
    neighborOwner(Owner, Count), 
    !,
    retract( neighborOwner(Owner, Count) ),
    Count2 is Count + 1,
    assert(neighborOwner(Owner, Count2)),
    teamsInNeighbors(Neighbors, TeamsNeighborsCount).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- 
    currentStep(Step),
    h(nodeTeam(Neighbor, Owner)),
    Owner \= none,
    h(position(Step, Agent, Neighbor)),
    team(Agent, Owner),
    !,
    assert(neighborOwner(Owner,1)),
    teamsInNeighbors(Neighbors, TeamsNeighborsCount).

teamsInNeighbors([ _ | Neighbors ], TeamsNeighborsCount) :- 
    !, 
    teamsInNeighbors(Neighbors, TeamsNeighborsCount).


% appears(+Team, +List, -CountTeam, -CountOtherTeam)
% returns in Count the number of times that Team appears in the List, and in CountOtherTeam the number of times the other team appears.

appears(_, [], 0, 0).
appears(Head, [Head|Tail], CountTeam, CountOtherTeam) :- 
    appears(Head, Tail, Aux, CountOtherTeam), 
    CountTeam is Aux + 1, 
    !.
appears(Element, [_ | Tail], CountTeam, CountOtherTeam) :- 
    appears(Element, Tail, CountTeam, Aux), 
    CountOtherTeam is Aux + 1.


% checkMajyorityInNode(+Node, +Team)
% checks if the number of agents of team Team are majority in node Node.

% checkMajorityInNode(Node, Team) :-  teamsInNode(Node, Teams), 
%                                    appears(Team, Teams, TeamInNode), 
%                                    length(Teams, TeamsInNode), 
%                                    Majority is floor(TeamsInNode / 2), 
%                                    TeamInNode > Majority,
%                                    setOwner([Node], Team).
% checkMajorityInNode(_, _).
checkMajorityInNode(Node) :-  
    teamsInNode(Node, Teams), 
    listOfTeams([Team1 | [Team2 | []]]),
    appears(Team1, Teams, Team1InNode, Team2InNode), 
    length(Teams, TeamsInNode), 
    Majority is floor(TeamsInNode / 2), 
    checkMajorityInNodeAux(Team1, Team1InNode, Team2, Team2InNode, Majority, Team),
    setOwner([Node], Team).
checkMajorityInNode(_).



% Is majority floor(TeamsInNode / 2)????
checkMajorityInNodeAux(Team1, Team1Count, _Team2, _Team2Count, Majority, Team1) :- Team1Count > Majority, !.
checkMajorityInNodeAux(_Team1, _Team1Count, Team2, Team2Count, Majority, Team2) :- Team2Count > Majority.



% maximum(+ListOfPairs, -Team)
% from a list of pairs (Team, Count) returns the Team that has the highest count and this count is greater than 1.
maximum([[Team, Count]], Team) :- 
    Count > 1, 
    !.
maximum([[Team1, Count1], [_Team2,Count2] | Rest], Team) :- 
    Count1 > Count2,
    maximumAux(Rest, [Team1, Count1], Count2, [Team, Count]),
    Count > 1.
maximum([[_Team1, Count1], [Team2,Count2] | Rest], Team) :- 
    Count1 =< Count2,
    maximumAux(Rest, [Team2, Count2], Count1, [Team, Count]),
    Count > 1.
                                                           

                                                       
% maximumAux(+ListOfPairs, +Higher, +OtherCount, -PairTeam&MaxCount)
% well, auxiliar predicate for maximum.
maximumAux([], [Team, Max1], Max2, [Team, Max1]) :- 
    Max1 > Max2.
maximumAux([[_Team1, Count] | Rest], [Team, Max], Max2, [TeamR, CountR]) :- 
    Count < Max,
    maximumAux(Rest, [Team, Max], Max2, [TeamR, CountR]).
maximumAux([[_Team1, Count] | Rest], [Team, Max], _Max2, [TeamR, CountR]) :- 
    Count = Max,
    maximumAux(Rest, [Team, Max], Count, [TeamR, CountR]).
maximumAux([[Team1, Count] | Rest], [_Team, Max], _Max2, [TeamR, CountR]) :- 
    Count > Max,
    maximumAux(Rest, [Team1, Count], Max, [TeamR, CountR]).

% checkMajorityInNeighbors(+Node)
% sets the color if the number of controlled neighbor nodes of Node are majority of a team.
checkMajorityInNeighbors(Node) :- 
    neighbors(Node, Neighbors),
    teamsInNeighbors(Neighbors, TeamsNeighborsCount),
    maximum(TeamsNeighborsCount, MaxTeam),
    setOwner([Node], MaxTeam).
checkMajorityInNeighbors(_).



% atLeastOne(+List1, +List2)
% checks if at least one element of List1 is present in List2
atLeastOne([Head | _], List) :- 
    member(Head, List), 
    !.
atLeastOne([_ | Tail], List) :- 
    atLeastOne(Tail, List).



% checkPaths(+ListOfPaths, +ListOfNodes)
% checks if for all paths in ListOfPaths at least 1 node is present in ListOfNodes.
checkPaths([Head | Tail], ListOfPaths) :- 
    atLeastOne(Head, ListOfPaths), 
    !, 
    checkPaths(Tail, ListOfPaths).



% dfs(+Node)
% dfs calls depthfirst, and sets the Owner of Node, and all the nodes it could reach.
dfs(Node) :- 
    depthfirst(Node, [Node], Team, ReachedNodes), 
    !, 
    setOwner(ReachedNodes, Team).
dfs(_).



% depthfirst(+Node, +Visited, -Team, -ReachedNodes)
% implements the search for neighbors and depth first them.
% It returns the Team of ReachedNodes.
depthfirst(Node, Visited, Team, ReachedNodes) :- 
    neighbors(Node, Neighbors), 
    checkNeighbors(Node, Neighbors, Visited, Team, ReachedNodes).



% checkNeighbors(+Node, +Neighbors, +Visited, +Team)
% checks neighbors of a node following these rules in order.
%  if there are no more Neighbors to visit, i've gone to the deepest node and found no enemy agents.
%  if there is an agent of the other team in the Node, then at least one agent of the enemy team can reach the Node, so its not isolated.
%  if my team is the owner of the Node, won't go deeper, because enemies must pass through this node to reach the analized node.
%  if none of above happens i countinue the depth first search from the actual node.
checkNeighbors(_Node, [], Visited, _Team, Visited) :- 
    !.
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- 
    member(Head, Visited), !,
    checkNeighbors(Node, Tail, Visited, Team, ReachedNodes).
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- 
    checkOwner(Head, Team),
    Team \= ofNoOne,
    Team \= none, 
    !,
    checkNeighbors(Node, Tail, Visited, Team, ReachedNodes).
checkNeighbors(_Node, [Head|_Tail], Visited, Team, _ReachedNodes) :- 
    var(Team), % me aseguro que no venga instanciado
    checkOwner(Head, ofNoOne), !,
    setOwner(Visited, ofNoOne),
    fail.
checkNeighbors(_Node, [Head|_Tail], Visited, Team, _ReachedNodes) :- 
    atom(Team), % me aseguro que venga instanciado
    checkOwner(Head, OtherTeam),
    OtherTeam \= none,
    % OtherTeam \= ofNoOne,
    Team \= OtherTeam, !,
    setOwner(Visited, ofNoOne),
    fail.
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- 
    depthfirst(Head, [Head|Visited], Team, ReachedNodes1),
    checkNeighbors(Node, Tail, [Head|Visited], Team, ReachedNodes2),
    union(ReachedNodes1, ReachedNodes2, ReachedNodes).

cleanColors :- 
    listOfNodes(ListOfNodes),
    setOwner(ListOfNodes, none).

setHypotheticalMap :-
    retractall(h(position(_, _, _))),
    retractall(h(nodeTeam(_, _))),
    currentStep(Step),
    foreach(
        position(Step, A, N),
        assert(h(position(Step, A, N)))
    ),
    foreach(
        k(nodeValue(Node, _V)),
        assert(h(nodeTeam(Node, none)))
    ).
    
moveAgent(Agent, Node) :-
    currentStep(Step),
    retractall(h(position(_Step, Agent, _))),
    assert(h(position(Step, Agent, Node))).

visibleNode(N) :-
    explored(N),
    inRange(N),
    foreach(
        k(edge(N, N1, _)),
        inRange(N1)
    ), !,
    retract(notVisible(N)),
    asserta(visibleNode(N)).
    
    
% toogleOnVisibleNode(+Node)
% si el nodo ya estÃ¡ marcado como visible, no hace nada
% sino, hace el toogle
toogleOnVisibleNode(Node) :-
    visibleNode(Node), !.
    
toogleOnVisibleNode(Node) :-
    retractall(notVisible(Node)),
    % write('1.3 retract '),write(Agent),nl,
    asserta(visibleNode(Node)).
    % write('1.4 asserta '),write(Agent),nl.
    
toogleOffVisibleNodes :-
    foreach(
                visibleNode(N),
                (
                    retract(visibleNode(N)),
                    assert(notVisible(N))
                )
           ).
    
agentsRangeVision(Step, MyTeam, Range, Position) :-
    team(Agent, MyTeam),
    visualRange(Step, Agent, Range),
    position(Step, Agent, Position),
    write(position(Step, Agent, Position)),nl,
    Range \= unknown.            % esto es un parche para cuando se corre sin servidor de percepciones, porque sino el rango del compañero es un dato que se deberña tener
    
% setExploredAndVisible
% predicado que setea como "exploredNode" a los nodos para los cuales conozco todos sus vecinos,
% y como visibleNode(Node) a los nodos a los que marque como explorados ESTE TURNO.
setExploredAndVisible :-
    currentStep(Step),
    myTeam(MyTeam),
    foreach(
        (
            agentsRangeVision(Step, MyTeam, Range, Position)
        ),
        (
            setExploredAndVisibleAux1(Range, Position)
            % writeln(Range),
            % writeln(Position)
        )
    ).

setExploredAndVisibleAux1(Range, Position) :-   
    write('setExploredAndVisibleAux1:'), write(Range), write(','), write(Position), nl,
    retractall(isGoal(_, _)),
    assert((isGoal(_Node2, Cost) :- !, Cost < Range)),
    foreach(
        breadthFirst(Position, Node, _Path, _Cost),
        (
            setExploredAndVisibleAux2(Node)
        )
    ).
    
setExploredAndVisibleAux2(Node) :- 
    retractall(notExplored(Node)),
    assertOnce(explored(Node)),
    toogleOnVisibleNode(Node).
    
% coloringAlgorithm
% clears the owner of all teams and runs the 3 steps of the coloring algorithm.

coloringAlgorithm :- 
    step1,
    step2,
    step3.

% step1
% first step of the coloring algorithm
step1 :- 
    setof(Node, nonEmptyNode(Node), ListOfNodes),
    % writeln('sale del set of del step 1'),
    foreach(
        member(Node2,ListOfNodes), 
        (
            checkMajorityInNode(Node2)
        )
    ).



% step2
% second step of the coloring algorithm
step2 :- 
    foreach(
        (
            emptyNode(Node),
            visibleNode(Node)
        ),
        (
            checkMajorityInNeighbors(Node)
        )
    ).



% step3
% third step of the coloring algorithm
step3 :-    
    % findall(N1, (notExplored(N1), h(nodeTeam(N1, none))), ListOfNotExplored), % agregue que el nodo no haya estado pintado en un paso anterior (estabamos pisando data)
    findall(N2, (notVisible(N2), h(nodeTeam(N2, none))), ListOfNotVisible),
    % setOwner(ListOfNotExplored, ofNoOne),
    setOwner(ListOfNotVisible, ofNoOne),
    foreach(
        clearNode(Node), 
        (
            dfs(Node)
        )
    ).
    
    
% Algoritmo para calcular los puntos de un equipo a partir de hnodes.
teamHPoints(Team, Points) :-
    findall(
        [Node, Team, Value],
        (
            h(nodeTeam(Node, Team)), 
            k(nodeValue(Node, Value))
        ), 
        ListOfNodes),
    calcHPoints(ListOfNodes, 0, Points).
    
calcHPoints([], Points, Points).

calcHPoints([[Node, Team, unknown] | Nodes], Points1, Points3):-
    checkHNeighbors(Node, Team), !,
    Points2 is Points1 + 1,
    calcHPoints(Nodes, Points2, Points3).

calcHPoints([[Node, Team, Value] | Nodes], Points1, Points3):-
    checkHNeighbors(Node, Team), !,
    Points2 is Points1 + Value,
    calcHPoints(Nodes, Points2, Points3).
    
calcHPoints([_ | Nodes], Points1, Points2):-
    calcHPoints(Nodes, Points1, Points2).
    
% chequea que por lo menos un vecino sea del mismo equipo, para asi podes sumar sus puntos
checkHNeighbors(Node, Team) :-
    k(edge(Node, X, _)),
    h(nodeTeam(X, Team)), !.
    
% Algoritmo para calcular los puntos de un equipo.
teamPoints(Team, Points) :-

    setHypotheticalMap,
    calcTime('coloringAlgorithm', coloringAlgorithm),
    teamHPoints(Team, Points).

/*
teamPoints(Team, Points) :-
    currentStep(Step),
    findall(
        [Node, Team, Value],
        (
            k(nodeTeam(Step, Node, Team)), 
            k(nodeValue(Node, Value))
        ), 
        ListOfNodes),
    calcPoints(ListOfNodes, 0, Points).
*/
    
calcPoints([], Points, Points).

calcPoints([[Node, Team, unknown] | Nodes], Points1, Points3):-
    checkNeighbors(Node, Team), !,
    Points2 is Points1 + 1,
    calcPoints(Nodes, Points2, Points3).

calcPoints([[Node, Team, Value] | Nodes], Points1, Points3):-
    checkNeighbors(Node, Team), !,
    Points2 is Points1 + Value,
    calcPoints(Nodes, Points2, Points3).
    
calcPoints([_Node | Nodes], Points1, Points2):-
    calcPoints(Nodes, Points1, Points2).
    
checkNeighbors(Node, Team) :-
    k(edge(Node, X, _)),
    currentStep(Step),
    k(nodeTeam(Step, X, Team)), !.
