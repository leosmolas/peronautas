:-['edges.pl', 'nodes.pl', 'agents.pl'].
:- dynamic neighborOwner/2.

% paths(+Start, +Finish, -Path)
% Pretty obvius actually but returns a path in Path with start point at node Start and endpoint in node Finish.
paths(Start, Finish, Path) :- path(Start, Finish, [Start], Path).

% path(+Start, +Finish, +Visited, +Path).
path(Finish, Finish, _, [Finish]).
path(Start, Finish, Visited, [Start|Path]) :- edge(Start, Next, _), not(member(Next,Visited)), path(Next, Finish, [Next|Visited], Path).


% neighbors(+Node, -Neighbors)
% returns in Neighbors a list of neighbors of node Node.
neighbors(Node, Neighbors) :- findall(Neigh, edge(Node,Neigh,_), Neighbors).


% agentsInNode(+Node, -Agents)
% returns in Agents a list of agents which are at Node.
agentsInNode(Node, Agents) :- findall(Agent, position(Agent, Node), Agents).


% teamsInNode(+Node, -Teams)
% returns in Teams a list of Teams which are present in a Node.
teamsInNode(Node, Teams) :- findall(Team, (position(Agent, Node), teamOfAgent(Agent, Team)), Teams).

% doNotFail(X) :- call(X), !.
% doNotFail(_).              ._.

% teamsInNeighbors(+Neighbors, -TeamsNeighborsCount)
% returns in TeamsNeighborsCount a list of pairs (Owner, Count), representing count of neighbor nodes owned by the teams.
teamsInNeighbors([], TeamsNeighborsCount) :- !, findall([Owner,Count], neighborOwner(Owner,Count), TeamsNeighborsCount),
                                             retractall(neighborOwner(_,_)).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- node(Neighbor,_,Owner),
                                                                 Owner \= none,
                                                                 position(Agent,Neighbor),
                                                                 teamOfAgent(Agent, Owner),
                                                                 neighborOwner(Owner,Count), !,
                                                                 retract(neighborOwner(Owner,Count)),
                                                                 Count2 is Count + 1,
                                                                 assert(neighborOwner(Owner,Count2)),
                                                                 teamsInNeighbors(Neighbors, TeamsNeighborsCount).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- node(Neighbor,_,Owner),
                                                                 Owner \= none,
                                                                 position(Agent,Neighbor),
                                                                 teamOfAgent(Agent, Owner), !,
                                                                 assert(neighborOwner(Owner,1)),
                                                                 teamsInNeighbors(Neighbors, TeamsNeighborsCount).

teamsInNeighbors([ _ | Neighbors ], TeamsNeighborsCount) :- !, teamsInNeighbors(Neighbors, TeamsNeighborsCount).


% appears(+Team, +List, -CountTeam, -CountOtherTeam)
% returns in Count the number of times that Team appears in the List, and in CountOtherTeam the number of times the other team appears.
appears(_, [], 0, 0).
appears(Head, [Head|Tail], CountTeam, CountOtherTeam) :- appears(Head, Tail, Aux, CountOtherTeam), CountTeam is Aux + 1, !.
appears(Element, [_|Tail], CountTeam, CountOtherTeam) :- appears(Element, Tail, CountTeam, Aux), CountOtherTeam is Aux + 1.


% checkMajyorityInNode(+Node, +Team)
% checks if the number of agents of team Team are majority in node Node.
%checkMajorityInNode(Node, Team) :-  teamsInNode(Node, Teams), 
%                                    appears(Team, Teams, TeamInNode), 
%                                    length(Teams, TeamsInNode), 
%                                    Majority is floor(TeamsInNode / 2), 
%                                    TeamInNode > Majority,
%                                    setOwner([Node], Team).
%checkMajorityInNode(_, _).
checkMajorityInNode(Node) :-  teamsInNode(Node, Teams), 
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
maximum([[Team, Count]], Team) :- Count > 1, !.
maximum([[Team1, Count1], [_Team2,Count2] | Rest], Team) :- Count1 > Count2,
                                                           maximumAux(Rest, [Team1, Count1], Count2, [Team, Count]),
                                                           Count > 1.
maximum([[_Team1, Count1], [Team2,Count2] | Rest], Team) :- Count1 =< Count2,
                                                           maximumAux(Rest, [Team2, Count2], Count1, [Team, Count]),
                                                           Count > 1.
                                                           
% maximumAux(+ListOfPairs, +Higher, +OtherCount, -PairTeam&MaxCount)
% well, auxiliar predicate for maximum.
maximumAux([], [Team, Max1], Max2, [Team, Max1]) :- Max1 > Max2.
maximumAux([[_Team1, Count] | Rest], [Team, Max], Max2, [TeamR, CountR]) :- Count < Max,
                                                                            maximumAux(Rest, [Team, Max], Max2, [TeamR, CountR]).
maximumAux([[_Team1, Count] | Rest], [Team, Max], _Max2, [TeamR, CountR]) :- Count = Max,
                                                                            maximumAux(Rest, [Team, Max], Count, [TeamR, CountR]).
maximumAux([[Team1, Count] | Rest], [_Team, Max], _Max2, [TeamR, CountR]) :- Count > Max,
                                                                            maximumAux(Rest, [Team1, Count], Max, [TeamR, CountR]).

% checkMajorityInNeighbors(+Node)
% sets the color if the number of controlled neighbor nodes of Node are majority of a team.
checkMajorityInNeighbors(Node) :- neighbors(Node, Neighbors),
                                  teamsInNeighbors(Neighbors, TeamsNeighborsCount),
                                  maximum(TeamsNeighborsCount, MaxTeam),
                                  setOwner([Node], MaxTeam).
checkMajorityInNeighbors(_).


% atLeastOne(+List1, +List2)
% checks if at least one element of List1 is present in List2
atLeastOne([Head | _], List) :- member(Head, List), !.
atLeastOne([_ | Tail], List) :- atLeastOne(Tail, List).


% checkPaths(+ListOfPaths, +ListOfNodes)
% checks if for all paths in ListOfPaths at least 1 node is present in ListOfNodes.
checkPaths([Head | Tail], ListOfPaths) :- atLeastOne(Head, ListOfPaths), !, checkPaths(Tail, ListOfPaths).





% dfs(+Node)
% dfs calls depthfirst, and sets the Owner of Node, and all the nodes it could reach.
dfs(Node) :- depthfirst(Node, [Node], Team, ReachedNodes), !, setOwner(ReachedNodes, Team).
dfs(_).


% depthfirst(+Node, +Visited, -Team, -ReachedNodes)
% implements the search for neighbors and depth first them.
% It returns the Team of ReachedNodes.
depthfirst(Node, Visited, Team, ReachedNodes) :- neighbors(Node, Neighbors), 
                                                 checkNeighbors(Node, Neighbors, Visited, Team, ReachedNodes).


% checkNeighbors(+Node, +Neighbors, +Visited, +Team)
% checks neighbors of a node following these rules in order.
%  if there are no more Neighbors to visit, i've gone to the deepest node and found no enemy agents.
%  if there is an agent of the other team in the Node, then at least one agent of the enemy team can reach the Node, so its not isolated.
%  if my team is the owner of the Node, won't go deeper, because enemies must pass through this node to reach the analized node.
%  if none of above happens i countinue the depth first search from the actual node.

checkNeighbors(_Node, [], Visited, _Team, Visited) :- !.
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- member(Head, Visited), !,
                                                                  checkNeighbors(Node, Tail, Visited, Team, ReachedNodes).
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- checkOwner(Head, Team),
                                                                  Team \= ofNoOne,
                                                                  Team \= none, !,
                                                                  checkNeighbors(Node, Tail, Visited, Team, ReachedNodes).
checkNeighbors(_Node, [Head|_Tail], Visited, Team, _ReachedNodes) :- atom(Team), % me aseguro que venga instanciado
                                                                     checkOwner(Head, OtherTeam),
                                                                     OtherTeam \= none,
                                                                     % OtherTeam \= ofNoOne,
                                                                     Team \= OtherTeam, !,
                                                                     setOwner(Visited, ofNoOne),
                                                                     fail.
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- depthfirst(Head, [Head|Visited], Team, ReachedNodes1),
                                                                  checkNeighbors(Node, Tail, [Head|Visited], Team, ReachedNodes2),
                                                                  union(ReachedNodes1, ReachedNodes2, ReachedNodes).


cleanColors :- listOfNodes(ListOfNodes),
               setOwner(ListOfNodes, none).

% coloringAlgorithm
% clears the owner of all teams and runs the 3 steps of the coloring algorithm.
coloringAlgorithm :- cleanColors,
                     step1,
                     step2,
                     step3.


% step1
% first step of the coloring algorithm
step1 :- findall(Node, nonEmptyNode(Node), ListOfNodes),
         % convertir ListOfNodes en un conjunto
         foreach(member(Node2,ListOfNodes), (checkMajorityInNode(Node2))).


% step2
% second step of the coloring algorithm
step2 :- foreach(emptyNode(Node), (checkMajorityInNeighbors(Node))).


% step3
% third step of the coloring algorithm
step3 :- foreach(clearNode(Node), (dfs(Node))).
