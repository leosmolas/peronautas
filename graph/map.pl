:-['edges.pl', 'nodes.pl', 'agents.pl'].
:- dynamic neighborOwner/2.

%paths(+Start, +Finish, -Path)
%Pretty obvius actually but returns a path in Path with start point at node Start and endpoint in node Finish.
paths(Start, Finish, Path) :- path(Start, Finish, [Start], Path).

%path(+Start, +Finish, +Visited, +Path).
path(Finish, Finish, _, [Finish]).
path(Start, Finish, Visited, [Start|Path]) :- edge(Start, Next, _), not(member(Next,Visited)), path(Next, Finish, [Next|Visited], Path).


%neighbors(+Node, -Neighbors)
%returns in Neighbors a list of neighbors of node Node.
neighbors(Node, Neighbors) :- findall(Neigh, edge(Node,Neigh,_), Neighbors).


%agentsInNode(+Node, -Agents)
%returns in Agents a list of agents which are at Node.
agentsInNode(Node, Agents) :- findall(Agent, position(Agent, _, Node), Agents).


%teamsInNode(+Node, -Teams)
%returns in Teams a list of Teams which are present in a Node.
teamsInNode(Node, Teams) :- findall(Team, position(_, Team, Node), Teams).

% doNotFail(X) :- call(X), !.
% doNotFail(_).              ._.

%teamsInNeighbors(+Neighbors, -TeamsNeighborsCount)
%returns in TeamsNeighborsCount a list of Teams that own neighbor Nodes.

teamsInNeighbors([], TeamsNeighborsCount) :- findall([Owner,Count], neighborOwner(Owner,Count), TeamsNeighborsCount),
                                             retractall(neighborOwner(_,_)).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- node(Neighbor,_,Owner),
                                                                  neighborOwner(Owner,Count), !,
                                                                  retract(neighborOwner(Owner,Count)),
                                                                  Count2 is Count + 1,
                                                                  assert(neighborOwner(Owner,Count2)),
                                                                  teamsInNeighbors(Neighbors, TeamsNeighborsCount).

teamsInNeighbors([Neighbor | Neighbors], TeamsNeighborsCount) :- node(Neighbor,_,Owner),
                                                                  assert(neighborOwner(Owner,1)),
                                                                  teamsInNeighbors(Neighbors, TeamsNeighborsCount).

                                                    %findall(Owner, (member(Neigh, Neighbors), node(Neigh, _, Owner)), Teams).
                                                  

%appears(+Element, +List, -Count)
%returns in Count the number of times that Element appears in the List.
appears(_, [], 0).
appears(Head, [Head|Tail], Count) :- appears(Head, Tail, Aux), Count is Aux + 1, !.
appears(Element, [_|Tail], Count) :- appears(Element, Tail, Count).


%checkMajyorityInNode(+Node, +Team)
%checks if the number of agents of team Team are majority in node Node.
checkMajorityInNode(Node, Team) :-  teamsInNode(Node, Teams), 
                                    appears(Team, Teams, TeamInNode), 
                                    length(Teams, TeamsInNode), 
                                    Majority is floor(TeamsInNode / 2), 
                                    TeamInNode > Majority,
                                    setOwner([Node], Team).
checkMajorityInNode(_, _).

maximum([[Team, Count]], Team) :- Count > 1.
maximum([[Team1, Count1], [_Team2,Count2] | Rest], Team) :- Count1 > Count2,
                                                           maximumAux(Rest, [Team1, Count1], Count2, [Team, Count]),
                                                           Count > 1.
maximum([[_Team1, Count1], [Team2,Count2] | Rest], Team) :- Count1 =< Count2,
                                                           maximumAux(Rest, [Team2, Count2], Count1, [Team, Count]),
                                                           Count > 1.
                                                           
maximumAux([], [Team, Max1], Max2, [Team, Max1]) :- Max1 > Max2.
maximumAux([[_Team1, Count] | Rest], [Team, Max], Max2, [TeamR, CountR]) :- Count < Max,
                                                                            maximumAux(Rest, [Team, Max], Max2, [TeamR, CountR]).
maximumAux([[_Team1, Count] | Rest], [Team, Max], _Max2, [TeamR, CountR]) :- Count = Max,
                                                                            maximumAux(Rest, [Team, Max], Count, [TeamR, CountR]).
maximumAux([[Team1, Count] | Rest], [_Team, Max], _Max2, [TeamR, CountR]) :- Count > Max,
                                                                            maximumAux(Rest, [Team1, Count], Max, [TeamR, CountR]).

%checkMajorityInNeighbors(+Node)
%sets the color if the number of controlled neighbor nodes of Node are majority of a team.
checkMajorityInNeighbors(Node) :- neighbors(Node, Neighbors),
                                  teamsInNeighbors(Neighbors, TeamsNeighborsCount),
                                  maximum(TeamsNeighborsCount, MaxTeam),
                                  setOwner([Node], MaxTeam).
%                                         appears(Team, TeamsInNeighbors, TeamInTeamsInNeighbors),
%                                         length(TeamsInNeighbors, CountTeamsInNeighbors),
%                                         Majority is floor(CountTeamsInNeighbors / 2),
%                                         TeamInTeamsInNeighbors > Majority,
%                                         TeamInTeamsInNeighbors > 1,
%                                         setOwner([Node], Team).
%checkMajorityInNeighbors(_, _).


%atLeastOne(+List1, +List2)
%checks if at least one element of List1 is present in List2
atLeastOne([Head | _], List) :- member(Head, List), !.
atLeastOne([_ | Tail], List) :- atLeastOne(Tail, List).


%checkPaths(+ListOfPaths, +ListOfNodes)
%checks if for all paths in ListOfPaths at least 1 node is present in ListOfNodes.
checkPaths([Head | Tail], ListOfPaths) :- atLeastOne(Head, ListOfPaths), !, checkPaths(Tail, ListOfPaths).


% dfs(+Node, +Team)
% Checks depth first if this node is isolated from the enemy team... and if it is, it sets the owner to the Team.
dfs(Node, Team) :- depthfirst(Node, [Node], Team, ReachedNodes), !, setOwner(ReachedNodes, Team).
dfs(_,_).


% depthfirst(+Node, +Visited, +Team, -ReachedNodes)
% implements the search for neighbors and depth first them.
depthfirst(Node, Visited, Team, ReachedNodes) :- neighbors(Node, Neighbors), checkNeighbors(Node, Neighbors, Visited, Team, ReachedNodes).


% checkNeighbors(+Node, +Neighbors, +Visited, +Team)
% checks neighbors of a node following these rules in order.
%  if there are no more Neighbors to visit, i've gone to the deepest node and found no enemy agents.
%  if there is an agent of the other team in the Node, then at least one agent of the enemy team can reach the Node, so its no isolated.
%  if my team is the owner of the Node, wont go deeper, becuase enemies must pass through this node to reach the analized node.
%  if none of above happens i countinue the depth first search from the actual node.
checkNeighbors(_Node, [], Visited, _Team, Visited).
checkNeighbors(_Node, [Head|_Tail], Visited, Team, Visited) :- otherTeam(Team, OtherTeam), position(_, OtherTeam, Head), !, false.
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- member(Head, Visited), !, checkNeighbors(Node, Tail, Visited, Team, ReachedNodes).
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- checkOwner(Head, Team), !, checkNeighbors(Node, Tail, [Head|Visited], Team, ReachedNodes).
checkNeighbors(Node, [Head|Tail], Visited, Team, ReachedNodes) :- depthfirst(Head, [Head|Visited], Team, ReachedNodes1),
                                                                  checkNeighbors(Node, Tail, [Head|Visited], Team, ReachedNodes2),
                                                                  append(ReachedNodes1, ReachedNodes2, ReachedNodes).


% step1
% first step of the coloring algorithm
step1 :- teams(Teams),
         nodes(Nodes),
         foreach(member(Team,Teams), (foreach(nonEmptyNode(Node,Nodes), (checkMajorityInNode(Node, Team))))).


% step2
% second step of the coloring algorithm
step2 :- nodes(Nodes),
         foreach(emptyNode(Node, Nodes), (checkMajorityInNeighbors(Node))).


% step3
% third step of the coloring algorithm
step3 :- teams(Teams),
         nodes(Nodes),
         foreach(member(Team, Teams), (foreach(clearNode(Node,Nodes), (dfs(Node, Team))))).
