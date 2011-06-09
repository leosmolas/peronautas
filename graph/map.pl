:-['edges.pl', 'nodes.pl', 'agents.pl'].

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


%teamsInNeighbors(+Node, -Teams)
%returns in Teams a list of Teams that own neighbor Nodes.
teamsInNeighbors(Node, Teams) :- neighbors(Node, Neighbors),                           
                                 findall(Owner, (member(Neigh, Neighbors), node(Neigh, _, Owner)), Teams).
                                                  

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
                                    setOwner(Node, Team).
checkMajorityInNode(_, _) :- true.                            


%checkMajorityInNeighbors(+Node, +Team)
%check if the number of controlled neighbor nodes of Node are majority of team Team.
checkMajorityInNeighbors(Node, Team) :- teamsInNeighbors(Node, TeamsInNeighbors),
                                        appears(Team, TeamsInNeighbors, TeamInTeamsInNeighbors),
                                        length(TeamsInNeighbors, CountTeamsInNeighbors),
                                        Majority is floor(CountTeamsInNeighbors / 2),
                                        TeamInTeamsInNeighbors > Majority,
                                        setOwner(Node, Team).
checkMajorityInNeighbors(_, _) :- true.


%atLeastOne(+List1, +List2)
%checks if at least one element of List1 is present in List2
atLeastOne([Head | _], List) :- member(Head, List), !.
atLeastOne([_ | Tail], List) :- atLeastOne(Tail, List).


%checkPaths(+ListOfPaths, +ListOfNodes)
%checks if for all paths in ListOfPaths at least 1 node is present in ListOfNodes.
checkPaths([Head | Tail], ListOfPaths) :- atLeastOne(Head, ListOfPaths), !, checkPaths(Tail, ListOfPaths).


% dfs(+Node, +Team)
% Checks depth first if this node is isolated from the enemy team... and if it is, it sets the owner to the Team.
dfs(Node, Team) :- depthfirst(Node, [Node], Team), !, setOwner(Node, Team).
dfs(_,_) :- true.


% depthfirst(+Node, +Visited, +Team)
% implements the search for neighbors and depth first them.
depthfirst(Node, Visited, Team) :- neighbors(Node, Neighbors), checkNeighbors(Node, Neighbors, Visited, Team).


% checkNeighbors(+Node, +Neighbors, +Visited, +Team)
% checks neighbors of a node following these rules in order.
%  if there are no more Neighbors to visit, i've gone to the deepest node and found no enemy agents.
%  if there is an agent of the other team in the Node, then at least one agent of the enemy team can reach the Node, so its no isolated.
%  if my team is the owner of the Node, wont go deeper, becuase enemies must pass through this node to reach the analized node.
%  if none of above happens i countinue the depth first search from the actual node.
checkNeighbors(_Node, [], _Visited, _Team).
checkNeighbors(_Node, [Head|_Tail], _Visited, Team) :- otherTeam(Team, OtherTeam), position(_, OtherTeam, Head), !, false.
checkNeighbors(Node, [Head|Tail], Visited, Team) :- member(Head, Visited), !, checkNeighbors(Node, Tail, Visited, Team).
checkNeighbors(Node, [Head|Tail], Visited, Team) :- checkOwner(Head, Team), !, checkNeighbors(Node, Tail, [Head|Visited], Team).
checkNeighbors(Node, [Head|Tail], Visited, Team) :- depthfirst(Head, [Head|Visited], Team), checkNeighbors(Node, Tail, [Head|Visited], Team).


% step1
% first step of the coloring algorithm
step1 :- teams(Teams),
         nodes(Nodes),
         foreach(member(Team,Teams), (foreach(nonEmptyNode(Node,Nodes), (checkMajorityInNode(Node, Team))))).


% step2
% second step of the coloring algorithm
step2 :- teams(Teams),
         nodes(Nodes),
         foreach(member(Team,Teams), (foreach(emptyNode(Node, Nodes), (checkMajorityInNeighbors(Node, Team))))).


% step3
% third step of the coloring algorithm
step3 :- teams(Teams),
         nodes(Nodes),
         foreach(member(Team, Teams), (foreach(clearNode(Node,Nodes), (dfs(Node, Team))))).
