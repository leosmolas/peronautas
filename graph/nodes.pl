:- dynamic nodes/1.
:- dynamic node/3.

% insertNode(+Name, +Value)
% inserts node to graph with name Name and value Value
insertNode(Name, Value) :- retract(nodes(Z)), append(Z,[Name],X), asserta(nodes(X)), assertz(node(Name,Value, null)).


% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).
setOwner([Node | Nodes], Owner) :- retract(node(Node, Value, _)), assertz(node(Node, Value, Owner)), setOwner(Nodes, Owner).


% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team
nodesOfTeam(Team, NodesOfTeam) :- findall(Node, node(Node, _, Team), NodesOfTeam).


% clearNode(+Node, -ListOfNodes)
% checks if Node is present in the ListOfNodes and there is no agent at the node, and has no owner
clearNode(Node, ListOfNodes) :- member(Node, ListOfNodes), not(position(_,_, Node)), node(Node, _, no).


% emptyNode(+Node, +ListOfNodes)
% checks if the Node is present in the ListOfNodes and there is no agent positioned at the node
emptyNode(Node, ListOfNodes) :- member(Node, ListOfNodes), not(position(_,_, Node)).


% nonEmptyNode(+Node, +ListOfNodes)
% checks if the node is present in the ListOfNodes and the node has no agent on it.
nonEmptyNode(Node, ListOfNodes) :- member(Node, ListOfNodes), position(_, _, Node).


% checkOwner(+Node, +Team)
% checks if the owner of the Node is Team.
checkOwner(Node, Team) :- node(Node, _, Team).


% nodes(List of Nodes).
nodes([a,b,c,d,e,f,g,h,i,j]).


% node(Name, Value, Owner).
node(a, 1, no).
node(b, 2, no).
node(c, 3, no).
node(d, 1, no).
node(e, 1, no).
node(f, 1, no).
node(g, 5, no).
node(h, 4, no).
node(i, 1, no).
node(j, 2, no).
