:- dynamic node/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).
setOwner([Node | Nodes], Owner) :- retract(node(Node, Value, _)), assertz(node(Node, Value, Owner)), setOwner(Nodes, Owner).


% changeValue(+Node, +Value).
% changes the value of the node to a new value
changeValue(Node, Value) :- retract(node(Node,_,Owner)), assertz(node(Node, Value, Owner)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team
nodesOfTeam(Team, NodesOfTeam) :- findall(Node, node(Node, _, Team), NodesOfTeam).


% clearNode(-Node)
%
clearNode(Node) :- node(Node, _, none), not(position(_, Node)).


% emptyNode(-Node)
%
emptyNode(Node) :- node(Node, _, _), not(position(_, Node)).


% nonEmptyNode(-Node)
%
nonEmptyNode(Node) :- node(Node, _, _), position(_, Node).


% checkOwner(+Node, ?Team)
% checks if the owner of the Node is Team. It can also return the actual Team of the node
checkOwner(Node, Team) :- node(Node, _, Team), !.


listOfNodes(ListOfNodes) :- findall(Node, node(Node, _, _), ListOfNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% node(Name, Value, Owner).
% this block should die when we merge it with the agent.
% if everyone agrees, this should be named vert (since the server sents vert)
node(a, 1, none).
node(b, 2, none).
node(c, 3, none).
node(d, 1, none).
node(e, 1, none).
node(f, 1, none).
node(g, 5, none).
node(h, 4, none).
node(i, 1, none).
node(j, 2, none).
