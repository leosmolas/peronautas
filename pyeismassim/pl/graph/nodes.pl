:- dynamic hnode/3.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).
setOwner([Node | Nodes], Owner) :- 
    retract( hnode(Node, Value, _)     ), 
    assertz( hnode(Node, Value, Owner) ), 
    setOwner(Nodes, Owner).



% changeValue(+Node, +Value).
% changes the value of the node to a new value
changeValue(Node, Value) :- 
    retract( hnode(Node,_,Owner)       ), 
    assertz( hnode(Node, Value, Owner) ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team
nodesOfTeam(Team, NodesOfTeam) :- 
    findall(Node, hnode(Node, _, Team), NodesOfTeam).


% clearNode(-Node)
clearNode(Node) :- 
    hnode(Node, _, none), 
    not(hposition(_, Node)).



% emptyNode(-Node)
emptyNode(Node) :- 
    hnode(Node, _, _), 
    not(hposition(_, Node)).



% nonEmptyNode(-Node)
nonEmptyNode(Node) :- 
    hnode(Node, _, _), 
    hposition(_, Node).



% checkOwner(+Node, ?Team)
% checks if the owner of the Node is Team. It can also return the actual Team of the node
checkOwner(Node, Team) :- 
    hnode(Node, _, Team), 
    !.



listOfNodes(ListOfNodes) :- 
    findall(Node, hnode(Node, _, _), ListOfNodes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% hnode(Name, Value, Owner).
% this block should die when we merge it with the agent.
% if everyone agrees, this should be named vert (since the server sents vert)
hnode(a, 1, none).
hnode(b, 2, none).
hnode(c, 3, none).
hnode(d, 1, none).
hnode(e, 1, none).
hnode(f, 1, none).
hnode(g, 5, none).
hnode(h, 4, none).
hnode(i, 1, none).
hnode(j, 2, none).
