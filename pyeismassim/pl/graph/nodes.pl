:- dynamic hnode/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).
setOwner([Node | Nodes], Owner) :- 
    retract(hnode(Node, _, Value)), 
    assertz(hnode(Node, Owner, Value)), 
    setOwner(Nodes, Owner).

% changeValue(+Node, +Value).
% changes the value of the node to a new value
changeValue(Node, Value) :- 
    retract(hnode(Node, Owner, _)), 
    assertz(hnode(Node, Owner, Value)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team
nodesOfTeam(Team, NodesOfTeam) :- findall(Node, hnode(Node, Team, _), NodesOfTeam).


% clearNode(-Node)
%
clearNode(Node) :- hnode(Node, none, _), not(hposition(_, Node)).


% emptyNode(-Node)
%
emptyNode(Node) :- hnode(Node, _, _), not(hposition(_, Node)).


% nonEmptyNode(-Node)
%
nonEmptyNode(Node) :- hnode(Node, _, _), hposition(_, Node).


% checkOwner(+Node, ?Team)
% checks if the owner of the Node is Team. It can also return the actual Team of the node
checkOwner(Node, Team) :- hnode(Node, Team, _), !.


listOfNodes(ListOfNodes) :- findall(Node, hnode(Node, _, _), ListOfNodes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% hnode(Name, Owner, Value).
% this block should die when we merge it with the agent.
% if everyone agrees, this should be named vert (since the server sents vert)
% hnode(a, none, 1).
% hnode(b, none, 2).
% hnode(c, none, 3).
% hnode(d, none, 1).
% hnode(e, none, 1).
% hnode(f, none, 1).
% hnode(g, none, 5).
% hnode(h, none, 4).
% hnode(i, none, 1).
% hnode(j, none, 2).
