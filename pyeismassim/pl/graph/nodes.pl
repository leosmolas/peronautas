:- dynamic hnode/3.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).
setOwner([Node | Nodes], Owner) :-
    retractall( h(nodeTeam(_, Node, _))), 
    currentStep(Step),
    assertz( h(nodeTeam(Step, Node, Owner))), 
    setOwner(Nodes, Owner).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team
nodesOfTeam(Team, NodesOfTeam) :- 
    currentStep(Step),
    findall(Node, h(nodeTeam(Step, Node, Team)), NodesOfTeam).


% clearNode(-Node)
clearNode(Node) :- 
    currentStep(Step),
    h(nodeTeam(Step, Node, none)), 
    not(h(position(Step, _, Node))).



% emptyNode(-Node)
emptyNode(Node) :- 
    currentStep(Step),
    k(nodeValue(Node, _)), 
    not(h(position(Step, _, Node))).



% nonEmptyNode(-Node)
nonEmptyNode(Node) :- 
    currentStep(Step),
    k(nodeValue(Node, _)), 
    h(position(Step, _, Node)).



% checkOwner(+Node, ?Team)
% checks if the owner of the Node is Team. It can also return the actual Team of the node
checkOwner(Node, Team) :- 
    currentStep(Step),
    h(nodeTeam(Step, Node, Team)), 
    !.



listOfNodes(ListOfNodes) :- 
    currentStep(Step),
    findall(Node, k(nodeValue(Step, Node, _)), ListOfNodes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% hnode(Name, Value, Owner).
% this block should die when we merge it with the agent.
% if everyone agrees, this should be named vert (since the server sents vert)
% hnode(a, 1, none).
% hnode(b, 2, none).
% hnode(c, 3, none).
% hnode(d, 1, none).
% hnode(e, 1, none).
% hnode(f, 1, none).
% hnode(g, 5, none).
% hnode(h, 4, none).
% hnode(i, 1, none).
% hnode(j, 2, none).
