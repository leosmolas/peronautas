:- dynamic hnode/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% setOwner(+Nodes, +Owner)
% sets the owner of the node Nodes to Owner.
setOwner([], _).

setOwner([Node | Nodes], Owner) :-
    retractall( h(nodeTeam(Node, _))), 
    assertz( h(nodeTeam(Node, Owner))), 
    setOwner(Nodes, Owner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% nodesOfTeam(+Team, -NodesOfTeam)
% returns in NodesOfTeam a list of nodes that are owned by the Team

nodesOfTeam(Team, NodesOfTeam) :- 
    findall(Node, h(nodeTeam(Node, Team)), NodesOfTeam).

% clearNode(-Node)

clearNode(Node) :- 
    h(nodeTeam(Node, none)). 
    % not(h(position(Step, _, Node))). % (??)

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
    h(nodeTeam(Node, Team)), !.

listOfNodes(ListOfNodes) :- 
    findall(Node, k(nodeValue(Node, _)), ListOfNodes).