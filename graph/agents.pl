:- dynamic position/2.
:- dynamic energy/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% moveAgent(+Agent, +Node).
% Moves the agent from his current position to Node.
moveAgent(Agent, Node) :- node(Node, _, _), retract(position(Agent, _OldNode)), assertz(position(Agent, Node)), !.


% travel(+Agent, +Node).
% Moves the agent from ActualNode to Node.  There must be and edge between these two nodes and the agent must have the necessary energy to travel.
travel(Agent, Node) :-  energy(Agent, Energy),                        
                        position(Agent, ActualNode),
                        edge(ActualNode, Node, Cost),
                        NewEnergy is Energy - Cost, 
                        NewEnergy >= 0,
                        retract(position(Agent, ActualNode)), 
                        assertz(position(Agent, Node)), 
                        setEnergy(Agent, NewEnergy), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- teams(T), T \= Team, !, OtherTeam = T.

listOfTeams(Teams) :- findall(Team, teams(Team), Teams).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% teams(-ListofTeams).
team(peronismo).
team(cacoteam).

% team(+Name, +ListofAgents).
%team(peronismo, [peron, evita, menem, cafiero]).
%team(cacoteam, [iorio, delia, moyano, castels]).


% teamOfAgent(+Agent, -Team)
% Team of an Agent.
teamOfAgent(peron, peronismo).
teamOfAgent(evita, peronismo).
teamOfAgent(cafiero, peronismo).
teamOfAgent(menem, peronismo).

teamOfAgent(iorio, cacoteam).
teamOfAgent(delia, cacoteam).
teamOfAgent(moyano, cacoteam).
teamOfAgent(castels, cacoteam).


% position(+Agent, -Team, -Node).
position(peron,a).
position(evita,a).
position(menem,g).
position(cafiero,b).

position(iorio,h).
position(delia,f).
position(castels,b).
position(moyano,b).
