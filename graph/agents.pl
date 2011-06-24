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
otherTeam(Team, OtherTeam) :- teams(OtherTeam), OtherTeam \= Team, !.

listOfTeams(Teams) :- findall(Team, team(Team), Teams).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% teams(-ListofTeams).
team(peronismo).
team(cacoteam).

% team(+Name, +ListofAgents).
%team(peronismo, [peron, evita, menem, cafiero]).
%team(cacoteam, [iorio, delia, moyano, castels]).


% teamOfAgent(?Agent, ?Team)
% Team of an Agent.
teamOfAgent(peron, peronismo).
teamOfAgent(evita, peronismo).
teamOfAgent(cafiero, peronismo).
teamOfAgent(menem, peronismo).

teamOfAgent(iorio, cacoteam).
teamOfAgent(delia, cacoteam).
teamOfAgent(moyano, cacoteam).
teamOfAgent(castels, cacoteam).


% position(?Agent, ?Node).
% position(peron,a).
% position(evita,a).
% position(menem,g).
% position(cafiero,b).
% 
% position(iorio,h).
% position(delia,f).
% position(castels,b).
% position(moyano,b).

%%%%%%%%%%%%%%%%%%
% CORRECT RESULT

% Node = a,
% Team = peronismo ;
% Node = b,
% Team = cacoteam ;
% Node = f,
% Team = cacoteam ;
% Node = g,
% Team = peronismo ;
% Node = h,
% Team = cacoteam ;
% Node = c,
% Team = cacoteam ;
% Node = d,
% Team = peronismo ;
% Node = e,
% Team = cacoteam ;
% Node = i,
% Team = cacoteam ;
% Node = j,
% Team = cacoteam.

% 
position(peron,a).
position(evita,e).
position(menem,g).
position(cafiero,d).

position(iorio,h).
position(delia,f).

%%%%%%%%%%%%%%%%%%
% CORRECT RESULT

% Node = c,
% Team = none ;
% Node = i,
% Team = none ;
% Node = j,
% Team = none ;
% Node = a,
% Team = peronismo ;
% Node = d,
% Team = peronismo ;
% Node = e,
% Team = peronismo ;
% Node = f,
% Team = cacoteam ;
% Node = g,
% Team = peronismo ;
% Node = h,
% Team = cacoteam ;
% Node = b,
% Team = peronismo.

% position(peron,a).
% position(evita,a).
% position(menem,g).
% position(cafiero,b).
% 
% position(iorio,h).
% position(delia,f).
% position(castels,b).
% position(moyano,b).

%%%%%%%%%%%%%%%%%%
% CORRECT RESULT

% Node = a,
% Team = peronismo ;
% Node = b,
% Team = cacoteam ;
% Node = f,
% Team = cacoteam ;
% Node = g,
% Team = peronismo ;
% Node = h,
% Team = cacoteam ;
% Node = c,
% Team = cacoteam ;
% Node = d,
% Team = peronismo ;
% Node = e,
% Team = cacoteam ;
% Node = i,
% Team = cacoteam ;
% Node = j,
% Team = cacoteam.
