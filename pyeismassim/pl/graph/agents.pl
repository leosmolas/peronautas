:- dynamic hposition/2.
:- dynamic energy/2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% moveAgent(+Agent, +Node).
% Moves the agent from his current position to Node.
moveAgent(Agent, Node) :- 
    hnode(Node, _, _), 
    retract( hposition(Agent, _OldNode) ), 
    assertz( hposition(Agent, Node)     ), 
    !.



% travel(+Agent, +Node).
% Moves the agent from ActualNode to Node.  There must be and edge between these two nodes and the agent must have the necessary energy to travel.
travel(Agent, Node) :-  
    energy(Agent, Energy),                        
    hposition(Agent, ActualNode),
    hedge(ActualNode, Node, Cost),
    NewEnergy is Energy - Cost, 
    NewEnergy >= 0,
    retract(hposition(Agent, ActualNode)), 
    assertz(hposition(Agent, Node)), 
    setEnergy(Agent, NewEnergy), 
    !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- 
    teams(OtherTeam), 
    OtherTeam \= Team, 
    !.


listOfTeams(Teams) :- 
    findall(Team, team(Team), Teams).



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
teamOfAgent(peron,   peronismo).
teamOfAgent(evita,   peronismo).
teamOfAgent(cafiero, peronismo).
teamOfAgent(menem,   peronismo).
teamOfAgent(iorio,   cacoteam).
teamOfAgent(delia,   cacoteam).
teamOfAgent(moyano,  cacoteam).
teamOfAgent(castels, cacoteam).



% hposition(?Agent, ?Node).
% hposition(peron,a).
% hposition(evita,a).
% hposition(menem,g).
% hposition(cafiero,b).
% 
% hposition(iorio,h).
% hposition(delia,f).
% hposition(castels,b).
% hposition(moyano,b).

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
%hposition(peron,a).
%hposition(evita,e).
%hposition(menem,g).
%hposition(cafiero,d).

%hposition(iorio,h).
%hposition(delia,f).

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

% hposition(peron,a).
% hposition(evita,a).
% hposition(menem,g).
% hposition(cafiero,b).
% 
% hposition(iorio,h).
% hposition(delia,f).
% hposition(castels,b).
% hposition(moyano,b).

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
