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


% setEnergy(+Agent, +Energy).
% sets the Energy of the Agent
setEnergy(Agent, Energy) :- retract(energy(Agent, _OldEnergy)), assertz(energy(Agent, Energy)).


% setMaxEnergy(+Agent, +Energy).
% sets the MaxEnergy of the Agent
setMaxEnergy(Agent, MaxEnergy) :- retract(maxEnergy(Agent, _OldMaxEnergy)), assertz(maxEnergy(Agent, MaxEnergy)).


% setHealth(+Agent, +Health).
% sets the Health of the Agent
setHealth(Agent, Health) :- retract(health(Agent, _OldHealth)), assertz(health(Agent, Health)).


% setMaxHealth(+Agent, +Health).
% sets the MaxHealth of the Agent
setMaxHealth(Agent, MaxHealth) :- retract(maxHealth(Agent, _OldMaxHealth)), assertz(maxHealth(Agent, MaxHealth)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- teams([Team|[OtherTeam|[]]]), !.
otherTeam(Team, OtherTeam) :- teams([OtherTeam|[Team|[]]]), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% teams(-ListofTeams).
teams([peronismo, cacoteam]).


% team(+Name, +ListofAgents).
team(peronismo, [peron, evita, menem, cafiero]).
team(cacoteam, [iorio, delia, moyano, castels]).


% maxEnergy(+Agent, -MaxEnergy).
% the MaxEnergy of the Agent.
maxEnergy(peron, 10).
maxEnergy(evita, 10).
maxEnergy(menem, 10).
maxEnergy(cafiero, 10).

maxEnergy(iorio, 10).
maxEnergy(delia, 10).
maxEnergy(moyano, 10).
maxEnergy(castels, 10).


% energy(+Agent, -Energy).
% the current Energy of the Agent.
energy(peron, 10).
energy(evita, 10).
energy(menem, 10).
energy(cafiero, 10).

energy(iorio, 10).
energy(delia, 10).
energy(moyano, 10).
energy(castels, 10).


% maxHealth(+Agent, -MaxHealth).
% the MaxHealth of the Agent.
maxHealth(peron, 10).
maxHealth(evita, 10).
maxHealth(menem, 10).
maxHealth(cafiero, 10).

maxHealth(iorio, 10).
maxHealth(delia, 10).
maxHealth(moyano, 10).
maxHealth(castels, 10).


% health(+Agent, -Health).
% the current Health of the Agent.
health(peron, 10).
health(evita, 10).
health(menem, 10).
health(cafiero, 10).

health(iorio, 10).
health(delia, 10).
health(moyano, 10).
health(castels, 10).


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
