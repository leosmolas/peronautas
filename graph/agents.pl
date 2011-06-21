:- dynamic position/3.
:- dynamic energy/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% moveAgent(+Agent, +Node).
% Moves the agent from his current position to Node.
moveAgent(Agent, Node) :- node(Node, _, _), retract(position(Agent, Team, _OldNode)), assertz(position(Agent, Team, Node)), !.


% travel(+Agent, +Node).
% Moves the agent from ActualNode to Node.  There must be and edge between these two nodes and the agent must have the necessary energy to travel.
travel(Agent, Node) :-  energy(Agent, Energy),                        
                        position(Agent, Team, ActualNode),
                        edge(ActualNode, Node, Cost),
                        NewEnergy is Energy - Cost, 
                        NewEnergy >= 0,
                        retract(position(Agent, Team, ActualNode)), 
                        assertz(position(Agent, Team, Node)), 
                        setEnergy(Agent, NewEnergy), !.


% setEnergy(+Agent, +Energy).
% sets the Energy of the Agent
setEnergy(Agent, Energy) :- retract(energy(Agent, _OldEnergy)), assertz(energy(Agent, Energy)).


% setMaxEnergy(+Agent, +Energy).
% sets the MaxEnergy of the Agent
setMaxEnergy(Agent, MaxEnergy) :- retract(energy(Agent, _OldMaxEnergy)), assertz(energy(Agent, MaxEnergy)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% teamOfAgent(+Agent, -Team)
% returns in Team the team of an Agent.
teamOfAgent(Agent, Team) :- team(Team, Agents), member(Agent, Agents), !.


% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- teams([Team|[OtherTeam|[]]]), !.
otherTeam(Team, OtherTeam) :- teams([OtherTeam|[Team|[]]]), !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% teams(List of Teams).
teams([peronismo, cacoteam]).


% team(Name, List of Agents).
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


% energy(Agent, Energy).
% the current Energy of the Agent.
energy(peron, 10).
energy(evita, 10).
energy(menem, 10).
energy(cafiero, 10).
energy(iorio, 10).
energy(delia, 10).
energy(moyano, 10).
energy(castels, 10).

% position(Agent, Team, Node).
position(peron,peronismo,a).
position(evita,peronismo,a).
position(menem,peronismo,g).
position(cafiero,peronismo,b).

position(iorio,cacoteam,h).
position(delia,cacoteam,f).
position(castels,cacoteam,b).
position(moyano,cacoteam,b).
