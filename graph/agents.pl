:- dynamic position/3.
:- dynamic energy/2.

% teamOfAgent(+Agent, -Team)
% returns in Team the team of an Agent.
teamOfAgent(Agent, Team) :- team(Team, Agents), member(Agent, Agents), !.


% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- teams([Team|[OtherTeam|[]]]), !.
otherTeam(Team, OtherTeam) :- teams([OtherTeam|[Team|[]]]), !.


% moveAgent(+Agent, +Node).
% Moves the agent from his current position to Node.
% here we could check if the agent can move and if so substract the value of the edge he takes from his total energy.
moveAgent(Agent, Node) :- node(Node, _, _), retract(position(Agent, Team, _OldNode)), assertz(position(Agent, Team, Node)).


% setEnergy(+Agent, +Energy).
% sets the Energy of the Agent
setEnergy(Agent, Energy) :- retract(energy(Agent, _OldEnergy)), assertz(energy(Agent, Energy)).

% teams(List of Teams).
teams([peronismo, cacoteam]).


% team(Name, List of Agents).
team(peronismo, [peron, evita, menem, cafiero]).
team(cacoteam, [iorio, delia, moyano, castels]).


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
