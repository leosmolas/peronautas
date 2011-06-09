:- dynamic position/2.

% teamOfAgent(+Agent, -Team)
% returns in Team the team of an Agent.
teamOfAgent(Agent, Team) :- team(Team, Agents), member(Agent, Agents), !.


% otherTeam(+Team, -OtherTeam).
% returns in OtherTeam the name of the other Team.
otherTeam(Team, OtherTeam) :- teams([Team|[OtherTeam|[]]]), !.
otherTeam(Team, OtherTeam) :- teams([OtherTeam|[Team|[]]]), !.


% teams(List of Teams).
teams([peronismo, cacoteam]).


% team(Name, List of Agents).
team(peronismo, [peron, evita, menem, cafiero]).
team(cacoteam, [iorio, delia, moyano, castels]).

% position(Agent, Team, Node).
position(peron,peronismo,a).
position(evita,peronismo,a).
position(menem,peronismo,g).
position(cafiero,peronismo,b).

position(iorio,cacoteam,h).
position(delia,cacoteam,f).
position(castels,cacoteam,b).
position(moyano,cacoteam,b).
