:- style_check(-discontiguous).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          MAPA DE JUGUETE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RandomSeed = 1
% Number of nodes = 15 (si, tiene 14 x ser simetrico).

k(nodeTeam(0, vertex0,  none)). 
k(nodeTeam(0, vertex1,  none)). 
k(nodeTeam(0, vertex3,  none)). 
k(nodeTeam(0, vertex4,  none)). 
k(nodeTeam(0, vertex8,  none)). 
k(nodeTeam(0, vertex10, none)). 
k(nodeTeam(0, vertex12, none)). 
k(nodeTeam(0, vertex9,  none)).
k(nodeTeam(0, vertex11, none)).
k(nodeTeam(0, vertex7,  none)).
k(nodeTeam(0, vertex2,  none)).
k(nodeTeam(0, vertex6,  none)).
k(nodeTeam(0, vertex5,  none)).
k(nodeTeam(0, vertex13, none)).

k(nodeValue(vertex0,  10)). 
k(nodeValue(vertex1,   5)). 
k(nodeValue(vertex3,   5)). 
k(nodeValue(vertex4,   4)). 
k(nodeValue(vertex8,   3)). 
k(nodeValue(vertex10,  4)). 
k(nodeValue(vertex12,  2)). 
k(nodeValue(vertex9,  10)).
k(nodeValue(vertex11,  5)).
k(nodeValue(vertex7,   5)).
k(nodeValue(vertex2,   4)).
k(nodeValue(vertex6,   3)).
k(nodeValue(vertex5,   4)).
k(nodeValue(vertex13,  2)).


h(nodeTeam(0, vertex0,  none)). 
h(nodeTeam(0, vertex1,  none)). 
h(nodeTeam(0, vertex3,  none)). 
h(nodeTeam(0, vertex4,  none)). 
h(nodeTeam(0, vertex8,  none)). 
h(nodeTeam(0, vertex10, none)). 
h(nodeTeam(0, vertex12, none)). 
h(nodeTeam(0, vertex9,  none)).
h(nodeTeam(0, vertex11, none)).
h(nodeTeam(0, vertex7,  none)).
h(nodeTeam(0, vertex2,  none)).
h(nodeTeam(0, vertex6,  none)).
h(nodeTeam(0, vertex5,  none)).
h(nodeTeam(0, vertex13, none)).

%h(nodeValue(vertex0,  10)). 
%h(nodeValue(vertex1,   5)). 
%h(nodeValue(vertex3,   5)). 
%h(nodeValue(vertex4,   4)). 
%h(nodeValue(vertex8,   3)). 
%h(nodeValue(vertex10,  4)). 
%h(nodeValue(vertex12,  2)). 
%h(nodeValue(vertex9,  10)).
%h(nodeValue(vertex11,  5)).
%h(nodeValue(vertex7,   5)).
%h(nodeValue(vertex2,   4)).
%h(nodeValue(vertex6,   3)).
%h(nodeValue(vertex5,   4)).
%h(nodeValue(vertex13,  2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                      Edges                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

k(edge(vertex5,  vertex8,  4)). 
k(edge(vertex8,  vertex7,  1)). 
k(edge(vertex2,  vertex5,  9)). 
k(edge(vertex2,  vertex8,  9)). 
k(edge(vertex5,  vertex7,  9)). 
k(edge(vertex2,  vertex7,  6)). 

k(edge(vertex6,  vertex10, 4)).
k(edge(vertex3,  vertex6,  1)).
k(edge(vertex4,  vertex10, 9)).
k(edge(vertex6,  vertex4,  9)).
k(edge(vertex3,  vertex10, 9)).
k(edge(vertex3,  vertex4,  6)).

k(edge(vertex5,  vertex1,  9)). 
k(edge(vertex5,  vertex0,  5)). 
k(edge(vertex0,  vertex7,  2)). 
k(edge(vertex1,  vertex0,  7)). 
k(edge(vertex1,  vertex13, 6)). 
k(edge(vertex13, vertex0,  6)). 

k(edge(vertex10, vertex11, 9)).
k(edge(vertex9,  vertex10, 5)).
k(edge(vertex3,  vertex9,  2)).
k(edge(vertex9,  vertex11, 7)).
k(edge(vertex12, vertex11, 6)).
k(edge(vertex9,  vertex12, 6)).

k(edge(vertex8,  vertex6,  8)).
k(edge(vertex7,  vertex3,  1)).
k(edge(vertex0,  vertex9,  4)).
k(edge(vertex13, vertex12, 2)).

k(edge(vertex8, vertex5, 4)).
k(edge(vertex7, vertex8, 1)).
k(edge(vertex5, vertex2, 9)).
k(edge(vertex8, vertex2, 9)).
k(edge(vertex7, vertex5, 9)).
k(edge(vertex7, vertex2, 6)).

k(edge(vertex10, vertex6, 4)).
k(edge(vertex6, vertex3, 1)).
k(edge(vertex10, vertex4, 9)).
k(edge(vertex4, vertex6, 9)).
k(edge(vertex10, vertex3, 9)).
k(edge(vertex4, vertex3, 6)).

k(edge(vertex1, vertex5, 9)).
k(edge(vertex0, vertex5, 5)).
k(edge(vertex7, vertex0, 2)).
k(edge(vertex0, vertex1, 7)).
k(edge(vertex13, vertex1, 6)).
k(edge(vertex0, vertex13, 6)).

k(edge(vertex11, vertex10, 9)).
k(edge(vertex10, vertex9, 5)).
k(edge(vertex9, vertex3, 2)).
k(edge(vertex11, vertex9, 7)).
k(edge(vertex11, vertex12, 6)).
k(edge(vertex12, vertex9, 6)).

k(edge(vertex6, vertex8, 8)).
k(edge(vertex3, vertex7, 1)).
k(edge(vertex9, vertex0, 4)).
k(edge(vertex12, vertex13, 2)).
%h(edge(vertex5,  vertex8,  4)). 
%h(edge(vertex8,  vertex7,  1)). 
%h(edge(vertex2,  vertex5,  9)). 
%h(edge(vertex2,  vertex8,  9)). 
%h(edge(vertex5,  vertex7,  9)). 
%h(edge(vertex2,  vertex7,  6)). 
%
%h(edge(vertex6,  vertex10, 4)).
%h(edge(vertex3,  vertex6,  1)).
%h(edge(vertex4,  vertex10, 9)).
%h(edge(vertex6,  vertex4,  9)).
%h(edge(vertex3,  vertex10, 9)).
%h(edge(vertex3,  vertex4,  6)).
%
%h(edge(vertex5,  vertex1,  9)). 
%h(edge(vertex5,  vertex0,  5)). 
%h(edge(vertex0,  vertex7,  2)). 
%h(edge(vertex1,  vertex0,  7)). 
%h(edge(vertex1,  vertex13, 6)). 
%h(edge(vertex13, vertex0,  6)). 
%
%h(edge(vertex10, vertex11, 9)).
%h(edge(vertex9,  vertex10, 5)).
%h(edge(vertex3,  vertex9,  2)).
%h(edge(vertex9,  vertex11, 7)).
%h(edge(vertex12, vertex11, 6)).
%h(edge(vertex9,  vertex12, 6)).
%
%h(edge(vertex8,  vertex6,  8)).
%h(edge(vertex7,  vertex3,  1)).
%h(edge(vertex0,  vertex9,  4)).
%h(edge(vertex13, vertex12, 2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    Position                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

currentStep(0).

h(position(0, peron, vertex0)).
h(position(0, evita, vertex13)).
h(position(0, machinea, vertex7)).
h(position(0, cavallo, vertex8)).


h(position(0, iorio, vertex5)).
h(position(0, moyano, vertex5)).
h(position(0, castels, vertex2)).
h(position(0, delia, vertex2)).

team(peron, peronismo).
team(evita, peronismo).
team(machinea, peronismo).
team(cavallo, peronismo).

team(iorio, cacoteam).
team(moyano, cacoteam).
team(castels, cacoteam).
team(delia, cacoteam).

team(peronismo).
team(cacoteam).
