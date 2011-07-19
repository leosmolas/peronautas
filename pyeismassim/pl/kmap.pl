%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          MAPA DE JUGUETE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RandomSeed = 1
% Number of nodes = 15 (si, tiene 14 x ser simetrico).

myName(evita).
% k(position(vasco, vertex0)).

my_team(peronismo).

k(position(peron, vertex8)).
k(position(evita, vertex13)).
k(position(menem, vertex7)).
k(position(cafiero, vertex0)).

k(position(iorio, vertex9)).

k(node(vertex0 , peronismo, 10)). k(node(vertex9 , none, 10)).
k(node(vertex1 , peronismo,  5)). k(node(vertex11, none,  5)).
k(node(vertex3 , none,  5)). k(node(vertex7 , peronismo,  5)).
k(node(vertex4 , none,  4)). k(node(vertex2 , peronismo,  4)).
k(node(vertex8 , peronismo,  3)). k(node(vertex6 , none,  3)).
k(node(vertex10, none,  4)). k(node(vertex5 , peronismo,  4)).
k(node(vertex12, none,  2)). k(node(vertex13, peronismo,  2)).

k(edge(vertex5, vertex8,  4)). k(edge(vertex6, vertex10, 4)).
k(edge(vertex8, vertex7,  1)). k(edge(vertex3, vertex6,  1)).
k(edge(vertex2, vertex5,  9)).  k(edge(vertex4, vertex10, 9)).
k(edge(vertex2, vertex8,  9)). k(edge(vertex6, vertex4,  9)).
k(edge(vertex5, vertex7,  9)). k(edge(vertex3, vertex10, 9)).
k(edge(vertex2, vertex7,  6)). k(edge(vertex3, vertex4,  6)).

k(edge(vertex5, vertex1,  9)). k(edge(vertex10, vertex11, 9)).
k(edge(vertex5, vertex0,  5)). k(edge(vertex9, vertex10, 5)).
k(edge(vertex0, vertex7,  2)). k(edge(vertex3, vertex9, 2)).
k(edge(vertex1, vertex0,  7)). k(edge(vertex9, vertex11, 7)).
k(edge(vertex1, vertex13, 6)). k(edge(vertex12, vertex11, 6)).
k(edge(vertex13, vertex0, 6)). k(edge(vertex9, vertex12, 6)).

k(edge(vertex6, vertex8, 8)).
k(edge(vertex3, vertex7, 1)).
k(edge(vertex9, vertex0, 4)).
k(edge(vertex12, vertex13, 2)).

k(edge(vertex8, vertex5, 4)). k(edge(vertex10, vertex6, 4)).
k(edge(vertex7, vertex8, 1)). k(edge(vertex6, vertex3, 1)).
k(edge(vertex5, vertex2, 9)).  k(edge(vertex10, vertex4, 9)).
k(edge(vertex8, vertex2, 9)). k(edge(vertex4, vertex6, 9)).
k(edge(vertex7, vertex5, 9)). k(edge(vertex10, vertex3, 9)).
k(edge(vertex7, vertex2, 6)). k(edge(vertex4, vertex3, 6)).

k(edge(vertex1, vertex5, 9)). k(edge(vertex11, vertex10, 9)).
k(edge(vertex0, vertex5, 5)). k(edge(vertex10, vertex9, 5)).
k(edge(vertex7, vertex0, 2)). k(edge(vertex9, vertex3, 2)).
k(edge(vertex0, vertex1, 7)). k(edge(vertex11, vertex9, 7)).
k(edge(vertex13, vertex1, 6)). k(edge(vertex11, vertex12, 6)).
k(edge(vertex0, vertex13, 6)). k(edge(vertex12, vertex9, 6)).

k(edge(vertex8, vertex6,  8)).
k(edge(vertex7, vertex3,  1)).
k(edge(vertex0, vertex9,  4)).
k(edge(vertex13, vertex12, 2)).