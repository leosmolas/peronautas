%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          MAPA DE JUGUETE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RandomSeed = 1
% Number of nodes = 15 (si, tiene 14 x ser simetrico).

knode(vertex0 , d3lp0r, 10). knode(vertex9 , none, 10).
knode(vertex1 , d3lp0r,  5). knode(vertex11, none,  5).
knode(vertex3 , none,  5). knode(vertex7 , none,  5).
knode(vertex4 , none,  4). knode(vertex2 , none,  4).
knode(vertex8 , none,  3). knode(vertex6 , none,  3).
knode(vertex10, none,  4). knode(vertex5 , d3lp0r,  4).
knode(vertex12, none,  2). knode(vertex13, d3lp0r,  2).


kedge(vertex5, vertex8,  4). kedge(vertex6, vertex10, 4).
kedge(vertex8, vertex7,  1). kedge(vertex3, vertex6,  1).
kedge(vertex2, vertex5,  9).  kedge(vertex4, vertex10, 9).
kedge(vertex2, vertex8,  9). kedge(vertex6, vertex4,  9).
kedge(vertex5, vertex7,  9). kedge(vertex3, vertex10, 9).
kedge(vertex2, vertex7,  6). kedge(vertex3, vertex4,  6).

kedge(vertex5, vertex1,  9). kedge(vertex10, vertex11, 9).
kedge(vertex5, vertex0,  5). kedge(vertex9, vertex10, 5).
kedge(vertex0, vertex7,  2). kedge(vertex3, vertex9, 2).
kedge(vertex1, vertex0,  7). kedge(vertex9, vertex11, 7).
kedge(vertex1, vertex13, 6). kedge(vertex12, vertex11, 6).
kedge(vertex13, vertex0, 6). kedge(vertex9, vertex12, 6).

kedge(vertex6, vertex8, 8).
kedge(vertex3, vertex7, 1).
kedge(vertex9, vertex0, 4).
kedge(vertex12, vertex13, 2).



kedge(vertex8, vertex5, 4). kedge(vertex10, vertex6, 4).
kedge(vertex7, vertex8, 1). kedge(vertex6, vertex3, 1).
kedge(vertex5, vertex2, 9).  kedge(vertex10, vertex4, 9).
kedge(vertex8, vertex2, 9). kedge(vertex4, vertex6, 9).
kedge(vertex7, vertex5, 9). kedge(vertex10, vertex3, 9).
kedge(vertex7, vertex2, 6). kedge(vertex4, vertex3, 6).

kedge(vertex1, vertex5, 9). kedge(vertex11, vertex10, 9).
kedge(vertex0, vertex5, 5). kedge(vertex10, vertex9, 5).
kedge(vertex7, vertex0, 2). kedge(vertex9, vertex3, 2).
kedge(vertex0, vertex1, 7). kedge(vertex11, vertex9, 7).
kedge(vertex13, vertex1, 6). kedge(vertex11, vertex12, 6).
kedge(vertex0, vertex13, 6). kedge(vertex12, vertex9, 6).



kedge(vertex8, vertex6,  8).
kedge(vertex7, vertex3,  1).
kedge(vertex0, vertex9,  4).
kedge(vertex13, vertex12, 2).





