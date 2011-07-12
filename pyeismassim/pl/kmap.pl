%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                          MAPA DE JUGUETE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RandomSeed = 1
% Number of nodes = 15 (si, tiene 14 x ser simetrico).

knode(vertex0 , unknow, 10). knode(vertex9 , unknow, 10).
knode(vertex1 , unknow,  5). knode(vertex11, unknow,  5).
knode(vertex3 , unknow,  5). knode(vertex7 , unknow,  5).
knode(vertex4 , unknow,  4). knode(vertex2 , unknow,  4).
knode(vertex8 , unknow,  3). knode(vertex6 , unknow,  3).
knode(vertex10, unknow,  4). knode(vertex5 , unknow,  4).
knode(vertex12, unknow,  2). knode(vertex13, unknow,  2).


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

kedge(vertex8, vertex6,  8).
kedge(vertex7, vertex3,  1).
kedge(vertex0, vertex9,  4).
kedge(vertex13, vertex12, 2).





