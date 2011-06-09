:- dynamic edge/3.

% insertEdge(+Node1, +Node2, +Value)
% creates an edge between Node1 and Node2 with value Value
insertEdge(Node1, Node2, Value) :- assertz(edge(Node1, Node2, Value)), assertz(edge(Node2, Node1, Value)).

edge(a, b, 1).
edge(b, a, 1).

edge(a, d, 1).
edge(d, a, 1).

edge(b, c, 1).
edge(c, b, 1).

edge(b, e, 1).
edge(e, b, 1).

edge(c, h, 1).
edge(h, c, 1).

edge(d, e, 1).
edge(e, d, 1).

edge(d, g, 1).
edge(g, d, 1).

edge(e, f, 1).
edge(f, e, 1).

edge(e, h, 1).
edge(h, e, 1).

edge(g, h, 1).
edge(h, g, 1).

edge(c, i, 1).
edge(i, c, 1).

edge(c, j, 1).
edge(j, c, 1).
