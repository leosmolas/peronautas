:- dynamic hedge/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                Operations                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% insertEdge(+Node1, +Node2, +Value)
% creates an edge between Node1 and Node2 with value Value
% insertEdge(Node1, Node2, Value) :- assertz(hedge(Node1, Node2, Value)), assertz(hedge(Node2, Node1, Value)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Consults                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Representation                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%hedge(a, b, 1).
%hedge(b, a, 1).

%hedge(a, d, 1).
%hedge(d, a, 1).

%hedge(b, c, 1).
%hedge(c, b, 1).

%hedge(b, e, 1).
%hedge(e, b, 1).

%hedge(c, h, 1).
%hedge(h, c, 1).

%hedge(d, e, 1).
%hedge(e, d, 1).

%hedge(d, g, 1).
%hedge(g, d, 1).

%hedge(e, f, 1).
%hedge(f, e, 1).

%hedge(e, h, 1).
%hedge(h, e, 1).

%hedge(g, h, 1).
%hedge(h, g, 1).

%hedge(c, i, 1).
%hedge(i, c, 1).

%hedge(c, j, 1).
%hedge(j, c, 1).
