%padre(homero, a(b(c(bart, maggie)))).
padre(homero, [lisa2, a([x(y)], c)]).
%padre(homero, lisa).

unario(0).
unario(s(X)):- unario(X).

suma(0, X, X).
suma(s(X), Y, s(Z)):- suma(X, Y, Z).

list_sum([], 0).
list_sum([Head | Tail], TotalSum) :- list_sum(Tail, Sum1), TotalSum is Head + Sum1.