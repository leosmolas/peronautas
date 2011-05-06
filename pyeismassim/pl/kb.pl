:-dynamic position/1,energy/1,last_action/1,last_action_result/1,money/1,verts/1,edges/1.

last_action(a).
action(skip).
position(pete).
verts([]).
edges([]).

member(X,[X|_]).
member(X,[Y|Ys]):-X\=Y,member(X,Ys).

actualizarListas([],_).
actualizarListas([X|Xs],Func):-F=..[Func,V],F,member(X,V),actualizarListas(Xs,Func).
actualizarListas([X|Xs],Func):-F=..[Func,V],F,not(member(X,V)),retract(F),F2=..[Func,[X|V]],assert(F2),actualizarListas(Xs,Func).