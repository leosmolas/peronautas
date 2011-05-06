:-dynamic position/1,energy/1,last_action/1,last_action_result/1,money/1,verts/1,edges/1,intention/1,max_health/1,plan/1,max_energy/1.

%beliefs
last_action(a).
action(skip).
position(pete).
verts([]).
edges([]). % lista de functores de la forma arco(vert1,vert2)



member(X,[X|_]).
member(X,[Y|Ys]):-X\=Y,member(X,Ys).

actualizarListas([],_).
actualizarListas([X|Xs],Func):-F=..[Func,V],F,member(X,V),actualizarListas(Xs,Func).
actualizarListas([X|Xs],Func):-F=..[Func,V],F,not(member(X,V)),retract(F),F2=..[Func,[X|V]],assert(F2),actualizarListas(Xs,Func).


%argumentacion

intention(explore). % intenciones posibles: explore, recharge

argumentation:-intention(recharge),max_energy(X),energy(X),retract(intention(recharge)),assert(intention(explore)).

argumentation:-last_action_result(failed),retract(intention(_)),assert(intention(recharge)).




%planning

planning:-intention(explore),searchNeigh(N),retract(plan(_)),assert(plan([goto(N)])).

planning:-intention(recharge),retract(plan(_)),assert(plan([recharge])).

searchNeigh(N):-position(Pos),edges(E),member(edge(Pos,N),E). %inferencia mapa
searchNeigh(N):-position(Pos),edges(E),member(edge(N,Pos),E).

%ejecutor

plan([]).
exec(skip):-plan([]).
exec(Action):-plan([X|Xs]),X=..Action,retract(plan(_)),assert(plan(Xs)).

