:- [delp]. % intérprete

:- consult('arg.delp'), % reglas de argumentación
   consult('mundo1.delp'). % hechos asertados en una situación del mundo particular

% criterios de comparación
% greaterArgValue está definido más abajo, y lo que hace es buscar los argValue de los argumentos, si es que existen, para compararlos
:- comparison_on(greaterArgValue),
% defeater2assumption es un criterio que establece derrota cuando uno de los argumentos es una asunción, es derrotado por cualquier otra cosa que tenga algo.
   comparison_on(defeater2assumption),
% more_specific es la espeficidad de siempre.
   comparison_on(more_specific).

% todos los predicados que siguen son operaciones aritméticas y de comparación, para que los use delp.
is_a_built_in(mult(X,Y,Z)).
is_a_built_in(add(X,Y,Z)).
is_a_built_in(sust(X,Y,Z)).
is_a_built_in(power(X,Y,Z)).
is_a_built_in(greater(X,Y)).
is_a_built_in(less(X,Y)).
is_a_built_in(greaterEq(X,Y)).
is_a_built_in(lessEq(X,Y)).
is_a_built_in(equal(X,Y)).
is_a_built_in(notEqual(X,Y)).


mult(X,Y,Z)    :- Z is X * Y.
add(X,Y,Z)     :- Z is X + Y.
sust(X,Y,Z)    :- Z is X - Y.
power(X,Y,Z)   :- Z is X ** Y.
greater(X,Y)   :- X > Y.
less(X,Y)      :- X < Y.
greaterEq(X,Y) :- X >= Y.
lessEq(X,Y)    :- X =< Y.
equal(X,Y)     :- X =:= Y. % este es el igual, pero no instancia, solo chequea igualdad. (O sea, no es el mismo que el =, que si instancia.)
notEqual(X,Y)  :- X \= Y.

% criterio de comparación
greaterArgValue(arg(Ac, _), arg(Bc, _)) :-
%     writeln(Ac),
%     writeln(Bc),
    member(s_rule(argValue(ValA),true), Ac), !,
%    writeln(ValA),
    member(s_rule(argValue(ValB),true), Bc), !,
%     writeln(ValB),
%     writeln('greater'),
    ValA > ValB.