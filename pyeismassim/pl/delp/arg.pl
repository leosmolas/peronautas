:- [delp].

:- consult('arg.delp').

:-comparison_on(defeater2assumption),comparison_on(more_specific).

is_a_built_in(mult(X,Y,Z)).
is_a_built_in(sum(X,Y,Z)).
is_a_built_in(sust(X,Y,Z)).
is_a_built_in(power(X,Y,Z)).
is_a_built_in(greater(X,Y)).
is_a_built_in(less(X,Y)).
is_a_built_in(greaterEq(X,Y)).
is_a_built_in(lessEq(X,Y)).
is_a_built_in(equal(X,Y)).
is_a_built_in(notEqual(X,Y)).


mult(X,Y,Z):- Z is X * Y.
sum(X,Y,Z):- Z is X + Y.
sust(X,Y,Z):- Z is X - Y.
power(X,Y,Z):- Z is X ** Y.
greater(X,Y):- X > Y.
less(X,Y):- X < Y.
greaterEq(X,Y):- X >= Y.
lessEq(X,Y):- X =< Y.
equal(X,Y):- X =:= Y.
notEqual(X,Y):- X \= Y.
