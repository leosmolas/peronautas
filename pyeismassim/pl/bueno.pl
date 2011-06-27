:- ['delp/delp.pl'].

:- consult('bueno.delp').

is_a_built_in(multiplicar(X,Y,Z)).
is_a_built_in(mayor(X,Y)).
:-comparison_on(defeater2assumption),comparison_on(more_specific).

multiplicar(X,Y,Z):- Z is X*Y.
mayor(X,Y):- X>Y.

