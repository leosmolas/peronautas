% BUILT-INS
% This file contains the facts and preidcates that will used by default as i
% built_ins by delp.pl
% Usage: a new entry of "is_a_built_in(B)" states that a new built-in "B" exists,
% (if necessary) the proper Prolog code for "B" should be added.
% For using a built-in DeLP will execute: "is_a_built_in(H), call(H)"
% See the examples below.

% is_a_built_in/1 indicates that the built-in exists
is_a_built_in(_ < _). 
is_a_built_in(_ > _).
is_a_built_in(_ >= _).
is_a_built_in(_ =< _).
is_a_built_in(sum(_,_,_)).
is_a_built_in(difference(_,_,_)).

sum(S1,S2,Result) :- Result is S1 + S2.
difference(S1,S2,Result) :- Result is S1 - S2.

% More built-ins can be added here or in our own program code.
