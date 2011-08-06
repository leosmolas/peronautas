% This file contains the predicates to admin the server compariosn criterion 
% functionalities. It allows to add or remove criterions from a pool, and to 
% enable them.
% This file is used by delp_server.pl
% Last Edit by Gotti 
%
:-dynamic(possible_criterion/1).
:-dynamic(last_criterions/1).

possible_criterion(defeater2assumption).
possible_criterion(more_specific).

add_posible_criterion(F, C) :-
    load_files([F], [silent(true)]),
    assert(possible_criterion(C)).

remove_possible_criterion(C) :-
    retract(possible_criterion(C)).
    
enable_criterion(C) :-
    possible_criterion(C),
    findall(X, criterion(X), L),
    assert(last_criterions(L)),
    retractall(criterion(X)),
    assert(criterion(C)).

enable_multiple_criterion([]).

enable_multiple_criterion([C | List]) :-
    assert(criterion(C)),
    enable_multiple_criterion(List).

enable_last_criterions.

