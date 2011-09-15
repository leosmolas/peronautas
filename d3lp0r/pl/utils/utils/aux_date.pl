%%%%%%%%%%%%%%%%%%%%%%%
%%% DATE MANAGEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%

% This file contains the predicates to admin. the date expiration.
% It is used by the server to show the user when the Software has expired
% Thus its used by delp_server.pl

:- dynamic distribution/1.
:- dynamic obsolete_version/0.
:- dynamic pre_expiration_date/1.
:- dynamic expiration_date/1.

pre_expiration_date(date(2099, 12, 1)).
expiration_date(date(2099, 7, 1)).

check_date :-
    date(DATE),
    expiration_date(EXP_DATE),
    posterior_date(DATE, EXP_DATE),
    write('This version of DeLP server is OBSOLETE'), nl,
    write('send an e-mail to delp@cs.uns.edu.ar to get the latest version.'), nl,
    assert(obsolete_version).

check_date :-
    date(DATE),
    pre_expiration_date(PRE_EXP_DATE),
    posterior_date(DATE, PRE_EXP_DATE),
    write('A new version of DeLP server is available,'), nl,
    write('send an e-mail to delp@cs.uns.edu.ar to get it.'), nl,
    !.

check_date.

posterior_date(date(Y, M, D), date(Ylimit, Mlimit, Dlimit)) :-
    Y > Ylimit, !
    ;
    Y = Ylimit,
    M > Mlimit, !
    ;
    Y = Ylimit,
    M = Mlimit,
    D > Dlimit.

