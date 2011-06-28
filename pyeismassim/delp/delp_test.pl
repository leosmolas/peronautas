%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%These lines should be in every Test pl

:-['top_server_l.pl'].
:-config_server.
:-assert(context_add([])),assert(context_ignore([])), asserta((toRoot(_):-!)), asserta((toTree(_,_,_,_):-!)),asserta((xml_toRoot(_):-!)), asserta((xml_toTree(_,_,_,_):-!)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% shortcut
t:-test.



try_module(X):-[X],get_test_cases(L),do_test(L,true).
try_all:- hacer.

%++++++++++++++++++++++++++++++++++++++++++++++++++++
%                  test program
test:-load_file(delpT),
		try1(L1),try2(L2),append(L1,L2,L),do_test(L,true). 
	 

do_test([],true):-nl, nl,write(' Everythink OK !'),nl.
do_test([],false):-nl,write('- - -WARNING - - -, at least one test has failed '),nl.
do_test([(Lit,Expected)|Rest],R):- 
        nl, write('  ------------------------------------------------------------------- '), 
        answer(Lit,A),  
        nl,  write('Answer for: '), write(Lit),write(' is '),write(A),write('. Was expected: '),write(Expected),
        propagate(R,A,Expected,NewR), 
        do_test(Rest,NewR).

propagate(false,A,E,false):-A \= E, write(' <---- it FAILS !!!'). 
propagate(false,A,E,false):-A = E, write('. Test OK ').
propagate(true,A,E,false):-A \= E, write(' <---- it FAILS !!! ').
propagate(true,A,E,true):-A = E, write('. Test OK ').

%++++++++++++++++++++++++++++++++++++++++++++++++++++
% include here a list of pairs (lit, ans) where "lit" is a literal to test and "ans" is the expected answer
% example try([(,),(,),(,)]).

%try([ (fly(coco),yes), (fly(chilly),yes) ]).

%++++++++++++++++++++++++++++++++++++++++++++++++++++
% include here the rules for the examples or consult them from here

% simple examples for testing each part of the implementation

% ...............one argument and no counterarguments

try1([(a_fact,yes),(a_presumption,yes),(one_with_fact,yes),(one_with_presumption,yes),
     (no_exist,unknown),(chain,yes),(tree,yes),(strict,yes),(contradictory,undecided)]).


try2([(a2,undecided),(also_blocked,undecided), (fly(tina),yes), (~fly(tina),no), (fly(tweety),no),(~fly(tweety),yes),
       (pacifist(nixon),undecided), (has_a_gun(nixon),yes), (has_a_gun(ale),no), (has_a_gun(john),yes), (has_a_gun(sam),yes),
      (a,undecided)]).
