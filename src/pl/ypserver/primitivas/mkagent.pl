:- [ypagent].

mk:-
	qsave_program(ypagent,[autoload=true,stand_alone=true]).

mk3:-
	qsave_program(ypagent,[goal=start(8000),autoload=true,stand_alone=true]).

mk2 :-
	qsave_program(ypagent,[toplevel=xmain,autoload=true,stand_alone=true,goal=start(8000)]).
