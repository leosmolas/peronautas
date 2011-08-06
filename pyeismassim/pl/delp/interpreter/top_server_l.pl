%
% This file contains the predicate to start a delp_server (xmain)
% It uses:
%    - delp_server = to call start_server, which starts the delp server
%


:- [delp].
:- [tree_swi].
:- [tree_xml].
:- [delp_server].

%%%%%%%%%%%%%%%%%%%%%%%
%%% AUX PREDICATES  %%%
%%%%%%%%%%%%%%%%%%%%%%%
:-[aux_predicates].

%%%%%%%%%%%%%%%%%%%%%%%
%%%    BUILT INS    %%%
%%%%%%%%%%%%%%%%%%%%%%%
:-[aux_built_ins].

%%%%%%%%%%%%%%%%%%%%%%%
%%%%  OUTPUT FILE  %%%%
%%%%%%%%%%%%%%%%%%%%%%%
%:-[aux_output_file].


% The win_window_pos([show(false)]) was added to hide the server window in this version of the server
xmain:- %%%current_prolog_flag(argv,Args),write(Args),get_char(_), %...uncomment this line to debug
	%win_window_pos([show(false)]),
    %catch(main,_,(tell('server.log'),write('ERROR!'),nl,told)),halt.
	main.

main:- %...run DeLP server with ARGS options...
        current_prolog_flag(argv,[_|ARGS]),start_app(ARGS).

main:- %...bottom case - bad input parameters...
	current_prolog_flag(argv,[_|Args]),write('Bad input parameters: '),write(Args),nl,
	tell('server.log'),write('Bad input parameters: '),write(Args),nl,told.



