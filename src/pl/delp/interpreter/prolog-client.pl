% ---------------------------
% DeLP top-level: an example of a prolog client for the DeLP server 
% ---------------------------
% (c) 2006 Artificial Intelligence Research and Development Laboratory (LIDIA). 
% Universidad Nacional del Sur. Bahia Blanca. Argentina. e-mail: delp@cs.uns.edu.ar
% Alejandro J. Garcia - Nicolas D. Rotstein - Mariano Tucat - Sebastian Gottifredi - Guillermo
% R. Simari

% Main predicates: delp/0 delp/1 and delp/2.

% This command load an interface to facilitate the communication with DeLP server.
%:- initialization load_foreign_library(interface).
:- [interface].
%:- initialization window_title(_, 'Prolog client for DeLP-Server').
:- dynamic server_rule/1, salir/0.
:- dynamic '-<'/2, '<-'/2.

:- at_halt(assert(salir)).

% This operator definitions are needed for sending information to the DeLP server.
:- op(1101, xfx, -<).  % Defeasible Rules
:- op(1101, xfx, <-).  % Strict Rules
:- op(190,  fx,  ~).   % Strong negation
:- op(191,  fx,  not). % Default negation
:- op(192,  fx,  '?'). % To avoid requiring parentheses when asking for an 
                       % explanation.

% Starts a top-level client for DeLP server in the same host and Port 8000 
% (default).
delp :-
    serverIP(SIP),
    delp(SIP,8000). 

% Starts a top-level client for DeLP server in the same host and in a 
% particular port.
delp(PORT) :- 
    serverIP(SIP), 
    delp(SIP,PORT).

%%%% starts a top-level client for DeLP server in an specific host and port
delp(SIP,PORT):-
    write('--- Prolog Client - Personal Copy for Univ. de Catamarca ---'), nl,
    nl,
    write('Available commands:'), nl,
    nl,
    write('.  quit: terminates this top-level agent'), nl,
    write('.  stop: kills the associated server and terminates this top-level agent'), nl,
    write('.  tree: draws the dialectical trees associated with the last explanation query using the SWI trees'), nl,
    write('.  name(Agent-name): informs the server a name for this agent '), nl,
    write('.  [[ ...list of terms representing the context sets... ], query]: consults the server about \'query\''), nl,
    write('.  [[ ...list of terms representing the context sets... ], ? query]: consults the server for an explanation about \'query\''), nl,
    write('.  anything else is considered a query'), nl,
    nl,
    connect(ID,SIP,PORT), % this predifined primitive from "interface.pl" connects to a DeLP server
    write('Connected to '), write(SIP), write(' IP address, at port '), write(PORT), nl,
    nl,
    repeat,  % loop until the quit command is typed
    write('-< '),
    catch(
        read(X),
        _,
        (
            write('Invalid query or command'), nl,
            nl,
            fail
        )
    ),
    once(process(X,ID)),
    X = quit. 

delp(SIP,PORT):-
    write('Available commands:'),nl,nl,
    write('.  quit: terminates this top-level agent'),nl,
    write('.  stop: kills the associated server and terminates this top-level agent'),nl,
    write('.  tree: draws the dialectical trees associated with the last explanation query using the SWI trees'),nl,
    write('.  name(Agent-name): informs the server a name for this agent '),nl,
    write('.  [[ ...list of terms representing the context sets... ], query]: consults the server about \'query\''),nl,
    write('.  [[ ...list of terms representing the context sets... ], ? query]: consults the server for an explanation about \'query\''),nl,
    write('.  anything else is considered a query'),nl,nl,
    connect(ID,SIP,PORT), % This predifined primitive from "interface.pl" connects to a DeLP server.
    write('Connected to '),write(SIP),write(' IP address, at port '),write(PORT),nl,nl,
        repeat,  % Loop until the quit command is typed.
        write('-< '),
    catch(
        read(X),
        _,
        (
            write('Invalid query or command'), nl,
            nl,
            fail
        )
    ),
    once(process(X, ID)),
    X = quit. 

delp(SIP,PORT) :-
    write('No server at '), write(SIP), write(' IP address,and port '), write(PORT), nl,
    nl,
    write('Retry? (y/n) '),
    catch(
        read(X),
        _,
        (
            write('Invalid query or command'), nl,
            nl,
            fail
        )
    ),
    retryConnection(X).
        
retryConnection(y) :- delp.
retryConnection(n) :- halt.

process(quit, _)              :- halt.
process(tree, _)              :- thread_create(shell('tree -show'),_,[]), !.
process(view, _)              :- thread_create(shell('viewer.bat explanation.xml'),_,[]), !.
process(stop, ServerID)       :- send_msg(ServerID, stop), halt.
process(reload, ServerID)     :- send_msg(ServerID, reload), !.
process(name(NAME), ServerID) :- send_msg(ServerID, name(NAME)), !.
%process(exp(QUERY),ID):-!,ask_server(exp([],QUERY),ID).
%process(exp(KB,QUERY),ID):-!,ask_server(exp(KB,QUERY),ID).
%process((KB,QUERY),ID):- ask_server((KB,QUERY),ID),!.
%process(QUERY,ID):- is_contextual(QUERY),!,ask_server(QUERY,ID).
%process(QUERY,ID):- ask_server(([],QUERY),ID).

% Shows and saves the rules stored in the server.
process(listing,ServerID) :-  
    send_msg(ServerID,listing),
    recv_msg(ServerID,PROGRAM),
    retractall(server_rule(_)),
    nl,
    write('(Program stored in the server)'), nl,
    nl,
    showrules(PROGRAM),
    nl,
    !.

%process((File,Query),ID):-parse_file(File,Context),ask_server([Context,Query],ID).
process(QUERY,ID) :-
    parse_query(QUERY,PQ),
    ask_server(PQ,ID).

showrules([]).
showrules([Rule | RuleRest]) :- 
    assert(server_rule(Rule)),
    write('     '),
    portray_clause(Rule),
    showrules(RuleRest).

% Este predicado agrega a los literales que representan hechos '<- true' para 
% que puedan ser procesados por el server.
parse_query([Context, Q], [Context_arranged, Q]) :- parse_context(Context, Context_arranged).
parse_query(Q, Q).

parse_context([], []).
parse_context([Set | Rest], [Set_arranged | Rest_arranged]) :-
    Set =.. [Op | S],
    parse_set(S, S_arranged),
    parse_context(Rest, Rest_arranged),
    Set_arranged =.. [Op | S_arranged].

parse_set([], []).
parse_set([Rule | Set_rest], Result) :-
    Rule = '-<'(_A, _B),
    parse_set(Set_rest, Set_rest_arranged),
    append([Rule], Set_rest_arranged, Result).

parse_set([Rule | Set_rest], Result) :-
    Rule = '<-'(_A, _B),
    parse_set(Set_rest, Set_rest_arranged),
    append([Rule], Set_rest_arranged, Result).

parse_set([file(Name) | Set_rest], Result) :-
    parse_file(Name, Context),
    parse_set(Set_rest, Set_rest_arranged),
    append(Context, Set_rest_arranged, Result).

parse_set([Rule | Set_rest],Result) :-
    parse_set(Set_rest, Set_rest_arranged),
    append([(Rule <- true)], Set_rest_arranged, Result).

parse_file(File_input, Sal) :-
    atom_concat(File_input, '.delp', File),
    read_file_to_terms(File, Sal, []).
    %ignore(read_file_to_terms(File, Sal, [])).
    %catch(read_file_to_terms(File, Sal, []),_,no_file_msg(dont_fail,File)).

no_file_msg(dont_fail, File) :-
        write('The file '), write(File), write(' does not exist'), nl.

is_contextual( p(_,_)  ).
is_contextual( np(_,_) ).
is_contextual( r(_,_)  ).
is_contextual( cc(_,_) ).

%%%%%%%%%%%%%%%%%%%%
%%% ASK_SERVER/2 %%%
%%%%%%%%%%%%%%%%%%%%
ask_server(QUERY,ID) :-
    send_msg(ID,QUERY),  
    % This predefined primitive from "interface.dll" sends a query to the 
    % connected DeLP server.
    recv_msg(ID,MSG),
    % This predefined primitive from "interface.dll" receives an answer from 
    % the connected DeLP server.
    process_msg(MSG).

%%%%%%%%%%%%%%%%%%%%%
%%% PROCESS_MSG/1 %%%
%%%%%%%%%%%%%%%%%%%%%
process_msg(MSG) :-
        MSG = error(timeout),
        write('Answer message timeout (check for server\'s availability)'), nl, !
    ;
        MSG = error(syntax),
        write('There was a syntax error in the last message sent'), nl, !
    ;
        MSG = error(contradiction(L)),
        write('WARNING! the subset of strict rules of your program is contradictory'), nl,
        write('Literals "'), write(L), write('" and "'), write(~L), write('" have a strict derivation'), nl
    ;
        MSG = error(BAD_MSG),
        write('The message \''), write(BAD_MSG), write('\' has a syntax error'), nl, !
    ;
        MSG = exp(EXP_PL,EXP_XML,ANS,NC),
        ANS =.. [ANSWER,INSTANTIATED],
        dump_explanation(EXP_PL, EXP_XML),
        write('The answer is ('), write(ANSWER), write(', '), write(NC), write(')'), nl, !
    ;
        MSG = invalid(L),
        write('The answer is INVALID, the contextual sets were contradictory '), write(L), nl, !
    ;
        MSG =.. [ANSWER,INSTANTIATED,NC],
        write('The answer for '), write(INSTANTIATED), write(' is ('), write(ANSWER), write(', '), write(NC), write(')'), nl.

dump_explanation(EXP_PL,EXP_XML):-
    load_files('tree.cfg', [silent(true)]),
    tree_file(T),
    atom_concat(T, '.pl', TdotPL),
    atom_concat(T, '.xml', TdotXML),
    tell(TdotPL),
    TERM_PL = tree_info(EXP_PL),
    write(TERM_PL), write('.'), nl,
    told,
    tell(TdotXML),
    write(EXP_XML),
    told.
    
% By default, the server is run on the same machine as the top-level.
serverIP(IP):- 
    gethostname(HOST),
    tcp_host_to_address(HOST, IP_address),
    IP_address = ip(B1, B2, B3, B4),
    concat(B1, '.', B1DOT),
    concat(B2, '.', B2DOT),
    concat(B3, '.', B3DOT),
    concat(B1DOT, B2DOT, B12),
    concat(B12, B3DOT, B123),
    concat(B123, B4, IP).
    
disconnected(_). % It never fails.

