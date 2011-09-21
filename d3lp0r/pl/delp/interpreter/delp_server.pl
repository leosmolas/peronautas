:-op(1101, xfx, -<).    % Defeasible Rules
:-op(1101, xfx, <-).    % Strict Rules
:-op(190,  fx,  ~).     % Strong negation
:-op(191,  fx,  not).   % Default negation

% This file contains the predicates to admin the server. It manages the 
% incoming queries and outgoing answers.
%
% This file uses predicates from:
% - delp.pl = to answer queries and check if a given (added) program is 
% contradictory.
% - tree_swi = to build the dialectical trees (with the SWI graphics) used to 
% get the answer of the incoming query.
% - tree_xml = to build the dialectical trees (with the VyGLab graphics) used 
% to get the answer of the incoming query.
% - aux_date = to check the expiration date
% - aux_server_config = to set and dynamically change the options of the 
% delp/server.
% - interface = to get the primitives to admin client connections and messages.
%
% This file is used by top_server_l.pl and delp.pl.
%
% Last adds (by Gotti)
% - Invalid Query detection (test_context_invalidity(+L,-R) in process())
% - Responses with the list of literals removed (Two new parameters in process and process context)
% - Hook para que no muestre el cartelito cuando se cae un cliente

%%%%%%%%%%%%%%%%%%%%%%%
%%% DATE MANAGEMENT %%%
%%%%%%%%%%%%%%%%%%%%%%%
:- ['aux_date'].

%%%%%%%%%%%%%%%%%%%%%%%
%%%% SERVER CONFIG %%%%
%%%%%%%%%%%%%%%%%%%%%%%
:- ['aux_server_config'].

%%%%%%%%%%%%%%%%%%%%%%%
%%%% INTERN CONFIG %%%%
%%%%%%%%%%%%%%%%%%%%%%%
:- ['intern_config'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIMITIVES LIBRARY LOAD  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- initialization load_foreign_library(interface).
:- ['interface']. % La version nueva de las primitvas del tucky!

% Para que no muestre mensajes de I/O error. En particular, cuando se cae un cliente.
:- dynamic message_hook/3.
message_hook(error(io_error(read, _), _), error,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DYNAMIC FACTS & DEFAULTS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic port/1, prog/1, verbose/1, server_name/1,  agent/2.
:- dynamic context_add/1, context_ignore/1.
:- dynamic distribution/1. % Asserted while compiling...

port(8000).            % default port
prog(no_prog).         % default program
verbose(yes).          % default verbose mode
connectionsNumber(64). % default number of connections sent to start/4
messagesNumber(16).    % default number of messages sent to start/4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SERVER CONFIGURATION %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config_server :-
    % Default Configuration (predicates from aux_server_config.pl).
    config_on(save_tree),
    set_tree_file('explanation.pl'),
    set_xml_tree_file('explanation.xml'),
    config_off(use_answer_port),
    config_off(to_file),
    config_on(use_blockblock),
    comparison_on(defeater2assumption),
    comparison_on(more_specific),
    config_on(verify_consistency_in_Pi),
    %%% Load Outside configuration
    load_files(['delp.cfg'], [silent(true)]).

%:- write(hola).
%:- config_server. %the server is configured automatically when this file is consulted
:- at_halt(quit).

%%%%%%%%%%%%
%%% MAIN %%%
%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% DeLP SERVER PARAMETERS:                                                  %%%
%%% ---- ------ ----------                                                   %%%
%%%                                                                          %%%
%%% -v               verbose mode on                                         %%%
%%% -voff            verbose mode off                                        %%%
%%% -prog <program>  <program>.delp preload                                  %%%
%%% -port <port>     connection port <port>                                  %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_app(_OPTIONS) :-
    % valid_options(OPTIONS), %checks if the given options are valid
    % |capture_options(OPTIONS), %processes the options
    port(PORT),
    connectionsNumber(CONN),
    messagesNumber(MSGS),
    config_server,
    delp_server(PORT,CONN,MSGS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SERVER LOOP & PROCESSING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delp_server(Port,_NumConn,_NumMsgs) :-
    %start(Port,NumConn,NumMsgs), %%% start/4 returns the local IP
    start_server(Port),
    %assert(serverIP(aca_iria_el_ip)),
    serverIP(IP),
    atom_concat('DeLP dedicated server @ ',IP,TITLE1),
    atom_concat(TITLE1,':',TITLE2),
    atom_concat(TITLE2,Port,TITLE),
    (
        current_prolog_flag(windows, true) -> window_title(_,TITLE)
    ;
        write(TITLE),nl
    ),
    server_loop.

server_loop :-
    banner,
    repeat,
    %%%catch(receive(ID,MSG),_,(write('An exception was triggered'),nl)),
    recv_msg(ID,MSG),
    echo(msg(ID,MSG)),
    %thread_create(process(ID,MSG),_,[]),
    process(ID,MSG),
    MSG = stop,halt.

process(  _, _)            :- obsolete_version, halt.
process(  _, stop).
process( ID, syntax_error) :- echo(error(ID,syntax)),send_msg(ID,error(syntax)),!.
process( ID, reload)       :- echo(reload(ID)),prog(PROGRAM),maybe_load_file(PROGRAM),!.
process( ID, name(NAME))   :- retractall(agent(ID,_)),assert(agent(ID,NAME)),!.
process( ID, prog(PROG))   :- echo(reload(ID)), loadProgram(PROG),send_msg(ID,aknowledge),!. %% Added by Gotti for update reasons
process(_ID, load(Prog))   :- write('Loading Program:'),write(Prog), nl, tell('intern.delp'),write(Prog),told,readProgram(intern).
process( ID, listing)      :- % This is used to load a program sent as a string
    findall((H<-B),(H<-B),S),
    findall((H-<B),(H-<B),D),
    subtract(S,[paraquenofallesinohayninguna<-true],SS),
    subtract(D,[paraquenofallesinohayninguna-<true],DD),
    append(SS,DD,SandD),
    send_msg(ID,SandD),
    !.

:- op(192,fx,'?'). %for explanations

%process(ID,[CX,ComparisonCriterion,QUERY]) :-
    %useContextCriterion(ComparisonCriterion),
    %process(ID,[CX,QUERY]).
    %removeContextCriterion(ComparisonCriterion).

process(ID,[CX,QUERY]) :- %to support combined queries
    is_list(CX),
    test_context_invalidity(CX,[]),!,
    findall('<-'(F,true), '<-'(F,true), PROG_FACTS1),
    findall('-<'(H,B), '-<'(H,B), PROG_RULES),
    subtract(PROG_FACTS1,['<-'(paraquenofallesinohayninguna,true)],PROG_FACTS),
    %% PROG_FACTS a list containing all the facts of the DeLP program loaded in the server
    append(PROG_FACTS,PROG_RULES,PROG),
    process_context(PROG,CX,[],[],Fin,Fout,NCOut), %%%VER delay EXCESIVO
    list_to_set(Fin,FSetin),
    list_to_set(Fout,FSetout),
    assert(context_add(FSetin)),
    assert(context_ignore(FSetout)),
    nl,write(context_add(FSetin)),nl,
    write(context_ignore(FSetout)),nl,nl,
    (
        QUERY = ?Q,!, %%% asking for explanation
        openTree, xml_openTree, exp(Q,ANS), dumpTree, xml_dumpTree,
        explanation(EXP_PL),
        %ANS = [A,NCOut],
        xml_explanation(_EXP_XML),
        %send_msg(ID,exp(EXP_PL,EXP_XML,ANS,NCOut)), %Removed by XML lenght
        send_msg(ID,exp(EXP_PL,'<forest>  <graph>  </graph></forest>',ANS,NCOut)),
        echo(ans(ANS,Q,ID))
    ; %%% not asking for explanation
        asserta((toRoot(_):-!)), asserta((toTree(_,_,_,_):-!)),
        asserta((xml_toRoot(_):-!)), asserta((xml_toTree(_,_,_,_):-!)),
        answer(QUERY,ANS),
        %ANS = [A,NCOut],
        retract((toRoot(_):-!)), retract((toTree(_,_,_,_):-!)),
        retract((xml_toRoot(_):-!)), retract((xml_toTree(_,_,_,_):-!)),
        FUNC_ANS =.. [ANS,QUERY,NCOut],
        send_msg(ID,FUNC_ANS),
        echo(ans(ANS,QUERY,ID))
    ),
    retract(context_add(FSetin)),
    retract(context_ignore(FSetout)).

process(ID,[CX,_]):- %to detect an Invalid query, ie with two complementary lits
    is_list(CX),!,
    test_context_invalidity(CX,R),R \= [],!,
    send_msg(ID,invalid(R)),
    echo(invalid(ID,R)).

process(ID,[CX,QUERY]):- %to support non-combined queries, ie, of the form `TYPE(C1,...,CN)'
    CX =.. [TYPE|_],
    member(TYPE,[+,-,*]),!,
    process(ID,[[CX],QUERY]).

process(ID,QUERY):- %to support queries with empty context, ie, of the form `L' or `?L'
    (atom(QUERY),!;compound(QUERY)),!,
    process(ID,[[],QUERY]).

process(ID,BAD_MSG):- %Error in the C. Query
    send_msg(ID,error(BAD_MSG)),
    echo(error(ID,BAD_MSG)).

process_context(_,[],_,_,[],[],[]):-!.

process_context(PROG_FACTS,['-<'(H,B)|More],PrevFin,PrevFout,['-<'(H,B)|Fin],Fout,NCOut):-
%%%write(adding_rule('-<'(H,B))),nl,
    !,process_context(PROG_FACTS,More,PrevFin,PrevFout,Fin,Fout,NCOut).

process_context(PROG_FACTS,[Set|More],PrevFin,PrevFout,Fin,Fout,NCOut):-
    Set =.. [TYPE|CONTEXT],
    append(PROG_FACTS,PrevFin,PFtemp),
    subtract(PFtemp,PrevFout,CURRENT_FACTS),
    revise_program(TYPE,CURRENT_FACTS,CONTEXT,CurrentFin,CurrentFout,NC),
    %nl,write('From: '),write(Set),write(' Ignore: '),write(NC),nl,
    %append(NCprev,NC,NCTemp),
    %union(NCprev,NC,NCTemp),

    %write(current_facts(CURRENT_FACTS)),nl,
    %write(current_fin(CurrentFin)),nl,
    %write(current_fout(CurrentFout)),nl,

    process_context(CURRENT_FACTS,More,CurrentFin,CurrentFout,NextFin,NextFout,NCTemp),
    remove_arrows(NC,NCwithoutArrows),
    append([NCwithoutArrows],NCTemp,NCOut),
    %nl,write('Saco esto al final: '),write(NCOut),nl,
    %union(NCTemp,NCTemp2,NCOut),
    append(CurrentFin,NextFin,Fin_temp),
    subtract(Fin_temp,NextFout,Fin),
    append(CurrentFout,NextFout,Fout_temp),
    subtract(Fout_temp,NextFin,Fout).

%% PROTOTIPO: detecta solo el primer literal (hecho) en contradiccion
revise_program(+,CurrF,CX,UNREPEATED_CX,Fout,Fout):- %prioriza el conocimiento enviado por el agente (Fin = V)
    subtract(CX,CurrF,UNREPEATED_CX), %Descarta los elem. repetidos entre prog y CX
    fact_complements(UNREPEATED_CX,CXcomps),
    intersection(CXcomps,CurrF,Fout).

revise_program(*,CurrF,CX,Fin,[],NC):- %prioriza el conocimiento alojado en el server
    subtract(CX,CurrF,UNREPEATED_CX), %Descarta los elem. repetidos entre prog y CX
    fact_complements(CurrF,CFcomps),
    subtract(UNREPEATED_CX,CFcomps,Fin),
    intersection(UNREPEATED_CX,CFcomps,NC).

revise_program(-,CurrF,CX,[],Fout,Fout):- %ignora los literales enviados en el contexto
    intersection(CurrF,CX,Fout).

%fact_complements(+Facts,-FactsComps) this predicate creates a list FactComps that contains the complements of the literals in Facts
fact_complements([],[]):-!.
fact_complements([F|Fs],[FC|FCs]):-
    fact_complement(F,FC),!,
    fact_complements(Fs,FCs).
fact_complements([F|Fs],[F,FCs]):-
    fact_complements(Fs,FCs).

%Given a literal this returns its complement
fact_complement(X <- true, CX <- true):- complement(X,CX).


%This predicate search contradictions in every set of a given context
test_context_invalidity([],[]).

test_context_invalidity([X|Rest],Lits):-
    test_set_invalidity(X,L),
    test_context_invalidity(Rest,R),
    append(L,R,Lits).

%This predicate ditermines in whose context set look for contradictions
test_set_invalidity(S,Lits):-
    S =.. [+|CONTEXT],!,isInval(CONTEXT,Lits).

test_set_invalidity(S,Lits):-
    S =.. [*|CONTEXT],!,isInval(CONTEXT,Lits).

test_set_invalidity(_,[]).

%This predicates returns a list of fact literals which are in contradiction, given a list of rules
isInval(L,R):- findall(X,(member('<-'(X,true),L),member('<-'(Y,true),L),complement(X,Y)),R).

remove_arrows(L,LwithoutArrows):-findall(X,(member('<-'(X,true),L)),LwithoutArrows).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST OF RULES CHECK %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_list_of_rules(L):-
    is_list(L),
    forall(member(Rule,L),is_rule(Rule)).

is_rule(_ <- _).
is_rule(_ -< _).


%%%%%%%%%%%%%%%%%
%%% SERVER IP %%%
%%%%%%%%%%%%%%%%%

serverIP(IP):- %by default, the server is ran on the same machine as the top-level
    gethostname(HOST),
    tcp_host_to_address(HOST,IP_address),
    IP_address = ip(B1,B2,B3,B4),
    concat(B1,'.',B1DOT),
    concat(B2,'.',B2DOT),
    concat(B3,'.',B3DOT),
    concat(B1DOT,B2DOT,B12),
    concat(B12,B3DOT,B123),
    concat(B123,B4,IP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HANDLER OF THE (DIS)CONNECTION EVENT %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connected(ID):-
    (verbose(yes);verbose(default)),
    get_agent_name(ID,Name),
    write('[Agent '),write(Name),write('] connected'),nl,nl,
    check_date,!.
connected(_). %it never fails

disconnected(ID):-
    (verbose(yes);verbose(default)),
    get_agent_name(ID,Name),retractall(agent(ID,Name)),
    write('[Agent '),write(Name),write('] disconnected'),nl,nl,
    check_date,!.
disconnected(_). %it never fails

%%%%%%%%%%%%%%
%%% BANNER %%%
%%%%%%%%%%%%%%

banner :-
    get_distro_name(DISTRO_SENTENCE),
    atom_concat('--  ',DISTRO_SENTENCE,TABBED_DISTRO_SENTENCE),
    atom_concat(TABBED_DISTRO_SENTENCE,'  --',FORMATTED_DISTRO_SENTENCE),

    nl,
    write('           ------------------------------------------'),nl,
    write('           --    DeLP Dedicated Server v0.4.0      --'),nl,
    write('           --        - research version -          --'),nl,
    write('           --                                      --'),nl,
    write('           --        Personal Naeem Khalid         --'),nl,
    write('           ------------------------------------------'),nl,nl,
    write(FORMATTED_DISTRO_SENTENCE),nl,nl,

    write('(c) 2006 Artificial Intelligence Research and Development Laboratory (LIDIA)'),nl,
    write('Universidad Nacional del Sur. Bahia Blanca. Argentina. e-mail: delp@cs.uns.edu.ar'),nl,
    write('Alejandro J. Garcia - Nicolas D. Rotstein - Sebastian Gottifredi - Mariano Tucat - Guillermo R. Simari'),nl,
    write('(developed with SWI-Prolog v5.6.11)'),nl,nl,

    check_date,
    echo(time),

    serverIP(IP),
    write('Running at '),write(IP),nl,
    echo(port),
    write('Verbose mode: '),echo(verbose),
    %...IN ORDER TO LOAD A FILE WITHOUT THROWING AN EXCEPTION IF IT DOESN'T EXIST...
    %LOADFILE/1 carga un archivo; en caso de que se produzca una excepcion, escribe
    %un mensaje y falla
    write('DeLP program loaded: '),echo(program),nl,
    prog(PROG),
    maybe_load_file(PROG),

    nl,write('Server started... OK'),nl,nl.


get_distro_name(SENTENCE):-
    distribution(NAME),
    atom_concat('Personal copy for ',NAME,SENTENCE),!.

get_distro_name('DeLP team').

get_agent_name(ID,NAME):-
    agent(ID,NAME),!.
get_agent_name(ID,ID).


%%%%%%%%%%%%%%
%%% ECHOES %%%
%%%%%%%%%%%%%%

echo(reload(ID)):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] has reloaded the program'),nl,nl.

echo(msg(ID,name(NEW_NAME))):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] has changed name to '),write(NEW_NAME),nl,nl.

echo(msg(ID,(KB,QUERY))):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] has performed query ('),write((KB,QUERY)),write(')'),nl.

echo(ans(A,Q,ID)):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] '),
    write('answer for query '),write(Q),write(' is '),write(A),nl,nl.
%   write('[Agent '),write(NAME),write('] '),
%   write('Sending '),write(ANS),write(' message to
%   '),write(NAME),nl,nl.

echo(error(ID,syntax)):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] '),
    write('Message syntax error'),nl,nl.


echo(error(ID,BAD_MSG)):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] '),
    write('Message \''),write(BAD_MSG),write('\' has a syntax error'),nl,nl.

echo(invalid(ID,R)):-
    verbose(yes),!,
    get_agent_name(ID,NAME),
    write('[Agent '),write(NAME),write('] '),
    write('Message has contradictions '),write(R),nl,nl.


echo(time):-
    get_time(TIME),
    convert_time(TIME,NICE_TIME),
    write('Server started at '),write(NICE_TIME),nl,!.

echo(verbose):-
    verbose(yes),!,
    write('FULL'),nl.
echo(verbose):-
    verbose(no),!,
    write('OFF'),nl.
echo(verbose):-
    verbose(default),!,
    write('DEFAULT'),nl.

echo(program):-
    prog(no_prog),!,
    write('NONE'),nl.
echo(program):-
    prog(PROG),!,
    atom_concat(PROG,'.delp',DeLP_PROG),
    write(DeLP_PROG),nl.

echo(port):-
    port(PORT),!,
    write('Connection port: '),write(PORT),nl.

echo(_). %an echo never fails


%%%%%%%%%%%%%%%%%%%%
%%% FILE LOADING %%%
%%%%%%%%%%%%%%%%%%%%

maybe_load_file(no_prog):-!.
%
%The follwing line has been commented by gotti: only one of the two below rule
%can be active

%maybe_load_file(FILE):- load_file_silent(FILE),halt_if_contradictory.

maybe_load_file(FILE):- load_file(FILE),halt_if_contradictory.


halt_if_contradictory:-
    contradictoryProgram(_),
    nl,write('Press a key and the server will be shutdown immediately...'),nl,nl,
    get_char(_),
    halt.
halt_if_contradictory.


%...IN ORDER TO LOAD A FILE WITHOUT THROWING AN EXCEPTION IF IT DOESN'T EXIST...
%LOADFILE/1 carga un archivo; en caso de que se produzca una excepcion, escribe
%un mensaje y falla


load_file(File):-
          string_concat(File,'.delp',DeLPFile),
          catch([DeLPFile],_,no_file_msg(DeLPFile)),
          %catch([DeLPFile],_,X),
    %write(X),
          verify_Pi.

load_file_silent(FILE):-
          string_concat(FILE,'.delp',DeLPFile),
          catch(load_files(DeLPFile,[silent(true)]),_,no_file_msg(dont_fail,DeLPFile)),

          verify_Pi.


no_file_msg(dont_fail,File):-
            write('The file '),write(File),write(' does not exist'),nl.

no_file_msg(File):-
            write('The file '),write(File),write(' does not exist'),nl,fail.


load_file(File,dontFail):-
          catch(ensure_loaded(File),_,true).

%% This prediactes are used to check consistency in the files (Only)

verify_Pi:- verify_consistency_in_Pi(yes),check_SSet.
verify_Pi:- verify_consistency_in_Pi(no).


valid_options(OPT):-
    not((member('-v',OPT),member('-voff',OPT))), %it cannot be the case that verbose mode is on and off

    valid(port,OPT),

    valid(hide,OPT),

    valid(prog,OPT).

valid(prog,OPT):-
    member('-prog',OPT),
    followed_by(OPT,'-prog',PROG),
    %%%atom_to_term(PROG,PROG_TERM,_),atom(PROG_TERM),retract(prog(_)),assert(prog(PROG_TERM)).
    retract(prog(_)),assert(prog(PROG)).
valid(prog,OPT):-
    not(member('-prog',OPT)).

valid(port,OPT):-
    member('-port',OPT),
    followed_by(OPT,'-port',PORT),
    atom_to_term(PORT,PORT_TERM,_),number(PORT_TERM),retract(port(_)),assert(port(PORT_TERM)).
valid(port,OPT):-
    not(member('-port',OPT)).

valid(hide,OPT):-
    member('-hide',OPT),
    win_window_pos([show(false)]).

valid(hide,OPT):-
       not(member('-port',OPT)).



followed_by(List,X,Y):- %if X is in List at pos I, and Y is in List at pos I+1
    nth0(I,List,X),
    Iplus is I+1,
    nth0(Iplus,List,Y).

%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIONS CAPTURE %%%
%%%%%%%%%%%%%%%%%%%%%%%

capture_options(OPT):-
    member('-v',OPT),retract(verbose(_)),assert(verbose(yes)),!
    ;
    member('-voff',OPT),retract(verbose(_)),assert(verbose(no)),!
    ;
    true.

%%% <EOF>

