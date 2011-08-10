% This file contains options and configs. used by the delp.pl, aux_server_config, tree_xml and tree_swi

use_built_ins(yes). %%%are we letting free usage of built-ins?

%%%%%%%%%%%%%%%%%%
%%%   OUTPUT   %%%
%%%%%%%%%%%%%%%%%% 
%use yes/no for different output configurations

%use_blockblock(no).	%now in 'delp.cfg'
tofile(yes).		% "yes" produces a continuous output (without "pause" stops)
traceF(no).		% "yes" shows a minimal trace of the argumentation process
traceW(no).		% set to "yes" for a trace of all defeasible argumentation
traceSpecificity(no).	% specificity messages still in Spanish
trazaarg(no).		% trace for argument construction


%..............FOR DYNAMIC CONFIGURATION PURPOSES.................

:- dynamic use_blockblock/1,comparison/1,save_tree/1,tree_file/1,xml_tree_file/1,use_answer_port/1,to_file/1,output_file/1,verify_consistency_in_Pi/1.

option_list([use_blockblock,comparison,save_tree,xml_tree_file,tree_file,use_answer_port,output_file,to_file,verify_consistency_in_Pi]).

is_a_built_in(config_off(_)).
is_a_built_in(config_on(_)).               
is_a_built_in(config_switch(_)).
is_a_built_in(comparison_off(_)).
is_a_built_in(comparison_on(_)).
is_a_built_in(set_tree_file(_)).
is_a_built_in(set_output_file(_)).
is_a_built_in(show_options).

%SHOW_OPTIONS/0 muestra por pantalla las opciones de configuracion segun
%su estado actual
show_options:-
        option_list(OpList),
        forall(
        (member(Option,OpList),functor(OptionWithArg,Option,1),OptionWithArg),
        (write(OptionWithArg),nl)
        ).

%SET_TREE_FILE/1 modifica el nombre del archivo que contiene la informacion de
%los arboles de dialectica para ser graficados



%...................................................................................


%...............DeLP INTERPRETER default CONFIGURATION FILE.........................

load_default_config:-test_and_set.

%TEST_AND_SET/0 testea y setea cada opcion de la lista de opciones
test_and_set:-
        forall(
        (option_list(OpList),member(Op,OpList)),
        test_and_set(Op)
        ).


%TEST_AND_SET/1 chequea si su argumento (una opcion) esta definido, si no lo esta
%toma la config default y la introduce en la base de datos
test_and_set(Smthg):-
        Fact =.. [Smthg,_],
        \+(Fact),
        forall(
        config(Smthg,Arg),
        (ToAssert =.. [Smthg,Arg],
        assert(ToAssert))
        ).

test_and_set(_).

%...default configuration...%
config(use_blockblock,yes).
config(comparison,defeater2assumption).
config(comparison,more_specific).
config(tree_file,'tree_info.pl').
config(use_answer_port,no).
config(to_file,yes).
config(output_file,'answer.txt').
config(verify_consistency_in_Pi,yes).


%EOF
