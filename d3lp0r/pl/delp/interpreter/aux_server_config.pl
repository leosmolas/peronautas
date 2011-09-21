% This file contains the auxiliary predicates used to configure the behavior of 
% the DeLP Server
% It uses intern_config for: dynamic comparison/1
% It is used by delp_server.pl

% config_off/1 deshabilita la opcion cuyo functor esta instanciado en el 
% parametro.
config_off(Func) :-
    set_config(Func, no).

% config_on/1 habilita la opcion cuyo functor esta instanciado en el parametro.
config_on(Func) :-
    set_config(Func,yes).
               
% comparison_off/1 quita de la base de datos el criterio de comparacion 
% instanciado en el parametro.
comparison_off(Func) :-
    retract(comparison(Func)).
               
% comparison_on/1 agrega a la base de datos el criterio de comparacion 
% instanciado en el parametro.
comparison_on(Func) :-
    assert(comparison(Func)).
              
% config_switch/1 invierte el estado de la opcion cuyo functor esta 
% instanciado en el parametro.
config_switch(Func) :-
    Cfg_pred =.. [Func, Arg],
    call(Cfg_pred),
    switch_arg(Arg, SwitchedArg),
    set_config(Func, SwitchedArg).

switch_arg(yes, no).
switch_arg(no, yes).

% set_comparison_order/1 recibe una lista de functores que representan 
% criterios de comparacion de argumentos y determina el orden de evaluacion de 
% los mismos de acuerdo al presentado en dicha lista
set_comparison_order(CriteriaList) :-
    retractall(comparison(_)),
    forall( member(Criterion, CriteriaList), assertz(comparison(Criterion))).

% SET_OUTPUT_FILE/1 modifica el nombre del archivo de salida que contiene la
% respuesta ultima consulta
set_output_file(File) :-
    set_config(output_file, File).
        
set_tree_file(File) :-
    set_config(tree_file, File).

set_xml_tree_file(File) :-
    set_config(xml_tree_file, File).
        
% set_config/2 retira de la base de datos el predicado cuyo functor es Pred y 
% agrega un predicado con igual functor y con el argumento Arg.
set_config(Pred,Arg) :- 
    % Si el predicado Pred no existe en la bd con el argumento Arg.
    functor(PredToRetract, Pred, 1),
    retractall(PredToRetract),
    Pred =.. [Func | _],
    PredWithArg =.. [Func, Arg],
    assert(PredWithArg).

