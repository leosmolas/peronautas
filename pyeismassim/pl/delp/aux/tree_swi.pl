%
% This file contains the predicates to build the xml tree representation used by the SWI tree representation
% Is uses the files:
%   - aux_tree_drwaing = which gives the basic predicates used for tree building
%   - intern_config = dynamic save_tree/1



%%%%%%%%%%%%%%%%%%%%
%%% DYNAMIC TREE %%%
%%%%%%%%%%%%%%%%%%%%

:- dynamic root/2, node/2.
:- dynamic toRoot/1, toTree/4.

%%%%%%%%%%%%%%%%%%%%%%%
%%%% TREE DRAWING  %%%%
%%%%%%%%%%%%%%%%%%%%%%%
:-[aux_tree_drawing].

        
%TOROOT/1 guarda la raiz de un arbol de dialectica rotulandolo con el argumento
%e identificandola con el functor 'root' si dicha opcion esta habilitada en la
%configuracion actual
toRoot(Arg):-
        save_tree(yes),
        newNodeID(Arg,NodeID),
              
        toArrow(Arg,ArrowArg),

	assertz(root(ArrowArg,NodeID)),!.

%        tree_file(TFile),

%        open(TFile,append,T),
%        write(T,root(ArrowArg,NodeID)),
%        write(T,'.'),
%        write(T,'\n'),
%        close(T).
        
toRoot(_):-save_tree(no),!.

toRoot(ARG):-write('Error writing tree info about root '),write(ARG),nl.


%TOTREE/2 guarda nodos del arbol de dialectica con el formato node(Padre,Hijo) si
%dicha opcion esta habilitada en la configuracion actual
%NOTA: los ultimos dos parametros son el subargum. de desacuerdo de Arg y el tipo
%de ataque (blocking o proper), pero son usados por la version XML del graficador
toTree(Arg,Def,_,_):-
        save_tree(yes),
        %treeCounter(TC),

        nodeID(Arg,NodeID),!, %existing node
        newNodeID(Def,NewNID), %a new one
        
        %write(old(NodeID)),nl,
        %write(newNode(Def,NewNID)),nl,nl,nl,
        
        toArrow(Arg,ArrowArg),
        toArrow(Def,ArrowDef),

	assertz(node((ArrowArg,NodeID),(ArrowDef,NewNID))),!.

%        tree_file(TFile),
%        open(TFile,append,T),
%        write(T,node((ArrowArg,NodeID),(ArrowDef,NewNID))),
%        write(T,'.'),
%        write(T,'\n'),
%        close(T).

toTree(_,_,_,_):-save_tree(no),!.

toTree(_,Def,_,_):-write('Error writing tree info about defeater '),write(Def),nl.



%OPENTREE/0 vacia el archivo que contiene la info para graficar los arboles si
%dicha opcion esta habilitada en la configuracion actual
openTree:-
          save_tree(yes),
	  retractall(root(_,_)),
	  retractall(node(_,_)).
          %tree_file(TFile),
          %open(TFile,write,T),
          %write(T,''),
          %close(T).
          
openTree:-save_tree(no).

%DUMPTREE/0 takes every fact root/2 and node/2 and writes it to a file (set by tree_file/1)
dumpTree.
/* :-
	tree_file(TFile),
        open(TFile,append,T),
	forall(root(X,Y),
	       (write(T,root(X,Y)),
		write(T,'.'),
		write(T,'\n')
	       )),
	forall(node(X,Y),
	       (write(T,node(X,Y)),
		write(T,'.'),
		write(T,'\n')
	       )),
        close(T).*/

%LOAD_CONFIG
%load_config:- ensure_loaded('delp.cfg'),load_default_config.
%load_config:- config_file(CfgFile),ensure_loaded(CfgFile),load_default_config,treeFile2pl.


%treeFile2pl:-
%	tree_file(TF),
%	concat(TF,'.pl',PL_treefile),
%	set_tree_file(PL_treefile).

%%% SWI TREEs EXPLANATIONS%%%%
explanation(EXPL):-
	findall(root(X,Y),root(X,Y),ROOTS),
	findall(node(X,Y),node(X,Y),NODES),
	append(ROOTS,NODES,EXPL).



%EOF
