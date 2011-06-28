
:- initialization load_foreign_library(foreign(pl2xpce)).

:- op(190,fx,~).
:- op(191,fx,not).
:- op(1101,xfx,-<).
:- op(1101,xfx,<-).

:- discontiguous root/2,node/2.

:- dynamic root/2,node/2, tree_info/1. %sometimes there is no nodes nor tree_info...


load_tree(ID):-
	startLog,
	%%%catch(
	%%%(['delp.cfg'],tree_file(TFile),directoryOrNot(ID,SlashedID),
	%%% atom_concat(SlashedID,TFile,NewTF),log(tree_info_file_is(NewTF)),[NewTF]),
	%%% _,load_default_tree).
	['tree.cfg'],
	tree_file(TFile),
	directoryOrNot(ID,SlashedID),
	atom_concat(SlashedID,TFile,NewTF),
	log(tree_info_file_is(NewTF)),
	[NewTF].


load_default_tree:- catch(([tree_info],log('DEFAULT tree info has been loaded!!!')),_,true).


toStr(Str,ParsedList):-
        is_list(Str),!,
        parseList(Str,ParsedList).

toStr(Str,Str):-
        atom(Str),!.

toStr(Str,S):-
        term_to_atom(Str,S).


parseList([E],A):-term_to_atom(E,A),!.

parseList([E1|Es],Str):-
        parseList(Es,E2),
        term_to_atom(E1,E),
        string_concat(E, ',\n', S1),
        string_concat(S1, E2, Str).


:- dynamic treeCounter/1.

treeCounter(1).

getTC(TC):-
        treeCounter(TC),
        TCplus is TC + 1,
        retract(treeCounter(_)),
        assert(treeCounter(TCplus)).


getHead(Root,Head):-
        Root = [(Head-<_)|_],!
        ;
        Root = [~(NotHead-<_)|_],Head = ~NotHead,!
        ;
        Root = [(Head<-_)|_],!
        ;
        Root = [~(NotHead<-_)|_],Head = ~NotHead,!
        ;
        Root = [Head]
        ;
        Root = [~NotHead],Head = ~NotHead.


argFormat(Str,Conclusion,ArgStr):-
        string_concat('<{',Str,HalfStr),
        string_concat(HalfStr,'},\n',OtherHalfStr),
        string_concat(OtherHalfStr,Conclusion,AlmostThere),
        string_concat(AlmostThere,'>',ArgStr).


replace(_,_,[],[]):-!.

replace(E,R,[E|Rest],[R|Restplace]):-replace(E,R,Rest,Restplace),!.

replace(E,R,[Other|Rest],[Other|Restplace]):-replace(E,R,Rest,Restplace).


rep(_,_,_,[],[]):-!.

rep(I,I,E,[_|Rest],[E|Rest]):-!.

rep(CurrentI,I,Erep,[E|Rest],[E|RestRep]):-
        Iplus is CurrentI + 1,
        rep(Iplus,I,Erep,Rest,RestRep).


:- dynamic treelog/0.

treeLogFile('treelog.txt').

startLog:- %%%!!!!!ERA TREELOG
	log,!,
	treeLogFile(TLF),open(TLF,append,LF),
	write(LF,'\n\n/*/......DeLP Tree Drawer log file....../*/\n'),
	get_time(T),convert_time(T,S),
	write(LF,'/*/...............'),write(LF,S),write(LF,'.............../*/\n\n'),close(LF).
startLog.


log(Text):- %%%!!!!!ERA TREELOG
        log,!,
	treeLogFile(TLF),open(TLF,append,LF),
        write(LF,'...'),write(LF,Text),write(LF,'...'),write(LF,'done\n'),
        close(LF).

log(_).


:-dynamic last/1.

showTitle(_,_):-notitle,!.

showTitle(P,Arg):-
        /*string_to_list(Arg,L),replace(10,32,L,L2),

        %%%forall(nth0(I,L2,32),(retractall(last(_)),assert(last(I)))),
        %%%last(Index),

	nth0(Index,L2,32),

        rep(0,Index,10,L2,Lreplaced),

        string_to_list(OneLine,Lreplaced),

        string_concat('Dialectical tree for argument\n',OneLine,Title),*/

	string_concat('Dialectical tree for argument\n',Arg,Title),
        new(TxtTitle,text(Title,center,bold)),
        send(TxtTitle,colour,blue),
		send(TxtTitle, center, P?visible?center),
		get(TxtTitle, position, point(X1,_)),
		getTitleY(Y1), %0
		send(TxtTitle,position(point(X1,Y1))),
		send(TxtTitle,pen,1),

		send(P, display, TxtTitle).


showLegend(_):-nolegend,!.

showLegend(P):-
		new(TxtLegend,text('Black nodes are undefeated arguments\nRed nodes are defeated arguments',center,bold)),
		send(TxtLegend,colour,blue),
		send(TxtLegend,center,P?visible?center),
		get(TxtLegend, position, point(X2,_)),
		getLegendY(Y),
		send(TxtLegend, position, point(X2,Y)),

		send(TxtLegend,pen,1),
		send(P, display, TxtLegend).





/*xxshowLegend(P):-

        new(TxtUndef,text('Black nodes are undefeated arguments',center,bold)),
        send(TxtUndef,colour,black),
		send(TxtUndef, center, P?visible?center),
		get(TxtUndef, position, point(X2,_)),
		getTxtUndefY(Y2), %25
		send(TxtUndef,position(point(X2,Y2))),

        new(TxtDef,text('Red nodes are defeated arguments',center,bold)),
        send(TxtDef,colour,red),
		send(TxtDef, center, P?visible?center),
		get(TxtDef, position, point(X3,_)),
		getTxtDefY(Y3), %45
		send(TxtDef,position(point(X3,Y3))),

		send(TxtUndef,pen,1),

        new(Box,box(256,45)),
		send(Box,center,P?visible?center),
		get(Box, position, point(X4,_)),
		getBoxY(Y4), %20
		send(Box,position(point(X4,Y4))),
        send(Box,position(point(0,20))),

        send(P, display, Box),
        send(P, display, TxtUndef),
        send(P, display, TxtDef).*/


getTitleY(0).
getLegendY(Y) :- getTitleY(Yt), Y is Yt+50.
%%getTxtUndefY(Y) :- getTitleY(Yt), Y is Yt+50.
%%getTxtDefY(Y) :- getTxtUndefY(Yu), Y is Yu+20.
%%getBoxY(Y) :- getTxtUndefY(Yu), Y is Yu-5.


legendOrNotY(60):-nolegend,!. %%%!!!!!CHECK THE CAP
legendOrNotY(120).


chkDir(Dir):-exists_directory(Dir),!.
chkDir(Dir):-make_directory(Dir).


directoryOrNot('',''):-!.
directoryOrNot(ID,SlashedID):-chkDir(ID),string_concat(ID,/,SlashedID).


tree(ID):-
        forall(
                (root(Root,NodeID),log(new_root_is(Root))),

        (toStr(Root,RootStr),
        getHead(Root,Head),
        term_to_atom(Head,AtomicHead),

        string_concat('DeLP / dialectical tree / ',AtomicHead,Title),

        argFormat(RootStr,AtomicHead,ArgRootStr),

        new(P, picture(Title)),
        new(Tree, tree(new(RootNode, node(text(ArgRootStr,center,font(screen,bold,12)))))),
        send(Tree,direction(vertical)),

        makeTree(Root,NodeID,RootNode,_),


        showTitle(P,ArgRootStr),
		showLegend(P),

        send(Tree, center, P?visible?center),
		get(Tree, position, point(X,_)),
		legendOrNotY(Y),
		send(Tree,position(point(X,Y))),
		send(P, display, Tree),
        send(P, open),

        getTC(TreeCounter),
		tree_file(TreeInfo),
		%%%string_length(TreeInfo,L),
		%%%Lminus3 is L - 3,
		%%%sub_string(TreeInfo, 0, Lminus3, _, TreeInfoWithoutExt),

        directoryOrNot(ID,NewID),
		%%%string_concat(NewID,TreeInfoWithoutExt,TreeFileID),
		string_concat(NewID,TreeInfo,TreeFileID),
        string_concat(TreeFileID,TreeCounter,PreFilename),
        string_concat(PreFilename,'.eps',Filename),log(tree_file_is(Filename)),

        new(File, file(Filename)),

        send(File, open, write),
        send(File, append, P?postscript),
        send(File, close),
        send(File, done),

        retractall(alreadyDrawn(_))
        )),log('Tree drawn successfully').

tree(_):-log('Something wrong happened').


:- dynamic alreadyDrawn/1, noLegend/0.


makeTree(SubRoot,SubRootID,Node,undefeated):-
        forall(
                (node((SubRoot,SubRootID),(Son,SonID)),log(new_node_is(Son))),

                (\+ alreadyDrawn(node((SubRoot,SubRootID),(Son,SonID))),
                assert(alreadyDrawn(node((SubRoot,SubRootID),(Son,SonID)))),

                toStr(Son,Str),
                getHead(Son,SonHead),
                term_to_atom(SonHead,AtomicSonHead),
                argFormat(Str,AtomicSonHead,ArgStr),

                send(Node, son, new(SonNode, node(text(ArgStr, center, font(screen,bold,12))))),
                makeTree(Son,SonID,SonNode,Mark),Mark = defeated)
                ),!.

makeTree(_,_,Node,defeated):-send(Node,colour,red).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN %%% MAIN %%% MAIN %%% MAIN %%% MAIN %%% MAIN %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reservedParameters(['-show','-nolegend','-log','-notitle']).


formatArg(P,FormattedP):-string_to_list(P,[_|L]),string_to_list(S,L),atom_to_term(S,FormattedP,_).


:- reservedParameters(Params),forall(member(P,Params),(formatArg(P,FP),dynamic(FP))). %sets every parameter as dynamic


captureArgs(Args):-
	reservedParameters(Params),
	forall(
	member(A,Args),
	(
		(member(A,Params),!,
		formatArg(A,FormattedA),
		assert(FormattedA)
	;
		retractall(id(_)),asserta(id(A)))
	)
	),!.

captureArgs(Args):-log,log('BAD input parameters'),log(parameters(Args)),fail.

captureArgs(_):-write('BAD input parameters'),nl.


:-dynamic id/1.
id(''). %default 'void' ID


tree:-
	id(ID),
	load_tree(ID),
	assertRootAndNodes,
	tree(ID),
	retractall(node(_,_)),
	retractall(root(_,_)).


assertRootAndNodes:-
	tree_info(NODES_LIST),!,
%%%write(NODES_LIST),nl,get_char(_),
	forall(member(NODE,NODES_LIST),assert(NODE)).

assertRootAndNodes. %if there are no trees to draw


stopOrNot:-
	show,!,
	get_char(_).

stopOrNot.


xmain :- catch(main,_,(tell('tree_error.txt'),write('ERROR!'),nl,told)).


main :- current_prolog_flag(argv,[_|Args]),captureArgs(Args),tree,noCerrar,stopOrNot,halt.

notitle.

noCerrar:-
	repeat,
	sleep(10),
	get(X),
	X = s.

%%% EOF %%%


%%%DEPRECATED%%%

oldmain :- current_prolog_flag(argv,[_]),load_tree(''),tree(''),halt.

oldmain :- current_prolog_flag(argv,[_,'-show']),load_tree(''),tree(''),get_char(_),halt.

oldmain :- current_prolog_flag(argv,[_,'-nolegend']),load_tree(''),assert(noLegend),tree(''),halt.

oldmain :- current_prolog_flag(argv,[_,'-log']),assert(treelog),load_tree(''),tree(''),halt.

oldmain :- current_prolog_flag(argv,[_,ID]),load_tree(ID),tree(ID),halt.

oldmain :- current_prolog_flag(argv,[_,'-show','-nolegend']),load_tree(''),assert(noLegend),tree(''),get_char(_),halt.

oldmain :- current_prolog_flag(argv,[_,'-show',ID]),load_tree(ID),tree(ID),get_char(_),halt.

oldmain :- current_prolog_flag(argv,[_,'-log',ID]),assert(treelog),load_tree(ID),catch(tree(ID),_,log('ERROR')),halt.

oldmain :- current_prolog_flag(argv,[_,'-show','-nolegend',ID]),load_tree(ID),assert(noLegend),tree(ID),get_char(_),halt.
