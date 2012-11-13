% This file is delp.pl reformatted.
:- dynamic volatile_knowledge/2.
:- dynamic volatile_knowledge/2.
:- dynamic context_add/1, context_ignore/1.
:- dynamic <- /2, -< /2.
:- dynamic lastRule/1.
:- dynamic contradictoryProgram/1.
:- dynamic contradictorySet/1.
:- multifile '<-' /2, '-<'/2, is_a_built_in/1.
:- op(1101, xfx, -<).
:- op(1101, xfx, <-).
:- op(190,  fx,  ~).
:- op(191,  fx,  not).
:- discontiguous '<-'/2,' -<'/2, is_a_built_in/1.

:- [tree_swi].
:- [tree_xml].
:- [aux_predicates].
:- [aux_built_ins].
:- [aux_date].
:- [aux_server_config].
:- [intern_config].
:- config_on(save_tree).

context_add([]).
context_ignore([]).
look_at_server(P) :- fail, on_server(P).
let_prolog_prove(_) :- use_built_ins(no), !, fail.
let_prolog_prove(listing) :- use_built_ins(yes), !, fail.
let_prolog_prove(H) :- use_built_ins(yes), is_a_built_in(H), catch(H, _, fail).
complement(~A, A) :- A \= ~ _.
complement(A, ~A) :- A \= ~ _.
paraquenofallesinohayninguna -< true.
paraquenofallesinohayninguna <- true.
s_rule(A, B) :- context_add(CXadd), member((A <- B), CXadd).
s_rule(A, B) :- (A <- B), context_ignore(CXign), not( member(A <- B,CXign) ).
d_rule(A, B) :- context_add(CXadd), member((A -< B), CXadd).
d_rule(A, B) :- (A -< B), context_ignore(CXign), not( member(A -< B, CXign) ).
answer(Q, yes) :- warrant(Q, _), !.
answer(Q, no) :- show('There is no warrant for '), show(Q), show('. We will see the contrary ...'), shownl, outputwait, complement(Q, CQ), warrant(CQ, _), !.
answer(Q, unknown) :- \+ in_signature(Q), show('not_in_signature'), shownl, !.
answer(_, undecided).
in_signature(Q) :- s_rule(Head, _), in_literal(Q, Head), !.
in_signature(Q) :- d_rule(Head, _), in_literal(Q, Head), !.
in_signature(Q) :- s_rule(_, Body), in_body(Q, Body), !.
in_signature(Q) :- d_rule(_, Body), in_body(Q, Body), !.
in_body(Q, L) :- in_literal(Q, L), !.
in_body(Q, (L, _)) :- in_literal(Q, L), !.
in_body(Q, (_, B)) :- in_body(Q, B), !.
in_literal(Q, Q) :- !.
in_literal(Q, C) :- complement(Q, C).
exp(Q,ANS) :- all(Q, L), complement(Q, CQ), all(CQ, CL), obtain_answer(Q, L, CL, ANS).
obtain_answer(Q, L, CL, yes(L, CL)) :- ground(Q), L  \= [], CL \= [], !.
obtain_answer(_, L, _, yes(L)) :- L \= [], !.
obtain_answer(_, _, CL, no(CL)) :- CL \= [], !.
obtain_answer(Q, _, _, unknown(Q)) :- \+ in_signature(Q), !.
obtain_answer(Q, _, _, undecided(Q)).
all(Q, Set) :- findall(Q, warrant(Q, _), List), list_to_set(List, Set).
warrant(Q, A) :- shownl, show('Main query: '), show(Q), shownl, shownl, find_argument(Q, A, []), toRoot(A), xml_toRoot(A), show('Supporting Argument: '), shownl, showArg(arg(A, Q)), shownl, outputwait, \+ defeated(A, [sup(A, Q, void, void)]).
defeated(A, ArgLine) :- showOnTraceW('Line:'), nlj, mostrarLista(ArgLine), pausej, find_acceptable_defeater(A, ArgLine, D, Disag, Status, NewArgLine), Disag = arg(_, AttkP), toTree(A, D, AttkP, Status), xml_toTree(A, D, AttkP, Status), show('is defeated by: '), shownl, showArg(arg(D, _G)), shownl, outputwait, \+ defeated(D,NewArgLine).
defeated2(A, ArgLine) :- trace, acceptable_defeaters(A, ArgLine, AdList), drawDefeaters(AdList), noDefeatedDefeaters(AdList, X), X > 0.
noDefeatedDefeaters([], 0).
noDefeatedDefeaters([Z | Rest], NnDD) :- Z = ad(_A,_ArgLine, D, _Disag, _Status, NewArgLine), ( defeated2(D,NewArgLine), noDefeatedDefeaters(Rest,RestNnDD), NnDD is RestNnDD ; noDefeatedDefeaters(Rest,RestNnDD), NnDD is RestNnDD + 1).
drawDefeaters(AdList) :- forall( member(ad(A, _ArgLine, D, Disag, Status, _NewArgLine), AdList), ( Disag = arg(_, AttkP), toTree(A, D, AttkP, Status), xml_toTree(A, D, AttkP, Status))).
find_acceptable_defeater(A, ArgLine, D, Disag, Status, NewArgLine) :- find_defeater(A, arg(D, G), ArgLine, SubA, DefeaterType), acceptable(arg(D, G), SubA, DefeaterType, ArgLine, NewArgLine), ( member( sup(D, Conc, Disag, Status), NewArgLine) ; member( int(D, Conc, Disag, Status), NewArgLine)).
acceptable_defeaters(A, ArgLine, AccDefList) :- findall( def(A, arg(D, G), ArgLine, SubA, DefeaterType), find_defeater(A, arg(D, G), ArgLine, SubA, DefeaterType), DefList), select_acceptables(DefList, AccDefList).
select_acceptables([],[]) :- !.
select_acceptables([Def | Rest], [ADef, RestAccetables]) :- isAcceptable(Def, ADef), select_acceptables(Rest, RestAccetables), !.
select_acceptables([_ | Rest], RestAcceptables) :- select_acceptables(Rest, RestAcceptables).
isAcceptable(def(A, arg(D, G), ArgLine, SubA, DefeaterType), ad(A, ArgLine, D, Disag, Status, NewArgLine)) :- acceptable(arg(D, G), SubA, DefeaterType, ArgLine, NewArgLine), ( member(sup(D, Conc, Disag, Status), NewArgLine) ; member(int(D, Conc, Disag, Status), NewArgLine)).
find_defeater(A, C, ArgLine, SubA, DefeaterType) :- ( find_counterargument(A, C, SubA, ArgLine), showOnTraceW('Counter-argument: '), nlj, showArgOnTrace(C), nlj, pausej, \+ better(SubA, C), defeater_type(C, SubA, DefeaterType) ; find_attack2assumption(A, C, ArgLine), DefeaterType = proper).
defeater_type(D, A, proper) :- better(D, A), !.
defeater_type(_, _, blocking).
acceptable(arg(NewArg, G), SubA, DefeaterType, ArgLine, NewArgLine) :- \+ circular(arg(NewArg, G), ArgLine), ( ArgLine = [sup(A, Q, _, _) | _], NewArgLine = [int(NewArg,G,SubA,DefeaterType)|ArgLine], !  ; ArgLine = [int(A,Q,_,_)|_], NewArgLine = [sup(NewArg, G, SubA, DefeaterType) | ArgLine], !), \+ blockblock(NewArgLine).
blockblock(NewLine) :- use_blockblock(yes), NewLine = [Arg1, Arg2, _ | _], ( Arg1 = sup(_, _, _, blocking), Arg2 = int(_, _, _, blocking), !  ; Arg1 = int(_, _, _, blocking), Arg2 = sup(_, _, _, blocking), !), showOnTraceW('!! blocking - blocking situation'), nlj.
circular(arg(A, _), [int(B, _, _, _) | _]) :- is_contained(A, B), !, showOnTraceW('circularity '), nlj.
circular(arg(A, _), [sup(B, _, _, _) | _]) :- is_contained(A, B), !, showOnTraceW('circularity '), nlj.
circular(arg(A, Q), [_ | RestOfLine]) :- circular(arg(A, Q), RestOfLine), !.
is_contained([], _) :- !.
is_contained([Clause | Cs], Arg) :- Clause = d_rule(Q, B), member(d_rule(Q, B), Arg), is_contained(Cs, Arg).
is_contained([Clause | Cs], Arg) :- Clause \= d_rule(_, _), is_contained(Cs, Arg).
allpoints(A, P) :- pointsOfAttack(A, Points), bodies(A, Bodies), assumptions(Bodies, Assump), append(Points, Assump, P).
defeatAPoint(P, A, C, Line) :- ( P = not L, find_argument(L, C, Line) ; P \= not _, attackAPoint(P, A, C, Line) ; P \= not _, attackAnOuterPoint(P, A, C,_, Line)).
attackAPoint(P,A,arg(CounterA,CompP),ArgLine):- complement(P, CompP), find_argument(CompP, CounterA, ArgLine), find_subargument(P, A, SubA), \+ better(arg(SubA, P),arg(CounterA, CompP)).
attackAnOuterPoint(P,A,arg(CounterA,Qc),arg(SubA,Lit),ArgLine):- complement(P, CompP), find_inverted(CompP, NewQ, RestOfBody), strict_derivation(RestOfBody, A, direct), ( find_argument(NewQ, CounterA, ArgLine), complement(NewQ, Lit), Qc = NewQ ; complement(NewQ, G), attackAnOuterPoint(G, A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine)), find_subargument(Lit,A,SubA), \+ better(arg(SubA,Lit),arg(CounterA,Qc)).
find_attack2assumption(A, arg(C, Q), ArgLine) :- bodies(A, Bodies), assumptions(Bodies, Assump), showOnTraceW(' ....... assumptions: '), showOnTraceW(Assump), nlj, attack_assumption(Assump, arg(C, Q), ArgLine).
attack_assumption([not P | _], arg(C, P), Line) :- find_argument(P, C, Line).
attack_assumption([_ | Ps], C, L) :- attack_assumption(Ps, C, L).
bodies([], []).
bodies([d_rule(_, Body) | Clauses], [Body | Bs]) :- bodies(Clauses, Bs).
bodies([s_rule(_, Body) | Clauses], [Body | Bs]) :- bodies(Clauses, Bs).
assumptions([], []).
assumptions([(A, B) | Rest], Points) :- assumptions([B | Rest], R), ( A = not _, \+ member(A, R), Points = [A | R], !  ; Points = R).
assumptions([A | Rest], Points) :- A \= (_, _), assumptions(Rest, R), ( A = not _, \+ member(A, R), Points = [A | R], !  ; Points = R).
find_counterargument(A, Counter, SubA, ArgLine) :- pointsOfAttack(A, Points), showOnTraceW(' Possible attack points: '), showOnTraceW(Points),nlj, ( attackInnerPoint(Points, A, Counter, SubA, ArgLine) ; attackOuterPoint(Points, A, Counter, SubA, ArgLine)).
pointsOfAttack([], []).
pointsOfAttack([d_rule(P, _) | Clauses], [P | Ps]) :- pointsOfAttack(Clauses, Ps).
pointsOfAttack([s_rule(_, _) | Clauses], Ps) :- pointsOfAttack(Clauses, Ps).
attackInnerPoint([P | _], A, arg(CounterA, CompP), arg(SubA, P), ArgLine) :- complement(P, CompP), find_argument(CompP, CounterA, ArgLine), find_subargument(P, A, SubA).
attackInnerPoint([_ | Ps], A, C, S, L) :- attackInnerPoint(Ps, A, C, S, L).
attackOuterPoint([P | _], A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine) :- complement(P, CompP), find_inverted(CompP, NewQ, RestOfBody), strict_derivation(RestOfBody, A, direct), ( find_argument(NewQ, CounterA, ArgLine), complement(NewQ, Lit), Qc = NewQ ; complement(NewQ, G), attackOuterPoint([G], A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine)), find_subargument(Lit, A, SubA).
attackOuterPoint([_ | Ps], A, C, S, L) :- attackOuterPoint(Ps, A, C, S, L).
find_subargument(true, _, []) :- !.
find_subargument((not _), _, []) :- !.
find_subargument((Q1, Q2), Arg, SubArg) :- !, find_subargument(Q1, Arg, S1), find_subargument(Q2, Arg, S2), append(S1, S2, SubArg).
find_subargument(Q, Arg, [Clause | Cs]) :- ( member(d_rule(Q, B), Arg), Clause = d_rule(Q, B) ; s_rule(Q, B), Clause = s_rule(Q, B) ; let_prolog_prove(Q), Clause = s_rule(Q, true) ; look_at_server(Q), Clause = s_rule(Q, true)), find_subargument(B, Arg, Cs).
find_argument(Q, A, Line) :- argument(Q, [], A_list, Line), list_to_set(A_list, A).
argument(true, D, D, _) :- !.
argument((not L), D, D, _) :- showOnTraceW(checking_assumptions(not L, D)), nlj, \+ strict_derivation(L, D, inverted), !.
argument((A,B), CurrentDerivation, ABDerivation, Line) :- !, argument(A, CurrentDerivation, ADerivation, Line), argument(B, ADerivation, ABDerivation, Line).
argument(H, CurrentDerivation, Argument, Line) :- ( s_rule(H, B), Clause = s_rule(H, B) ; d_rule(H, B), Clause = d_rule(H, B) ; let_prolog_prove(H), Clause = s_rule(H, true) ; look_at_server(H), Clause = s_rule(H, true)), argument(B, CurrentDerivation, BodyDerivation, Line), Argument = [Clause | BodyDerivation], verify_concordancy(H, Argument, Line), verify_assumptions(H, Argument).
verify_assumptions(H, Derivation) :- bodies(Derivation, Bodies), assumptions(Bodies, Assump), showOnTraceW(verifying_assumptions(H, Derivation)), nlj, not member(not H, Assump).
verify_concordancy(G,TF,Line) :- ( Line = [], All_TF = TF, !  ; Line = [sup(_, _, _, _) | _], collect_int([int(TF, G, _, _) | Line], All_TF) ; Line = [int(_, _, _, _) | _], collect_sup([sup(TF, G, _, _) | Line], All_TF)), verify_consistency(G, All_TF), !.
collect_int([], []) :- !.
collect_int([int(TF, _, _, _) | Line], All_TF) :- collect_int(Line, RestTF), append(TF, RestTF, All_TF), !.
collect_int([sup(_, _, _, _) | Line], RestTF) :- collect_int(Line,RestTF), !.
collect_sup([], []) :- !.
collect_sup([sup(TF, _, _, _) | Line], All_TF) :- collect_sup(Line, RestTF), append(TF, RestTF, All_TF), !.
collect_sup([int(_, _, _, _) | Line], RestTF) :- collect_sup(Line, RestTF), !.
verify_consistency(H, TF) :- complement(H, G), \+ strict_derivation(G, TF, inverted), !.
verify_consistency(H, _) :- showOnTraceW(H), showOnTraceW(' is contradictory !'), nlj, fail.
strict_derivation(true, _, _) :- !.
strict_derivation((A, B), TF, I) :- !, strict_derivation(A, TF, I), strict_derivation(B, TF, I).
strict_derivation(G, TF, _) :- temporary_fact(G,TF), !.
strict_derivation(G, _, _) :- let_prolog_prove(G), !.
strict_derivation(G, TF, _) :- s_rule(G,B), strict_derivation(B, TF, direct).
strict_derivation(G, TF, inverted) :- find_inverted(G, I, B), strict_derivation(I, TF, inverted), strict_derivation(B, TF, direct).
find_inverted(G, I, N) :- s_rule(H,B), B \= true, complement(G, C), body_goal(C, B, N), complement(H, I).
body_goal(G, (G, B), B) :- !.
body_goal(G, (H, B), (H, B1)) :- !, body_goal(G, B, B1).
body_goal(G, G, true).
temporary_fact(H, [s_rule(H, _) | _]) :- !.
temporary_fact(H, [d_rule(H, _) | _]) :- !.
temporary_fact(H, [_ | Rest]) :- temporary_fact(H, Rest).
better(A, B) :- comparison_order(L), compute(L, A, B).
comparison_order(L) :- findall(X, comparison(X), L).
compute(L, A, B) :- member(M, L), Q =.. [M, A, B], call(Q).
defeater2assumption(arg(_, Ga), arg(B, _)) :- bodies(B, Bodies), assumptions(Bodies, Assump), member(not Ga, Assump), showOnTraceW(' assumption attack is better :) '), nlj, !.
more_informed(arg(A, _), arg(B, _)) :- one_rule_better(A, B), \+ one_rule_better(B, A), showOnTraceW(' better because of priorities! '), nlj.
one_rule_better([R | _], L) :- priority(R, Other), member(Other, L), !.
one_rule_better([_ | Rest], L) :- one_rule_better(Rest, L).
priority(d_rule(H1, B1), d_rule(H2, B2)) :- better_rule((H1 -< B1), (H2 -< B2)).
more_specific(arg(Ac, MetaA), arg(Bc, MetaB)) :- showOnTraceS('proving that '), showOnTraceS(MetaA), showOnTraceS(' is more specific than '), showOnTraceS(MetaB), nlOnTraceS, activation_sets(Ac, [([MetaA], trivial)], [], Act_A), obtain_non_trivial(Act_A, NTA), activation_sets(Bc,[([MetaB], trivial)], [], Act_B), obtain_non_trivial(Act_B, NTB), showOnTraceS(MetaA), showOnTraceS(' conjuntos de act: '), showOnTraceS(Act_A), nlOnTraceS, showOnTraceS(MetaB), showOnTraceS(' conjuntos de act: '), showOnTraceS(Act_B), nlOnTraceS, showOnTraceS(MetaA), showOnTraceS(' no triviales: '),     showOnTraceS(NTA),   nlOnTraceS, showOnTraceS(MetaB), showOnTraceS(' no triviales: '),     showOnTraceS(NTB),   nlOnTraceS, ( see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc), !  ; see_both_conditions(NTA, Ac, MetaA, NTB, Bc, MetaB), !  ; better_base(Ac, NTA, Act_A, Bc, NTB, Act_B), !) .
obtain_non_trivial([], []).
obtain_non_trivial([(X, notrivial) | Resto], [X | RestoNoTrivial]) :- obtain_non_trivial(Resto, RestoNoTrivial).
obtain_non_trivial([(_, trivial) | Resto], RestoNoTrivial) :- obtain_non_trivial(Resto, RestoNoTrivial).
see_empty(Act_A, NTA, Ac, _, NTB, _) :- NTA  = [], NTB \= [], based_on_facts(Ac, Act_A), !, showOnTraceS(' Gana por ser argum. vacio '), nlOnTraceS, pauseOnTraceS.
see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc) :- NTA = [], NTB = [], based_on_facts(Ac, Act_A), \+ based_on_facts(Bc, Act_B), !, showOnTraceS(' Gana por ser argum. vacio basado en hechos '), nlOnTraceS, pauseOnTraceS.
see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc) :- NTA \= [], NTB  = [], based_on_facts(Ac, Act_A), \+ based_on_facts(Bc, Act_B), !, showOnTraceS(' Gana por ser el otro vacio basado en presup '), nlOnTraceS, pauseOnTraceS.
based_on_facts(A, _) :- member(s_rule(_, true), A), !, showOnTraceS(A), showOnTraceS(' Based on facts... ').
based_on_facts(_, Act_sets) :- there_is_a_not(Act_sets), !, showOnTraceS(' a not in the arg ').
there_is_a_not([(Pri, _) | _]) :- member(not _, Pri), !.
there_is_a_not([_ | Resto]) :- there_is_a_not(Resto).
better_base(Ac, NTA, Act_A, Bc, NTB, Act_B) :- NTB \= [], NTA \= [], based_on_facts(Ac, Act_A), \+ based_on_facts(Bc, Act_B), showOnTraceS(' Tiene mejor base '), nlOnTraceS, pauseOnTraceS.
see_both_conditions(NTA, Ac, MetaA, NTB, Bc, MetaB) :- NTB \= [], NTA \= [], showOnTraceS('condicion 1, probar: '), showOnTraceS(MetaB), showOnTraceS(' con '), showOnTraceS(Bc), showOnTraceS(' y '), nlOnTraceS, pauseOnTraceS, see_condition1(NTA, Bc, MetaB), showOnTraceS(' Paso la condicion 1'), nlOnTraceS, showOnTraceS('condicion 2, no probar: '), showOnTraceS(MetaA), showOnTraceS(' con '), showOnTraceS(Ac), showOnTraceS(' y '), nlOnTraceS, pauseOnTraceS, see_condition2(NTB, Ac, MetaA), showOnTraceS(' Paso la condicion 2'), nlOnTraceS.
see_condition1([], _, _).
see_condition1([Conj_Act | Resto], Bc,MetaB) :- showOnTraceS(Conj_Act), nlOnTraceS, prove_with_Kg(MetaB, Conj_Act, Bc, [], _, []), see_condition1(Resto, Bc, MetaB).
see_condition2(NTB, Ac, MetaA) :- \+ forallActSet(NTB, Ac, MetaA).
forallActSet([],_,_).
forallActSet([Conj_Act|Resto],Ac,MetaA):- showOnTraceS(Conj_Act),nlOnTraceS, prove_with_arg(MetaA,Conj_Act,Ac,[],_), forallActSet(Resto,Ac,MetaA).
prove_with_Kg(true, _, _, Arg, Arg, _) :- !.
prove_with_Kg(Meta, Conj_Act, _, Arg, Arg, _) :- Meta \= (_, _), member(Meta, Conj_Act), !.
prove_with_Kg(Consulta, Conj_Act, Arg_hallado, Ac, Arg, Rama) :- Consulta = (Meta, RestoConsulta), prove_with_Kg(Meta, Conj_Act, Arg_hallado, Ac, NuevoAc, Rama), prove_with_Kg(RestoConsulta, Conj_Act, Arg_hallado, NuevoAc, Arg, Rama).
prove_with_Kg(Meta, Conj_Act, Arg_hallado, Ac, NuevoArg, Rama) :- Meta \= (_, _), \+ ciclo(Meta, Rama), ( temporary_fact(Meta, Ac), Regla = ninguna, Cuerpo = true, !  ; s_rule(Meta, Cuerpo), Regla = s_rule(Meta, Cuerpo), Cuerpo \= true ; member(d_rule(Meta, Cuerpo), Arg_hallado), Cuerpo \= true, Regla = d_rule(Meta, Cuerpo)), prove_with_Kg(Cuerpo, Conj_Act, Arg_hallado, Ac, Arg, [Meta | Rama]), guardar_regla(Regla,Arg,NuevoArg).
guardar_regla(ninguna, Arg, Arg) :- !.
guardar_regla(Regla, Arg, [Regla | Arg]) :- !.
prove_with_arg(true, _, _, Arg, Arg).
prove_with_arg(Meta, Conj_Act, _, Arg, Arg) :- Meta \= (_, _), member(Meta, Conj_Act), !.
prove_with_arg(Consulta, Conj_Act, Arg_hallado, Ac, Arg) :- Consulta = (Meta, RestoConsulta), prove_with_arg(Meta, Conj_Act, Arg_hallado, Ac, NuevoAc), prove_with_arg(RestoConsulta, Conj_Act, Arg_hallado, NuevoAc, Arg).
prove_with_arg(Meta, Conj_Act, Arg_hallado, Ac, NuevoArg) :- Meta \= (_, _), ( temporary_fact(Meta, Ac), Regla = ninguna, Cuerpo = true, !  ; member(s_rule(Meta, Cuerpo), Arg_hallado), Cuerpo \= true, Regla = s_rule(Meta, Cuerpo) ; member(d_rule(Meta, Cuerpo), Arg_hallado), Cuerpo \= true, Regla = d_rule(Meta, Cuerpo)), prove_with_arg(Cuerpo, Conj_Act, Arg_hallado, Ac,Arg), guardar_regla(Regla, Arg, NuevoArg).
ciclo(Meta, Rama) :- esta_en_la_rama(Meta, Rama).
esta_en_la_rama(X, [Pri | _]) :- instanciado(Pri), X == Pri, !.
esta_en_la_rama(X, [Pri | _]) :- \+ instanciado(Pri), X = Pri, !.
esta_en_la_rama(X, [_ | Y]) :- esta_en_la_rama(X, Y).
instanciado(T) :- ground(T).
variable_en([X | _]) :- var(X), !.
variable_en([X | _]) :- \+ var(X), \+ atom(X), X =.. L, variable_en(L), !.
variable_en([_ | Y]) :- variable_en(Y), !.
activation_sets(_, [], Act_Sets, Act_Sets).
activation_sets(Arg, [(Node, Tipo) | Not_visited_yet], Visited, Act_Sets) :- largo_lista(Node, Largo), hijos(Arg, (Node, Tipo), [], Hijos,Largo), apilar_si_no_visitado(Hijos, Not_visited_yet, Pila, Visited), activation_sets(Arg, Pila, [(Node,Tipo) | Visited], Act_Sets).
largo_lista([], 0).
largo_lista([_ | Cola], N) :- largo_lista(Cola, N1), N is N1 + 1.
hijos(_, _, Sale, Sale, 0).
hijos(Arg, (Conjunto, TipoPadre), Entra, Sale, Posicion) :- reemplazo(Posicion, Conjunto, Nuevo, Arg, TipoHijo), Pos is Posicion - 1, calcularTipo(TipoPadre, TipoHijo, Tipo), agregar_si_cambio(Nuevo, Conjunto, Entra, Nueva_Entra, Tipo), hijos(Arg, (Conjunto, TipoPadre), Nueva_Entra, Sale, Pos).
reemplazo(1, [Pri | Resto], Nueva, Arg, Tipo) :- obtener_antecedentes(Arg, Pri, Ant, Tipo), append(Ant, Resto, Nueva), !.
reemplazo(1, L, L, _, _) :- !.
reemplazo(Pos, [Pri | Resto], Nueva, Arg, Tipo) :- Pos1 is Pos - 1, reemplazo(Pos1, Resto, Reemplazada, Arg, Tipo), append([Pri], Reemplazada, Nueva).
obtener_antecedentes(Arg, Pri, Lista_Ant, trivial) :- member(s_rule(Pri, Antecedente), Arg), Antecedente \= true, pasar(Antecedente, Lista_Ant).
obtener_antecedentes(Arg, Pri, Lista_Ant, notrivial) :- member(d_rule(Pri, Antecedente), Arg), Antecedente \= true, pasar(Antecedente, Lista_Ant).
pasar(true, []) :- !.
pasar(Literal, [Literal]) :- Literal \= (_, _).
pasar((Literal, Resto), [Literal | RestoLista]) :- pasar(Resto, RestoLista).
agregar_si_cambio(Nuevo, Conjunto, Entra, [(Nuevo, Tipo) | Entra], Tipo) :- Conjunto \= Nuevo.
agregar_si_cambio(Nuevo, Conjunto, Entra, Entra, _) :- Conjunto = Nuevo.
calcularTipo(trivial, trivial, trivial) :- !.
calcularTipo(trivial, notrivial, notrivial) :- !.
calcularTipo(notrivial, _, notrivial) :- !.
apilar_si_no_visitado(Hijos, Not_visited_yet, Pila, Visited) :- difer(Hijos, Visited, Hijos_no_visitados), union(Hijos_no_visitados, Not_visited_yet, Pila), !.
union([], L, L) :- !.
union([X | C], L2, L3) :- member(X, L2), !, union(C, L2, L3).
union([X | C], L2, [X | L4]) :- union(C, L2, L4).
difer([], _, []).
difer([X | C], L2, L3) :- member(X, L2), difer(C, L2, L3).
difer([X | C2], L2, [X | C3]) :- difer(C2, L2, C3).
intersec([], _, []) :- !.
intersec([X | C], L2, [X | L3]) :- member(X, L2), !, intersec(C, L2, L3).
intersec([_ | C], L2, L4) :- intersec(C, L2, L4).
pause :- get0(_).
show(X) :- traceF(yes), write(X), !.
show(_).
shownl :- traceF(yes), nl, !.
shownl.
outputwait :- traceF(yes), \+ tofile(yes), pause, !.
outputwait.
showArg(arg(A, Q)) :- traceF(yes), write(' { '), nl, mostrarArgumento(A), write(' } for '), write(Q), !.
showArg(_).
mostrarArgumento([]) :- !.
mostrarArgumento([s_rule(H, B) | R]) :- write('      '), write((H <- B)), nl, mostrarArgumento(R), !.
mostrarArgumento([d_rule(H, B) | R]) :- write('      '), write((H -< B)), nl, mostrarArgumento(R), !.
mostrarLista([]) :- !.
mostrarLista([A | B]) :- showOnTraceW(A), nlj, mostrarLista(B), !.
showOnTraceW(X) :- traceW(yes), write(X), !.
showOnTraceW(_).
showArgOnTrace(arg(A, Q)) :- traceW(yes), write(' { '), nl, mostrarArgumento(A), write(' } for '), write(Q), !.
showArgOnTrace(_).
nlj :- traceW(yes), nl, !.
nlj.
pausej :- traceW(yes), tofile(no), pause, !.
pausej.
mostrara(X) :- trazaarg(yes), write(X), !.
mostrara(_).
nla :- trazaarg(yes), nl, !.
nla.
pausea :- trazaarg(yes), pause, !.
pausea.
showOnTraceS(X) :- traceSpecificity(yes), write(X), !.
showOnTraceS(_).
nlOnTraceS :- traceSpecificity(yes), nl, !.
nlOnTraceS.
pauseOnTraceS :- traceSpecificity(yes), pause, !.
pauseOnTraceS.
f :- loadProgram(e).
loadProgram(FileName) :- readProgram(FileName), check_SSet.
loadProgram(_) :- tell('load.log'), write('ERROR inconsistent Strict Set'), nl, told, !.
readProgram(File) :- tell('load.log'), write('DeLP program successfully loaded'), nl, told, string_concat(File, '.delp', FileNameS), string_to_atom(FileNameS, FileName), see(FileName), retractall( -<(_, _)    ), retractall( <-(_, _)    ), retractall( lastRule(_) ), loadRules, seen.
loadRules :- repeat, catch(read(Rule), _, loadErrorReport), loadRule(Rule), Rule = end_of_file.
loadRule(end_of_file) :- !.
loadRule(R) :- R = (_Head -< _Body), assert(R), lastAdd(R), !.
loadRule(R) :- R = (_Head <- _Body), assert(R), lastAdd(R), !.
loadRule(R) :- write('ERROR in line: '), write(R), nl, nl, !.
lastAdd(R):- retractall( lastRule(_) ), assert(     lastRule(R) ).
loadErrorReport:- lastRule(R), tell('load.log'), write('Syntactical Error, in the rule below: '),write(R),nl, told,!.
loadErrorReport:- tell('load.log'), write('Syntactical Error in the first rule'),nl, told,!.
check_SSet :- retractall( contradictoryProgram(_) ), pos_heads(P), neg_heads(N), intersec(N, P, I), I \= [], deriveApair(I), !.
check_SSet.
neg_heads(L) :- findall(Head, s_rule(~Head, _), L).
pos_heads(L) :- findall(Head, (s_rule(Head, _), Head \= ~_), L).
deriveApair([H | _]) :- strict_derivation( H, [], strict), strict_derivation(~H, [], strict), write('WARNING! the subset of strict rules of your program is contradictory '), nl, write('Literals "'), write(H), write('" and "'), write(~H), write('" have a strict derivation'), nl, asserta( contradictoryProgram(H) ).
deriveApair([_ | L]) :- deriveApair(L).
startFile(trace) :- tmpFileName(Name), fcreate(trace, Name, 0), output(trace).
tmpFileName(X) :- ticks(T), number_atom(T, A), cat([tmpDeLP, A], X, _).
endFile(F, Q, A) :- nl, write('Answer to '), write(Q), write(' is '), write(A), nl, fclose(F).
go(Q) :- startFile(F), answer(Q, A), endFile(F, Q, A).

