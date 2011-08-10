%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHANGELOG
%
% 2011/05/15: Iñaki Garay: code reformatting, comment clarification.
% 2011/02   : Gotti: Trying to parameterize the pruning.
% 2008/03/27: Gotti: Cambiamos el segundo predicado de d-rule/2 para que
%             contemple las reglas ignoradas en la consulta.
% 2007      : Nico: Agregado de Nico para las contextual queries y para 
%             construir los arboles SWI y XML.
%             It uses the files:
%             - delp_server.pl = to get the contexts
%             - tree_swi.pl = to build the swi trees
%             - tree_xml.pl = to build the xml trees
% 2006/11/27: Diego/Sergio: desabilitamos temporary facts en find_argument por 
%             anomalia y agregamos list_to_set para que el argumento sea un 
%             conjunto.
% 2006/11/10: Nueva linea con disagreement sub y defeater type linea es una lista 
%             de [sup/4 o int/4].
%             Cambiaron: find_defeater, acceptable y blockblock, junto con todos 
%             los predicados que usaban la linea de argumentacion (en todos los 
%             casos, se agregaron dos variables anonimas)
%             Nico cambio el lugar donde se consulta 'delp_aux.pl' (ahora es mas 
%             abajo), porque se necesitan los operadores ya definidos para 
%             interpretar los predicados que guardan la informacion para dibujar 
%             el arbol.
%             Tambien agrego: "let_prolog_prove(listing) :- !, fail."
%             y modifico better/2, lo cual requirio la inclusion de 
%             comparison_order/1 y compute/3.
% 2005/05/02: Strict_derivation tiene que considerar built-ins.
%             Agregado use_built_ins/1 como hecho para des/habilitar los 
%             built-ins.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
                        DeLP Interpreter
              Defeasible Logic Programming Interpreter

(c) Alejandro J. Garcia,  1998-today,
e-mail agarcia@cs.uns.edu.ar
http://cs.uns.edu.ar/~ajg

"To program is to understand" (Thanks Nigart ;)

running on:
    - LPA (win-)Prolog
    - SWI Prolog
    - Bin Prolog (for Jinni see below to exclude the operators)
*/

:- dynamic volatile_knowledge/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% begin "working on" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For SWI Prolog 
% Agregado por Gotti
:- discontiguous '<-'/2,' -<'/2, is_a_built_in/1.
:- multifile '<-' /2, '-<'/2, is_a_built_in/1.
:- dynamic <- /2, -< /2.



%------------------------------------------------------------------------------%
% This predicate defines the interface for remote data extraction.
% It now fails.

look_at_server(P) :- 
    fail, 
    on_server(P).



%------------------------------------------------------------------------------%
% This predicate defines the interface for built-ins.

let_prolog_prove(_) :-
    use_built_ins(no),
    !,
    fail. % Version web (built-ins deshabilitados)
let_prolog_prove(listing) :- 
    use_built_ins(yes),
    !,
    fail.
let_prolog_prove(H) :- 
    use_built_ins(yes),
    is_a_built_in(H), 
    % Ale(3/5/5): cambiamos call por catch para que capture las excepciones y 
    % falle en lugar de abortar el codigo.
    catch(H, _, fail).  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% end "working on" %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Operators definition

:- op(1101, xfx, -<).  % Defeasible Rules
:- op(1101, xfx, <-).  % Strict Rules
:- op(190,  fx,  ~).   % Strong negation
:- op(191,  fx,  not). % Default negation



%------------------------------------------------------------------------------%
% Complementary literals definition (Observation: no nested " ~ " are allowed).

complement(~A, A) :- A \= ~ _.
complement(A, ~A) :- A \= ~ _.



%------------------------------------------------------------------------------%
% Some Prologs fail if no rules are defined at all, these two dummy rules below 
% are for that reason.

paraquenofallesinohayninguna -< true.
paraquenofallesinohayninguna <- true.



%------------------------------------------------------------------------------%
% Compatibility between internal representation and program rules with
% operators.

%s_rule(A,B) :- 
%    volatile_knowledge(_, List), 
%    member(A <- B, List),
%    context_ignore(CXign),
%    not( member(A <- B,CXign) ).
s_rule(A, B) :- 
    context_add(CXadd),
    member((A <- B), CXadd).
s_rule(A, B) :- 
    (A <- B), 
    context_ignore(CXign),
    not( member(A <- B,CXign) ).



%------------------------------------------------------------------------------%
%d_rule(A,B) :- 
%    volatile_knowledge(_, List), 
%    member(A -< B,List).
d_rule(A, B) :- 
    context_add(CXadd), 
    member((A -< B), CXadd).
% Antes cuando no se podian ignorar reglas rebatibles.
%d_rule(A, B) :- 
%    A -< B. 
d_rule(A, B) :- 
    (A -< B), 
    context_ignore(CXign),
    not( member(A -< B, CXign) ).

% Internal representation of rules without operators.
% Head -< Body is represented d_rule(Head,Body).
% Head <- Body is represented s_rule(Head,Body).
% ~ Atom is represented no(Atom)

% For BIN Prolog, negation is represented by the predicate no/1.
%complement(no(A), A) :- A \= no(_).
%complement(A, no(A)) :- A \= no(_).



%-------------------------------------------------------------------------------
% Answer for Q is
% yes       : if Q is warrant
% no        : if complement of Q is warrant
% unknown   : if Q is not in the signature of the DLP program
% undecided : otherwise (neither Q nor its complement is warrant)

answer(Q, yes) :-
    warrant(Q, _),
    !.
answer(Q, no) :-
    show('There is no warrant for '),
    show(Q),
    show('. We will see the contrary ...'),
    shownl,
    outputwait,
    complement(Q, CQ),
    warrant(CQ, _),
    !.
answer(Q, unknown) :- 
    % Gelfond's suggestion
    \+ in_signature(Q),
    show('not_in_signature'),
    shownl,
    !. 
%answer(Q, unknown) :- 
%    \+ find_argument(Q, _, _),
%    complement(Q, CQ), 
%    \+ find_argument(CQ, _, _),
%    show('not_argument'),
%    shownl,
%    !.
answer(_, undecided).



%------------------------------------------------------------------------------%
in_signature(Q) :-
    s_rule(Head, _),
    in_literal(Q, Head),
    !.
in_signature(Q) :-
    d_rule(Head, _),
    in_literal(Q, Head),
    !.
in_signature(Q) :-
    s_rule(_, Body),
    in_body(Q, Body),
    !.
in_signature(Q) :-
    d_rule(_, Body),
    in_body(Q, Body),
    !.



%------------------------------------------------------------------------------%
in_body(Q, L) :-
    in_literal(Q, L),
    !.
in_body(Q, (L, _)) :-
    in_literal(Q, L),
    !.
in_body(Q, (_, B)) :- 
    in_body(Q, B),
    !.



%------------------------------------------------------------------------------%
% Q is Q or Q is the complement of C.

in_literal(Q, Q) :- 
    !.
in_literal(Q, C) :- 
    complement(Q, C).



%------------------------------------------------------------------------------%
exp(Q,ANS) :- 
    all(Q, L), 
    complement(Q, CQ),
    all(CQ, CL), 
    obtain_answer(Q, L, CL, ANS).



%------------------------------------------------------------------------------%
%obtain_answer(Q, L, CL, yes(L)) :- 
%    ground(Q),
%    L  \= [],
%    CL \= [],
%    !.

obtain_answer(Q, L, CL, yes(L, CL)) :- 
    ground(Q),
    L  \= [],
    CL \= [],
    !.
obtain_answer(_, L, _, yes(L)) :- 
    L \= [],
    !.
obtain_answer(_, _, CL, no(CL)) :- 
    CL \= [],
    !.
obtain_answer(Q, _, _, unknown(Q)) :- 
    % Gelfond's suggestion
    \+ in_signature(Q),
    !.
%obtain_answer(Q, _, _, undecided_no_argument(Q)) :- 
%    \+ fa(Q, _), 
%    complement(Q, CQ), 
%    \+ fa(CQ, _).
obtain_answer(Q, _, _, undecided(Q)).



%------------------------------------------------------------------------------%
%all(Q, List) :- 
%    findall((Q, A), warrant(Q, A), List).
%all(Q, Set) :-
%    setof(Q, warrant(Q, _), Set), 
%    !.
all(Q, Set) :- 
    findall(Q, warrant(Q, _), List), 
    list_to_set(List, Set).
%all(_, []).



%------------------------------------------------------------------------------%
% DeLP full interpreter: warrant for query Q.

warrant(Q, A) :-                                % Q is a warranted literal
    shownl,
    show('Main query: '),
    show(Q),
    shownl,
    shownl,
    find_argument(Q, A, []),                    % if A is an argument for Q
    toRoot(A), % Para el graficador.
    xml_toRoot(A),
    show('Supporting Argument: '),
    shownl,
    showArg(arg(A, Q)),
    shownl,
    outputwait,
    \+ defeated(A, [sup(A, Q, void, void)]).    % and A is not defeated.



%------------------------------------------------------------------------------%
% Version con poda.

defeated(A, ArgLine) :-                         % A is defeated
    showOnTraceW('Line:'),
    nlj,
    mostrarLista(ArgLine),
    pausej,
    find_acceptable_defeater(A, ArgLine, D, Disag, Status, NewArgLine),
    Disag = arg(_, AttkP),
    toTree(A, D, AttkP, Status),     % Para el graficador.
    xml_toTree(A, D, AttkP, Status), % Para el graficador.
    show('is defeated by: '),
    shownl,
    showArg(arg(D, _G)),
    shownl,
    outputwait,
    \+ defeated(D,NewArgLine).                  % and D is not defeated.
    %defeated(D, NewArgLine),
    %fail.



%------------------------------------------------------------------------------%
% Version sin poda.
% Tiene algunos problemas con las lineas aceptables.
% Creo que es el or en find_acceptable_defeater

defeated2(A, ArgLine) :-                        % A is defeated
    %findall(
    %    ad(A, ArgLine, D, Disag, Status, NewArgLine),
    %    find_acceptable_defeater(A, ArgLine, D, Disag, Status, NewArgLine),
    %    AdList),
    trace,
    acceptable_defeaters(A, ArgLine, AdList),
    drawDefeaters(AdList),
    noDefeatedDefeaters(AdList, X),
    X > 0.



%------------------------------------------------------------------------------%
noDefeatedDefeaters([], 0).
noDefeatedDefeaters([Z | Rest], NnDD) :-
    Z = ad(_A,_ArgLine, D, _Disag, _Status, NewArgLine),
    (
        defeated2(D,NewArgLine),
        noDefeatedDefeaters(Rest,RestNnDD),
        NnDD is RestNnDD
    ;
        noDefeatedDefeaters(Rest,RestNnDD),
        NnDD is RestNnDD + 1
    ).



%------------------------------------------------------------------------------%
drawDefeaters(AdList) :-
    forall(
        member(ad(A, _ArgLine, D, Disag, Status, _NewArgLine), AdList),
        (
            Disag = arg(_, AttkP),
            toTree(A, D, AttkP, Status), % Para el graficador.
            xml_toTree(A, D, AttkP, Status)
        )
        ).



%------------------------------------------------------------------------------%
find_acceptable_defeater(A, ArgLine, D, Disag, Status, NewArgLine) :-
    find_defeater(A, arg(D, G), ArgLine, SubA, DefeaterType),       % If there is a defeater D
    acceptable(arg(D, G), SubA, DefeaterType, ArgLine, NewArgLine), % no fallacies are found.
    (
        member( sup(D, Conc, Disag, Status), NewArgLine)
    ;
        member( int(D, Conc, Disag, Status), NewArgLine)
    ).

%------------------------------------------------------------------------------%
% Otra alternativa para obtener los acceptable Defeaters.

acceptable_defeaters(A, ArgLine, AccDefList) :-
    findall(
        def(A, arg(D, G), ArgLine, SubA, DefeaterType),
        find_defeater(A, arg(D, G), ArgLine, SubA, DefeaterType),
        DefList),
    select_acceptables(DefList, AccDefList).



select_acceptables([], []) :- 
    !.
select_acceptables([Def | Rest], [ADef, RestAccetables]) :-
    isAcceptable(Def, ADef),
    select_acceptables(Rest, RestAccetables),
    !.
select_acceptables([_ | Rest], RestAcceptables) :-
    select_acceptables(Rest, RestAcceptables).



isAcceptable(def(A, arg(D, G), ArgLine, SubA, DefeaterType), ad(A, ArgLine, D, Disag, Status, NewArgLine)) :-
    acceptable(arg(D, G), SubA, DefeaterType, ArgLine, NewArgLine), % No fallacies are found.
    (
        member(sup(D, Conc, Disag, Status), NewArgLine)
    ;
        member(int(D, Conc, Disag, Status), NewArgLine)
    ).

%------------------------------------------------------------------------------%
% Finds a new defeater C (proper/blocking) for A with disagreement subargument 
% SubA, ArgLine is used for concordancy.
% Input:  A and ArgLine
% Output: C and SubA

find_defeater(A, C, ArgLine, SubA, DefeaterType) :- % C is a defeater (proper or blocking)
    (
        find_counterargument(A, C, SubA, ArgLine),  % if C counterargues A attacking SubA (and it is concordant with partners)
        showOnTraceW('Counter-argument: '),
        nlj,
        showArgOnTrace(C),
        nlj,
        pausej,
        \+ better(SubA, C),                         % and SubA is not better than C
        defeater_type(C, SubA, DefeaterType)
    ;                                               % or
        find_attack2assumption(A, C, ArgLine),      % C attacks a default literal.
        DefeaterType = proper
    ).



%------------------------------------------------------------------------------%
defeater_type(D, A, proper) :-
    better(D, A),
    !.
defeater_type(_, _, blocking). % With blocking instantiated always succeed.



%------------------------------------------------------------------------------%
% acceptable/3 looks into ArgLine to see whether arg(N,G) mantains the line 
% acceptable if there is no problem it returns NewArgLine with the new argument 
% and calculates the type of argument: support or intereference.

acceptable(arg(NewArg, G), SubA, DefeaterType, ArgLine, NewArgLine) :-
    \+ circular(arg(NewArg, G), ArgLine),
    (                                                                % If there are not fallacies
        ArgLine = [sup(A, Q, _, _) | _],                             % then the new arg is "int"
        NewArgLine = [int(NewArg,G,SubA,DefeaterType)|ArgLine],      % when the previous is "sup"
        !
    ;                                                                % or
        ArgLine = [int(A,Q,_,_)|_],                                  % the new one is "sup"
        NewArgLine = [sup(NewArg, G, SubA, DefeaterType) | ArgLine], % when the last one is "int".
        !
    ),
    \+ blockblock(NewArgLine).



%------------------------------------------------------------------------------%
blockblock(NewLine) :-
    use_blockblock(yes),            % Si esta puesto en yes chequea que no haya dos bloqueos seguidos.
    NewLine = [Arg1, Arg2, _ | _],  % Line has 3 or more arguments
    ( 
        Arg1 = sup(_, _, _, blocking), 
        Arg2 = int(_, _, _, blocking), 
        !
    ;
        Arg1 = int(_, _, _, blocking), 
        Arg2 = sup(_, _, _, blocking), 
        !
    ),
    showOnTraceW('!! blocking - blocking situation'),
    nlj.



%------------------------------------------------------------------------------%
circular(arg(A, _), [int(B, _, _, _) | _]) :-
    is_contained(A, B),
    !,
    showOnTraceW('circularity '),
    nlj. 
    %showOnTraceW(A),
    %nlj,
    %showOnTraceW(B),
    %nlj,
    %pausej.
circular(arg(A, _), [sup(B, _, _, _) | _]) :-
    is_contained(A, B),
    !,
    showOnTraceW('circularity '),
    nlj.
    %showOnTraceW(A),
    %nlj,
    %showOnTraceW(B),
    %nlj,
    %pausej.
circular(arg(A, Q), [_ | RestOfLine]) :-
    circular(arg(A, Q), RestOfLine),
    !.



%------------------------------------------------------------------------------%
is_contained([], _) :-
    !.
is_contained([Clause | Cs], Arg) :- 
    Clause = d_rule(Q, B),
    member(d_rule(Q, B), Arg),
    is_contained(Cs, Arg).
is_contained([Clause | Cs], Arg) :- 
    Clause \= d_rule(_, _),
    is_contained(Cs, Arg).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for Parallel/Distributed implementation in bin-Prolog.

%------------------------------------------------------------------------------%
allpoints(A, P) :-
    pointsOfAttack(A, Points),
    bodies(A, Bodies), 
    assumptions(Bodies, Assump),
    append(Points, Assump, P).



%------------------------------------------------------------------------------%
defeatAPoint(P, A, C, Line) :-
    (
        P = not L, 
        find_argument(L, C, Line)
    ;
        P \= not _,
        attackAPoint(P, A, C, Line)
    ;
        P \= not _,
        attackAnOuterPoint(P, A, C,_, Line)
    ).



%------------------------------------------------------------------------------%
attackAPoint(P,A,arg(CounterA,CompP),ArgLine):-
    complement(P, CompP),
    find_argument(CompP, CounterA, ArgLine),
    find_subargument(P, A, SubA),
    \+ better(arg(SubA, P),arg(CounterA, CompP)).



%------------------------------------------------------------------------------%
attackAnOuterPoint(P,A,arg(CounterA,Qc),arg(SubA,Lit),ArgLine):-
    complement(P, CompP),
    find_inverted(CompP, NewQ, RestOfBody),             % Use inverted.
    strict_derivation(RestOfBody, A, direct),           % Strong proof without inverted.
    (
        find_argument(NewQ, CounterA, ArgLine),         % Counterargumet is found for NewQ
        complement(NewQ, Lit),                          % in the point Lit
        Qc = NewQ
    ;                                                   % or
        complement(NewQ, G),                            % continue inverting from G.
        attackAnOuterPoint(G, A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine)
    ),
    find_subargument(Lit,A,SubA),
    \+ better(arg(SubA,Lit),arg(CounterA,Qc)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Default Negation
% warrant(not L,A) todavia no anda
% blocking - blocking?

%------------------------------------------------------------------------------%
find_attack2assumption(A, arg(C, Q), ArgLine) :-
    bodies(A, Bodies),
    assumptions(Bodies, Assump),
    showOnTraceW(' ....... assumptions: '),
    showOnTraceW(Assump),
    nlj,
    attack_assumption(Assump, arg(C, Q), ArgLine).



%------------------------------------------------------------------------------%
attack_assumption([not P | _], arg(C, P), Line) :- 
    find_argument(P, C, Line).
attack_assumption([_ | Ps], C, L) :-
    attack_assumption(Ps, C, L).



%------------------------------------------------------------------------------%
% Collects body rules in a list

bodies([], []).
bodies([d_rule(_, Body) | Clauses], [Body | Bs]) :- 
    bodies(Clauses, Bs).
bodies([s_rule(_, Body) | Clauses], [Body | Bs]) :- 
    bodies(Clauses, Bs).



%------------------------------------------------------------------------------%
% Collects assumed literals "not L"

assumptions([], []).
assumptions([(A, B) | Rest], Points) :- 
    assumptions([B | Rest], R),
    (
        A = not _,
        \+ member(A, R), 
        Points = [A | R],
        !
    ;
        Points = R
    ).
assumptions([A | Rest], Points) :- 
    A \= (_, _),
    assumptions(Rest, R),
    (
        A = not _,
        \+ member(A, R), 
        Points = [A | R],
        !
    ;
        Points = R
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find_counterargument looks for a counterargument for A ArgLine is needed for 
% concordancy check in find_argument predicate.

%------------------------------------------------------------------------------%
find_counterargument(A, Counter, SubA, ArgLine) :-
    pointsOfAttack(A, Points),
    showOnTraceW(' Possible attack points: '),
    showOnTraceW(Points),nlj,
    (
        attackInnerPoint(Points, A, Counter, SubA, ArgLine) % Point in A
    ;                                                       % or
        attackOuterPoint(Points, A, Counter, SubA, ArgLine) % point out of A.
    ).

%------------------------------------------------------------------------------%
% Originally: pointsOfAttack returns the set of consequents of clauses that are 
% not facts. Now returns the set of consequents of defeasible rules, strict 
% rule heads are considered by attackOuterPoint.

pointsOfAttack([], []).
pointsOfAttack([d_rule(P, _) | Clauses], [P | Ps]) :-
    pointsOfAttack(Clauses, Ps).
pointsOfAttack([s_rule(_, _) | Clauses], Ps) :- 
    pointsOfAttack(Clauses, Ps).

% CHANGE 5/00 
%pointsOfAttack([s_rule(P, Q) | Clauses], [P | Ps]) :- 
%    Q \= true,pointsOfAttack(Clauses, Ps).
%pointsOfAttack([s_rule(_, true) | Clauses], Ps) :-
%    pointsOfAttack(Clauses, Ps).



%------------------------------------------------------------------------------%
attackInnerPoint([P | _], A, arg(CounterA, CompP), arg(SubA, P), ArgLine) :- 
    % Counterargue P.
    complement(P, CompP),
    find_argument(CompP, CounterA, ArgLine),
    find_subargument(P, A, SubA).
attackInnerPoint([_ | Ps], A, C, S, L) :-
    % Counterargue other points.
    attackInnerPoint(Ps, A, C, S, L).



%------------------------------------------------------------------------------%
attackOuterPoint([P | _], A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine) :-     % Start from P.
    complement(P, CompP),
    find_inverted(CompP, NewQ, RestOfBody),                                     % Use inverted.
    strict_derivation(RestOfBody, A, direct),                                   % Strong proof without inverted.
    (
        find_argument(NewQ, CounterA, ArgLine),                                 % Counterargumet is found for NewQ
        complement(NewQ, Lit),                                                  % in the point Lit
        Qc = NewQ
    ;                                                                           % or
        complement(NewQ, G),                                                    % continue inverting from G.
        attackOuterPoint([G], A, arg(CounterA, Qc), arg(SubA, Lit), ArgLine)
    ),
    find_subargument(Lit, A, SubA).
attackOuterPoint([_ | Ps], A, C, S, L) :-
    % Start from other points.
    attackOuterPoint(Ps, A, C, S, L).



%------------------------------------------------------------------------------%
find_subargument(true, _, []) :- 
    !.
find_subargument((not _), _, []) :-
    !.
find_subargument((Q1, Q2), Arg, SubArg) :-
    !,
    find_subargument(Q1, Arg, S1),
    find_subargument(Q2, Arg, S2),
    append(S1, S2, SubArg).
find_subargument(Q, Arg, [Clause | Cs]) :-
    (
        member(d_rule(Q, B), Arg),
        Clause = d_rule(Q, B)
    ;
        s_rule(Q, B),
        Clause = s_rule(Q, B)
    ;
        let_prolog_prove(Q),
        Clause = s_rule(Q, true)
    ;
        look_at_server(Q),
        Clause = s_rule(Q, true)
    ),
    find_subargument(B, Arg, Cs).



%------------------------------------------------------------------------------%
% Cuando usaba temporary facts.

%find_argument(Q, A, Line) :- 
%    argument(Q, [], A, Line). 
find_argument(Q, A, Line) :- 
    argument(Q, [], A_list, Line), 
    list_to_set(A_list, A).


%------------------------------------------------------------------------------%
% argument(Q, _, A, Line) looks for an argument A for query Q
% (and updates argumentation line Line) ?
% Consistency check is replaced by concordancy with the line.

argument(true, D, D, _) :-
    !.
argument((not L), D, D, _) :- 
    showOnTraceW(checking_assumptions(not L, D)), % "default negated literals are `assumptions' on which the derivation is based".
    nlj,
    \+ strict_derivation(L, D, inverted),         % unless L has a strict derivation or L is a head of a rule in the current derivation.
    !.
argument((A,B), CurrentDerivation, ABDerivation, Line) :- 
    !,
    argument(A, CurrentDerivation, ADerivation, Line),
    argument(B, ADerivation, ABDerivation, Line).
% Caso quitado por anomalia: no deja hacer backtracking dentro del cuerpo de una regla.
%argument(H, D, D, _) :- 
%    temporary_fact(H,D), 
%    !.
argument(H, CurrentDerivation, Argument, Line) :-
    (
        s_rule(H, B),
        Clause = s_rule(H, B)
    ;
        d_rule(H, B),
        Clause = d_rule(H, B)
    ;
        let_prolog_prove(H),
        Clause = s_rule(H, true)
    ;
        look_at_server(H),
        Clause = s_rule(H, true)
    ),
    argument(B, CurrentDerivation, BodyDerivation, Line),
    Argument = [Clause | BodyDerivation],
    verify_concordancy(H, Argument, Line),
    verify_assumptions(H, Argument).



%------------------------------------------------------------------------------%
% H should not be accepted if "not H" was assumed.
verify_assumptions(H, Derivation) :- 
    bodies(Derivation, Bodies), % Collect bodies.
    assumptions(Bodies, Assump),% Collect assumptions.
    showOnTraceW(verifying_assumptions(H, Derivation)),
    nlj,
    not member(not H, Assump).  % Check if "not H" was assumed.

%------------------------------------------------------------------------------%
verify_concordancy(G,TF,Line) :-
    (
        Line = [],                                          % If Line is empty (first argument of line)
        All_TF = TF,                                        % then use only TF of current derivation.
        !
    ;
        Line = [sup(_, _, _, _) | _],                       % If the previous is sup then
        collect_int([int(TF, G, _, _) | Line], All_TF)      % collect interference TF
    ;                                                       % or
        Line = [int(_, _, _, _) | _],                       % if the previous is int then
        collect_sup([sup(TF, G, _, _) | Line], All_TF)      % collect supporting TF.
    ),                                                      % Then, verify  consistency
    verify_consistency(G, All_TF),                          % with all the TF of the line.
    !.



%------------------------------------------------------------------------------%
collect_int([], []) :-
    !.
collect_int([int(TF, _, _, _) | Line], All_TF) :-
    collect_int(Line, RestTF),
    append(TF, RestTF, All_TF),
    !.
collect_int([sup(_, _, _, _) | Line], RestTF) :-
    collect_int(Line,RestTF),
    !.



%------------------------------------------------------------------------------%
collect_sup([], []) :- 
    !.
collect_sup([sup(TF, _, _, _) | Line], All_TF) :-
    collect_sup(Line, RestTF),
    append(TF, RestTF, All_TF),
    !.
collect_sup([int(_, _, _, _) | Line], RestTF) :-
    collect_sup(Line, RestTF),
    !.



%------------------------------------------------------------------------------%
% A goal G is consistent with the current argument if there is no strict 
% derivation of the complement of G.

verify_consistency(H, TF) :- 
    complement(H, G), 
    \+ strict_derivation(G, TF, inverted), 
    !.
verify_consistency(H, _) :- 
    showOnTraceW(H),
    showOnTraceW(' is contradictory !'),
    nlj,
    fail.



%------------------------------------------------------------------------------%
% A strict derivation only uses stricts rules, Temporary Facts, Inverted Rules, 
% and ...

strict_derivation(true, _, _) :- 
    !.
strict_derivation((A, B), TF, I) :- 
    !, 
    strict_derivation(A, TF, I),
    strict_derivation(B, TF, I).
strict_derivation(G, TF, _) :- 
    temporary_fact(G,TF),
    !.
strict_derivation(G, _, _) :- 
    % Ale(2/5/5): agregado porque faltaba...
    let_prolog_prove(G), 
    !.
strict_derivation(G, TF, _) :- 
    s_rule(G,B), 
    strict_derivation(B, TF, direct).
strict_derivation(G, TF, inverted) :- 
    find_inverted(G, I, B),
    strict_derivation(I, TF, inverted),
    strict_derivation(B, TF, direct).



%------------------------------------------------------------------------------%
find_inverted(G, I, N) :-
    s_rule(H,B),
    B \= true,
    complement(G, C),
    body_goal(C, B, N),
    complement(H, I).



%------------------------------------------------------------------------------%
body_goal(G, (G, B), B) :- 
    !.
body_goal(G, (H, B), (H, B1)) :- 
    !, 
    body_goal(G, B, B1).
body_goal(G, G, true).



%------------------------------------------------------------------------------%
% H is a temporary fact if H is the Head of a proved subgoal, that is, a Head 
% of a clause in the current derivation.

temporary_fact(H, [s_rule(H, _) | _]) :-
    !.
temporary_fact(H, [d_rule(H, _) | _]) :- 
    !.
temporary_fact(H, [_ | Rest]) :-
    temporary_fact(H, Rest).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%------------------------------------------------------------------------------%
% <A,h> is better than <B,q> if
% 1) <A,h> is a defeater to an assumption in B, ie "not h" was assumed in B or
% 2) A is more specific than B or
% 3) A is more informed than B

%better(A, B) :- 
%    defeater2assumption(A, B),
%    !.
%better(A, B) :- 
%    more_specific(A, B),
%    !.
%better(A, B) :- 
%    more_informed(A, B).

% Dadas las estructuras de argumento A y B, obtiene el orden definido por el
% usuario y va evaluando segun cada criterio de comparacion.
better(A, B) :-
    comparison_order(L),
    compute(L, A, B).



%------------------------------------------------------------------------------%
comparison_order(L) :-
    findall(X, comparison(X), L).



%------------------------------------------------------------------------------%
compute(L, A, B) :-
    member(M, L),
    Q =.. [M, A, B],
    call(Q).



%------------------------------------------------------------------------------%
% Comparison criterion for attack to assumptions (default negation).
% <A,h> is better than <B,q> if 
%     <A,h> is a defeater to an assumption in B, 
% i.e. "not h" was assumed in B,

defeater2assumption(arg(_, Ga), arg(B, _)) :-
    bodies(B, Bodies),
    assumptions(Bodies, Assump),
    member(not Ga, Assump), 
    showOnTraceW(' assumption attack is better :) '),
    nlj,
    !.



%------------------------------------------------------------------------------%
% Comparison criterion using priorities among rules.
% <A,h> is better than <B,q> if
%     there exists ra from A and rb from  B such that ra > rb
%     but there is not rb' from B and ra' from A such that rb' > ra' .

more_informed(arg(A, _), arg(B, _)) :-
    one_rule_better(A, B),
    \+ one_rule_better(B, A),
    showOnTraceW(' better because of priorities! '),
    nlj.



%------------------------------------------------------------------------------%
one_rule_better([R | _], L) :- 
    priority(R, Other),
    member(Other, L),
    !.
one_rule_better([_ | Rest], L) :- 
    one_rule_better(Rest, L).



%------------------------------------------------------------------------------%
priority(d_rule(H1, B1), d_rule(H2, B2)) :-
    better_rule((H1 -< B1), (H2 -< B2)).



%------------------------------------------------------------------------------%
% General specifity comparison criterion.
% Given two completed argumentes <Ac,h1> y <Bc,h2> ,
% A is strictly more specific than A (A > B) iff
% 
% 1. For all C in Lit(Ac)
%    IF C U Ac |~ h1, THEN C U Kg U Bc |~ h2
% 2. There exists C' in Lit(Bc)
%    that: Bc|~ h2 but C' U Ac  no |~ h1
% 
% Equivalent:
% 1. For all non-trivial activation set S of A
%    exists a defeasible derivation for h2 with S U B U Kg
% 2. There exists a non-trivial activation set S' of B
%    which there is no def. derivation for h1 from S' U A

more_specific(arg(Ac, MetaA), arg(Bc, MetaB)) :-
    showOnTraceS('proving that '),
    showOnTraceS(MetaA),
    showOnTraceS(' is more specific than '),
    showOnTraceS(MetaB), 
    nlOnTraceS,
    activation_sets(Ac, [([MetaA], trivial)], [], Act_A),       % Calculates act. sets of Ac.
    obtain_non_trivial(Act_A, NTA),                             % Selects non trivial ones.
    activation_sets(Bc,[([MetaB], trivial)], [], Act_B),        % Idem for argument Bc.
    obtain_non_trivial(Act_B, NTB),
    showOnTraceS(MetaA), showOnTraceS(' conjuntos de act: '), showOnTraceS(Act_A), nlOnTraceS,
    showOnTraceS(MetaB), showOnTraceS(' conjuntos de act: '), showOnTraceS(Act_B), nlOnTraceS,
    showOnTraceS(MetaA), showOnTraceS(' no triviales: '),     showOnTraceS(NTA),   nlOnTraceS,
    showOnTraceS(MetaB), showOnTraceS(' no triviales: '),     showOnTraceS(NTB),   nlOnTraceS,
    (
        see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc),              % A > B if A is empty
        !
    ;                                                           % or
        see_both_conditions(NTA, Ac, MetaA, NTB, Bc, MetaB),    % both cond. hold
        !
    ;                                                           % or
        better_base(Ac, NTA, Act_A, Bc, NTB, Act_B),            % A has a better base.
        !
    ) .



%------------------------------------------------------------------------------%
obtain_non_trivial([], []).
obtain_non_trivial([(X, notrivial) | Resto], [X | RestoNoTrivial]) :-
    obtain_non_trivial(Resto, RestoNoTrivial).
obtain_non_trivial([(_, trivial) | Resto], RestoNoTrivial) :-
    obtain_non_trivial(Resto, RestoNoTrivial).



%------------------------------------------------------------------------------%
% Argument Ac is strictly more specific than Bc, if
% 1. NTA is empty, NTB is NOT empty, and Ac is based on facts
% 2. NTA and NTB are empty, and Ac is based on facts but Bc on pressumptions
% 3. NTA is empty, NTB is NOT empty, both Ac and Bc based on pressumptions.
% 4. NTA is NOT empty, NTB is empty, Ac is based on facts, but Bc not.

see_empty(Act_A, NTA, Ac, _, NTB, _) :- 
    NTA  = [], 
    NTB \= [],
    based_on_facts(Ac, Act_A), 
    !, 
    showOnTraceS(' Gana por ser argum. vacio '),
    nlOnTraceS,
    pauseOnTraceS.
see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc) :- 
    NTA = [],
    NTB = [],
    based_on_facts(Ac, Act_A), 
    \+ based_on_facts(Bc, Act_B),
    !,
    showOnTraceS(' Gana por ser argum. vacio basado en hechos '),
    nlOnTraceS,
    pauseOnTraceS.
%see_empty(Act_A, NTA, Ac, Act_B, NTB,Bc) :- 
%    NTA = [],
%    NTB \= [],
%    \+ based_on_facts(Ac, Act_A), 
%    \+ based_on_facts(Bc, Act_B),
%    !, 
%    showOnTraceS(' Gana por ser argum. vacio basado en presup '),
%    nlOnTraceS,
%    pauseOnTraceS.
see_empty(Act_A, NTA, Ac, Act_B, NTB, Bc) :- 
    NTA \= [], 
    NTB  = [],
    based_on_facts(Ac, Act_A), 
    \+ based_on_facts(Bc, Act_B),
    !,
    showOnTraceS(' Gana por ser el otro vacio basado en presup '),
    nlOnTraceS,
    pauseOnTraceS.



%------------------------------------------------------------------------------%
based_on_facts(A, _) :-
    member(s_rule(_, true), A),
    !,
    showOnTraceS(A),
    showOnTraceS(' Based on facts... ').
based_on_facts(_, Act_sets) :-
    there_is_a_not(Act_sets),
    !,
    showOnTraceS(' a not in the arg ').



%------------------------------------------------------------------------------%
there_is_a_not([(Pri, _) | _]) :-
    member(not _, Pri),
    !.
there_is_a_not([_ | Resto]) :-
    there_is_a_not(Resto).



%------------------------------------------------------------------------------%
better_base(Ac, NTA, Act_A, Bc, NTB, Act_B) :-
    NTB \= [], % Si no son vacios.
    NTA \= [],
    based_on_facts(Ac, Act_A),
    \+ based_on_facts(Bc, Act_B),
    showOnTraceS(' Tiene mejor base '),
    nlOnTraceS,
    pauseOnTraceS.



%------------------------------------------------------------------------------%
see_both_conditions(NTA, Ac, MetaA, NTB, Bc, MetaB) :-
    NTB \= [],
    NTA \= [],
    showOnTraceS('condicion 1, probar: '), 
    showOnTraceS(MetaB),
    showOnTraceS(' con '),
    showOnTraceS(Bc),
    showOnTraceS(' y '),
    nlOnTraceS,
    pauseOnTraceS,
    see_condition1(NTA, Bc, MetaB),
    showOnTraceS(' Paso la condicion 1'),
    nlOnTraceS,
    showOnTraceS('condicion 2, no probar: '), 
    showOnTraceS(MetaA),
    showOnTraceS(' con '),
    showOnTraceS(Ac),
    showOnTraceS(' y '),
    nlOnTraceS,
    pauseOnTraceS,
    see_condition2(NTB, Ac, MetaA),
    showOnTraceS(' Paso la condicion 2'),
    nlOnTraceS.



%------------------------------------------------------------------------------%
% 1. For all NTA set S exists a defeasible derivation for h2 with S U B U Kg
see_condition1([], _, _).
see_condition1([Conj_Act | Resto], Bc,MetaB) :-
    showOnTraceS(Conj_Act),
    nlOnTraceS,
    prove_with_Kg(MetaB, Conj_Act, Bc, [], _, []),
    see_condition1(Resto, Bc, MetaB).
% 2. one S' in NTB does not satisfy that  S' U A  |~ h1
see_condition2(NTB, Ac, MetaA) :- 
    \+ forallActSet(NTB, Ac, MetaA).

%------------------------------------------------------------------------------%
forallActSet([],_,_).
forallActSet([Conj_Act|Resto],Ac,MetaA):-
                showOnTraceS(Conj_Act),nlOnTraceS,
    prove_with_arg(MetaA,Conj_Act,Ac,[],_),
    forallActSet(Resto,Ac,MetaA).



%------------------------------------------------------------------------------%
prove_with_Kg(true, _, _, Arg, Arg, _) :-
    % true is proven.
    !.
prove_with_Kg(Meta, Conj_Act, _, Arg, Arg, _) :-
    % Members of Act set are proven.
    Meta \= (_, _), 
    member(Meta, Conj_Act),
    !.
prove_with_Kg(Consulta, Conj_Act, Arg_hallado, Ac, Arg, Rama) :-
    % Argumento para una lista de metas.
   Consulta = (Meta, RestoConsulta),
   prove_with_Kg(Meta, Conj_Act, Arg_hallado, Ac, NuevoAc, Rama),               % Intento probar el cuerpo de la CP.
   prove_with_Kg(RestoConsulta, Conj_Act, Arg_hallado, NuevoAc, Arg, Rama).     % Intento probar el resto de la consulta.
prove_with_Kg(Meta, Conj_Act, Arg_hallado, Ac, NuevoArg, Rama) :-
   Meta \= (_, _),                                                              % Si la meta es unica.
   \+ ciclo(Meta, Rama),                                                        % Control de ciclos.
   (
       temporary_fact(Meta, Ac),
       Regla = ninguna,
       Cuerpo = true,
       !
   ;                                                                            % o
       s_rule(Meta, Cuerpo),                                                    % busco una CPE
       Regla = s_rule(Meta, Cuerpo),
       Cuerpo \= true                                                           % que no sea un hecho
   ;                                                                            % o
       member(d_rule(Meta, Cuerpo), Arg_hallado),                               % busco una CPR en el arg. ant.
       Cuerpo \= true,                                                          % que no sea una presuposicion.
       Regla = d_rule(Meta, Cuerpo)
   ),
   prove_with_Kg(Cuerpo, Conj_Act, Arg_hallado, Ac, Arg, [Meta | Rama]),        % intento probar el cuerpo de la CP.
   guardar_regla(Regla,Arg,NuevoArg).


%------------------------------------------------------------------------------%
guardar_regla(ninguna, Arg, Arg) :-
    !.
guardar_regla(Regla, Arg, [Regla | Arg]) :-
    !.



%------------------------------------------------------------------------------%
prove_with_arg(true, _, _, Arg, Arg).
prove_with_arg(Meta, Conj_Act, _, Arg, Arg) :-
     Meta \= (_, _),
     member(Meta, Conj_Act),
     !.
prove_with_arg(Consulta, Conj_Act, Arg_hallado, Ac, Arg) :-
    % Argumento para una lista de metas.
    Consulta = (Meta, RestoConsulta),
    prove_with_arg(Meta, Conj_Act, Arg_hallado, Ac, NuevoAc),               % intento probar el cuerpo de la CP
    prove_with_arg(RestoConsulta, Conj_Act, Arg_hallado, NuevoAc, Arg).     % intento probar el resto de la consulta
prove_with_arg(Meta, Conj_Act, Arg_hallado, Ac, NuevoArg) :-
    Meta \= (_, _),                                                          % si la meta es unica
    (
        temporary_fact(Meta, Ac),
        Regla = ninguna,
        Cuerpo = true,
        !
    ;
        member(s_rule(Meta, Cuerpo), Arg_hallado),                           % busco una CPE en el arg. ant.
        Cuerpo \= true,
        Regla = s_rule(Meta, Cuerpo)
    ;                                                                        % o
        member(d_rule(Meta, Cuerpo), Arg_hallado),                           % busco una CPR en el arg. ant.
        Cuerpo \= true,                                                      % que no sea una presuposicion
        Regla = d_rule(Meta, Cuerpo)
    ),
    prove_with_arg(Cuerpo, Conj_Act, Arg_hallado, Ac,Arg),                   % intento probar el cuerpo de la CP
    guardar_regla(Regla, Arg, NuevoArg).



%------------------------------------------------------------------------------%
ciclo(Meta, Rama) :-
    esta_en_la_rama(Meta, Rama).



%------------------------------------------------------------------------------%
esta_en_la_rama(X, [Pri | _]) :- 
    instanciado(Pri), 
    X == Pri,
    !.
esta_en_la_rama(X, [Pri | _]) :- 
    \+ instanciado(Pri), 
    X = Pri,
    !.
esta_en_la_rama(X, [_ | Y]) :- 
    esta_en_la_rama(X, Y).



%------------------------------------------------------------------------------%
% Da true cuando no hay ninguna variable en todo el termino.

instanciado(T) :- 
    ground(T).
%instanciado(Termino):-
%    Termino =.. Lista,
%    \+ variable_en(Lista).



%------------------------------------------------------------------------------%
variable_en([X | _]) :- 
    var(X),
    !.
variable_en([X | _]) :- 
    \+ var(X), 
    \+ atom(X), 
    X =.. L, 
    variable_en(L), 
    !.
variable_en([_ | Y]) :- 
    variable_en(Y),
    !.



%------------------------------------------------------------------------------%
% activation_sets(+Arg,+Stack,Visited,Act_Sets)
%
% Obtains activation sets for argument Arg
% Performs a depth-first search over the possible activation sets search space
% Given an initial activation set
% 1. pop from Stack an activation set "Node"
% 2. expand new sets (Children) from "Node"
%    replacing each element of Node with the antecedent of a rule Node <-< Antecedent
% 3. push Children on Stack if no "visited" before
% 4. mark Node as "visited" whenever Node/=[] and there are non-trivial ones
% 5. continue with next in Stack

activation_sets(_, [], Act_Sets, Act_Sets). % When Stack is empty Visited becomes Act_Sets.
activation_sets(Arg, [(Node, Tipo) | Not_visited_yet], Visited, Act_Sets) :-
    largo_lista(Node, Largo),
    hijos(Arg, (Node, Tipo), [], Hijos,Largo),
    apilar_si_no_visitado(Hijos, Not_visited_yet, Pila, Visited),
    activation_sets(Arg, Pila, [(Node,Tipo) | Visited], Act_Sets).



%------------------------------------------------------------------------------%
largo_lista([], 0).
largo_lista([_ | Cola], N) :- 
    largo_lista(Cola, N1),
    N is N1 + 1.



%------------------------------------------------------------------------------%
hijos(_, _, Sale, Sale, 0).
hijos(Arg, (Conjunto, TipoPadre), Entra, Sale, Posicion) :-
    reemplazo(Posicion, Conjunto, Nuevo, Arg, TipoHijo),
    Pos is Posicion - 1,
    calcularTipo(TipoPadre, TipoHijo, Tipo),
    agregar_si_cambio(Nuevo, Conjunto, Entra, Nueva_Entra, Tipo),
    hijos(Arg, (Conjunto, TipoPadre), Nueva_Entra, Sale, Pos).



%------------------------------------------------------------------------------%
% reemplazo(+Pos,+Conjunto,-Conj_reemplazado,+Arg,-TipoRegla)
% Reemplaza el elemento de la posicion Pos del Conjunto por sus antecedentes
% del argumento.

reemplazo(1, [Pri | Resto], Nueva, Arg, Tipo) :-
    obtener_antecedentes(Arg, Pri, Ant, Tipo),
    append(Ant, Resto, Nueva),
    !.
reemplazo(1, L, L, _, _) :-
    % Si el elemento es un hecho, no hay reemplazo.
    !. 
reemplazo(Pos, [Pri | Resto], Nueva, Arg, Tipo) :-
    Pos1 is Pos - 1,
    reemplazo(Pos1, Resto, Reemplazada, Arg, Tipo),
    append([Pri], Reemplazada, Nueva).



%------------------------------------------------------------------------------%
% obtener_antecedentes(+Arg,+Pri,-Ant,-TipoRegla cpe(trivial) o cpr(notrivial))
%
% Originalmente:
%obtener_antecedentes(Arg, Pri, Ant, trivial) :- 
%    member((Ant -> Pri), Arg).
%obtener_antecedentes(Arg, Pri, Ant, notrivial) :- 
%    member((Ant >- Pri), Arg).

obtener_antecedentes(Arg, Pri, Lista_Ant, trivial) :-
    member(s_rule(Pri, Antecedente), Arg),
    Antecedente \= true,
    pasar(Antecedente, Lista_Ant).
obtener_antecedentes(Arg, Pri, Lista_Ant, notrivial) :-
    member(d_rule(Pri, Antecedente), Arg),
    Antecedente \= true,
    pasar(Antecedente, Lista_Ant).



%------------------------------------------------------------------------------%
% pasar(Antecedente, Lista literales)
% Dada una conjuncion de literales los pasa a una lista.
% Ej, a,b,c -> [a,b,c]
% Ej, true -> []

pasar(true, []) :-
    % Esto ocurre solo en el caso de suposiciones a -< true.
    !.
pasar(Literal, [Literal]) :- 
    Literal \= (_, _).
pasar((Literal, Resto), [Literal | RestoLista]) :-
    pasar(Resto, RestoLista).



%------------------------------------------------------------------------------%
% Agregar si hubo cambio, ie, Nuevo \= Conjunto.

agregar_si_cambio(Nuevo, Conjunto, Entra, [(Nuevo, Tipo) | Entra], Tipo) :-
    Conjunto \= Nuevo.
agregar_si_cambio(Nuevo, Conjunto, Entra, Entra, _) :-
    Conjunto = Nuevo.



%------------------------------------------------------------------------------%
% calcularTipo(+TipoPadre,+TipoHijo,-Tipo)

calcularTipo(trivial, trivial, trivial) :-
    !.
calcularTipo(trivial, notrivial, notrivial) :-
    !.
calcularTipo(notrivial, _, notrivial) :-
    !.



%------------------------------------------------------------------------------%
% apilar_si_no_visitado(+Hijos,+Not_visited_yet,-Pila,+Visited)
% Apila el nuevo nodo, en el caso que no haya sido visitado

apilar_si_no_visitado(Hijos, Not_visited_yet, Pila, Visited) :-
    difer(Hijos, Visited, Hijos_no_visitados),
    union(Hijos_no_visitados, Not_visited_yet, Pila),
    !.



%------------------------------------------------------------------------------%
%member(X, [X | _]) :- 
%    !.
%member(X, [_ | Y]) :- 
%    member(X,Y).



%------------------------------------------------------------------------------%
union([], L, L) :- 
    !.
union([X | C], L2, L3) :- 
    member(X, L2),
    !,
    union(C, L2, L3).
union([X | C], L2, [X | L4]) :-
    union(C, L2, L4).



%------------------------------------------------------------------------------%
difer([], _, []).
difer([X | C], L2, L3) :-
    member(X, L2),
    difer(C, L2, L3).
difer([X | C2], L2, [X | C3]) :-
    difer(C2, L2, C3).



%------------------------------------------------------------------------------%
intersec([], _, []) :-
    !.
intersec([X | C], L2, [X | L3]) :-
    member(X, L2),
    !,
    intersec(C, L2, L3).
intersec([_ | C], L2, L4) :-
    intersec(C, L2, L4).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary predicates

%------------------------------------------------------------------------------%
pause :- 
    get0(_).



%------------------------------------------------------------------------------%
show(X) :-
    traceF(yes),
    write(X),
    !.
show(_).



%------------------------------------------------------------------------------%
shownl :- 
    traceF(yes),
    nl,
    !.
shownl.



%------------------------------------------------------------------------------%
outputwait :-
    traceF(yes),
    \+ tofile(yes),
    pause,
    !.
outputwait.



%------------------------------------------------------------------------------%
showArg(arg(A, Q)) :- 
    traceF(yes), 
    write(' { '),
    nl,
    mostrarArgumento(A),
    write(' } for '),
    write(Q),
    !.
showArg(_).



%------------------------------------------------------------------------------%
mostrarArgumento([]) :-
    !.
mostrarArgumento([s_rule(H, B) | R]) :-
    write('      '),
    write((H <- B)),
    nl,
    mostrarArgumento(R),
    !.
mostrarArgumento([d_rule(H, B) | R]) :-
    write('      '),
    write((H -< B)),
    nl,
    mostrarArgumento(R),
    !.



%------------------------------------------------------------------------------%
mostrarLista([]) :-
    !.
mostrarLista([A | B]) :-
    showOnTraceW(A),
    nlj,
    mostrarLista(B),
    !.



%------------------------------------------------------------------------------%
showOnTraceW(X) :- 
    traceW(yes),
    write(X),
    !.
showOnTraceW(_).



%------------------------------------------------------------------------------%
showArgOnTrace(arg(A, Q)) :-
    traceW(yes),
    write(' { '),
    nl,
    mostrarArgumento(A),
    write(' } for '),
    write(Q),
    !.
showArgOnTrace(_).



%------------------------------------------------------------------------------%
nlj :-
    traceW(yes),
    nl,
    !.
nlj.



%------------------------------------------------------------------------------%
pausej :-
    traceW(yes),
    tofile(no),
    pause,
    !.
pausej.



%------------------------------------------------------------------------------%
mostrara(X) :- 
    trazaarg(yes),
    write(X),
    !.
mostrara(_).



%------------------------------------------------------------------------------%
nla :-
    trazaarg(yes),
    nl,
    !.
nla.



%------------------------------------------------------------------------------%
pausea :-
    trazaarg(yes),
    pause,
    !.
pausea.



%------------------------------------------------------------------------------%
showOnTraceS(X) :-
    traceSpecificity(yes),
    write(X),
    !.
showOnTraceS(_).



%------------------------------------------------------------------------------%
nlOnTraceS :- 
    traceSpecificity(yes),
    nl,
    !.
nlOnTraceS.



%------------------------------------------------------------------------------%
pauseOnTraceS :-
    traceSpecificity(yes),
    pause,
    !.
pauseOnTraceS.



%-------------------------------------------------------------------------
%  Load a DeLP program rule by rule

f :-
    loadProgram(e).



%------------------------------------------------------------------------------%
loadProgram(FileName) :-
    readProgram(FileName),
    check_SSet.
loadProgram(_) :-
    tell('load.log'),
    write('ERROR inconsistent Strict Set'),
    nl,
    told,
    !.



%------------------------------------------------------------------------------%
% Used for loading error reporting
:- dynamic lastRule/1.

%------------------------------------------------------------------------------%
readProgram(File) :-
    tell('load.log'),
    write('DeLP program successfully loaded'),
    nl,
    told,
    string_concat(File, '.delp', FileNameS),
    string_to_atom(FileNameS, FileName),
    see(FileName),                              % Read from File.
    retractall( -<(_, _)    ),
    retractall( <-(_, _)    ),
    retractall( lastRule(_) ),
    loadRules,
    seen.                                       % Close file.



%------------------------------------------------------------------------------%
loadRules :- 
    repeat,
    catch(read(Rule), _, loadErrorReport),
    loadRule(Rule),                             % Assert or write to output file an error message.
    Rule = end_of_file.



%------------------------------------------------------------------------------%
loadRule(end_of_file) :-
    !.
loadRule(R) :- 
    R = (_Head -< _Body),
    assert(R),
    lastAdd(R),
    !.
loadRule(R) :- 
    R = (_Head <- _Body), 
    assert(R),
    lastAdd(R),
    !.
loadRule(R) :- 
    write('ERROR in line: '),
    write(R),
    nl,
    nl,
    !.



%------------------------------------------------------------------------------%
% For Error Reporting. Admins the last correct rule loaded.

lastAdd(R):-
    retractall( lastRule(_) ),
    assert(     lastRule(R) ).



%------------------------------------------------------------------------------%
loadErrorReport:-
    lastRule(R),
    tell('load.log'),
    write('Syntactical Error, in the rule below: '),write(R),nl,
    told,!.



%------------------------------------------------------------------------------%
loadErrorReport:-
    tell('load.log'),
    write('Syntactical Error in the first rule'),nl,
    told,!.



%------------------------------------------------------------------------------%
% Automatic check for contradictions in strict rules and facts.
% check_SSet always succeeds.
% For SSet to being contradictory it should exist a pair of strict rules with 
% complementary heads. Therefore, if it is not contradictory the intersection 
% of heads will be empty. Thus, the test is fast in most cases.

:- dynamic contradictoryProgram/1. % Added fso the interpreter is able to deal with contradictory programs.
:- dynamic contradictorySet/1.

%------------------------------------------------------------------------------%
check_SSet :-
    retractall( contradictoryProgram(_) ),  % When the checking starts the program cannot be contradictory.
    pos_heads(P),                           % Collects Positive and Negative heads (in positive form).
    neg_heads(N),    
    intersec(N, P, I),                      % If the intersection is not empty
    I \= [],      
    deriveApair(I),                         % then try to derive a pair of contradictory literals.
    !.
check_SSet.

%------------------------------------------------------------------------------%
neg_heads(L) :- 
    findall(Head, s_rule(~Head, _), L).



%------------------------------------------------------------------------------%
pos_heads(L) :- 
    findall(Head, (s_rule(Head, _), Head \= ~_), L).



%------------------------------------------------------------------------------%
% Succeed only if a pair of complementary literals have a strict derivation.

deriveApair([H | _]) :-
    strict_derivation( H, [], strict),
    strict_derivation(~H, [], strict),
    write('WARNING! the subset of strict rules of your program is contradictory '),
    nl,
    write('Literals "'),
    write(H),
    write('" and "'),
    write(~H),
    write('" have a strict derivation'),
    nl,
    asserta( contradictoryProgram(H) ). % The program derives strictly H and ~H.
deriveApair([_ | L]) :- 
    deriveApair(L).



%-------------------------------------------------------------------------
% Output to a file Special for LPA Win-Prolog

startFile(trace) :- 
    tmpFileName(Name),
    fcreate(trace, Name, 0),
    output(trace).



%-------------------------------------------------------------------------
tmpFileName(X) :-
    ticks(T),
    number_atom(T, A),
    cat([tmpDeLP, A], X, _).



%-------------------------------------------------------------------------
endFile(F, Q, A) :-
    nl,
    write('Answer to '),
    write(Q),
    write(' is '),
    write(A),
    nl,
    fclose(F).



%-------------------------------------------------------------------------
% Shortcut for testing the interpreter
go(Q) :- 
    startFile(F),
    answer(Q, A),
    endFile(F, Q, A). % Output to file tmpDeLP

% EOF: delp.pl

