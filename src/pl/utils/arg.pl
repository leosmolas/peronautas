:- dynamic mejorMeta/2.

:- [utils/utilisima]. % interprete

:- consult('arg.delp'). % reglas de argumentación

% criterios de comparación

% greaterArgValue busca los argValue de los argumentos, si es que existen, para compararlos
:- % comparison_on(greaterArgValue),

% defeater2assumption establece: cuando uno de los argumentos es una asunción, es derrotado por cualquier otro que tenga razones a favor.
   comparison_on(defeater2assumption),
   
% more_specific es la espeficidad de siempre.
   comparison_on(more_specific).

doNotFail(X) :-
    call(X), !.

doNotFail(_).
   
% PREDICADO IMPORTANTE
% meta(+Meta)
% Calcula la mejor meta con el mundo actual.
% Realiza todo el ciclo de argumentación, teniendo previamente todo lo que necesita asertado.
   
meta(X) :- 
    retractall(mejorMeta(_, _)),
    assert(mejorMeta(_, -1000)), !, % meta con "menos infinito"
    % foreach(b(posibleExpansion(N )), doNotFail(calcMeta(expansion(N )))), !,
    foreach(b(posibleExplorar( N1)), doNotFail(calcMeta(explorar( N1)))), !,
    foreach(b(posibleAumento(  N2)), doNotFail(calcMeta(aumento(  N2)))), !, 
    foreach(b(posibleAuxilio(  N3)), doNotFail(calcMeta(auxilio(  N3)))), !,
    
    myPosition(Position),
	doNotFail(calcMeta(reagruparse)),
    doNotFail(calcMeta(defensaPropia(Position))),
    foreach(
        k(edge(Position, Neigh, _)),
        doNotFail(calcMeta(defensaPropia(Neigh)))
    ),
	foreach(
        member(X, [shield, sabotageDevice, sensor]),
        doNotFail(calcMeta(comprar(X)))
    ),
    doNotFail(calcMeta(quedarse(Position))), !,
    
    rolMetas, % predicado definido en cada rol
    
    mejorMeta(X, _).
    
calcMeta(X) :-
    X =.. [Meta | Args],
    Query =.. [Meta, Value | Args],
    answer(Query, Answer),
    % writeln(Answer), 
    Answer = yes, !,
    writeln(X),
    writeln(Value),
    mejorMeta(_, CurrentValue), !,
    Value > CurrentValue,
    retract(mejorMeta(_, CurrentValue)),
    assert(mejorMeta(X, Value)).
    

% predicados de operaciones aritméticas, comparación, y otros, declarados como
% built-in para ser utilizados directamente por delp.
is_a_built_in(mult(_X,_Y,_Z)).
is_a_built_in(add(_X,_Y,_Z)).
is_a_built_in(sust(_X,_Y,_Z)).
is_a_built_in(power(_X,_Y,_Z)).
is_a_built_in(greater(_X,_Y)).
is_a_built_in(less(_X,_Y)).
is_a_built_in(greaterEq(_X,_Y)).
is_a_built_in(lessEq(_X,_Y)).
is_a_built_in(equal(_X,_Y)).
is_a_built_in(notEqual(_X,_Y)).

is_a_built_in(phase(_)). 
is_a_built_in(role(_, _)).
is_a_built_in(position(_, _)).
is_a_built_in(myMaxHealth(_)).
is_a_built_in(myHealth(_)).
is_a_built_in(myRole(_)).
is_a_built_in(myEnergy(_)).
is_a_built_in(myPosition(_)).
is_a_built_in(myStatus(_)).
is_a_built_in(myTeam(_)).
is_a_built_in(myName(_)).
is_a_built_in(b(_)).
is_a_built_in(mePegaron).
is_a_built_in(money(_)).
is_a_built_in(currentStep(_)).
is_a_built_in(buyCount(_, _)).
is_a_built_in(agenteEnZona(_Agent)).

position(Agent, Position) :-
    currentStep(Step),
    position(Step, Agent, Position).
    
agenteEnZona(Agent):-
    currentStep(Step),
    myTeam(MyTeam),
    agenteEnZona(Step, Agent, MyTeam).

% Para imprimir en delp.
is_a_built_in(w(_)).

w(X) :- writeln(X).

% Operaciones aritméticas
% DEPRECATED
% Conviene usar una sola fórmula para las metas
mult(X,Y,Z)    :- Z is X *  Y.
add(X,Y,Z)     :- Z is X +  Y.
sust(X,Y,Z)    :- Z is X -  Y.
power(X,Y,Z)   :- Z is X ** Y.
div(X,Y,Z)     :- Z is X / Y.
greater(X,Y)   :- X >   Y.
less(X,Y)      :- X <   Y.
greaterEq(X,Y) :- X >=  Y.
lessEq(X,Y)    :- X =<  Y.
equal(X,Y)     :- X =:= Y. % este es el igual, pero no instancia, sólo chequea igualdad. (O sea, no es el mismo que el =, que si instancia.)
notEqual(X,Y)  :- X \=  Y.

% Por cuestiones históricas y emocionales, todo lo que sigue va a quedar. (El criterio de comparación).

% posibleMeta(+Meta, -Prioridad)
% La Meta tendrá una prioridad única, que le dará su orden de importancia
% Valor más alto => mayor prioridad.
% posibleMeta(explorar(_), 2).
% posibleMeta(expansion(_), 1).

% posibleMetaNeg(+Meta)
% Chequea que el parámetro ingresado sea una meta, así esté negada.
% posibleMetaNeg(explorar(_)).
% posibleMetaNeg(expansion(_)).
% posibleMetaNeg(~explorar(_)).
% posibleMetaNeg(~expansion(_)).

% criterio de comparación greaterArgValue
% greaterArgValue(arg([Ac | Acs], _), arg([Bc | Bcs], _)) :-
    % (
        % Ac = s_rule(HeadA, _)
        % ;
        % Ac = d_rule(HeadA, _)
    % ), !,
    % posibleMetaNeg(HeadA), 
    % (
        % Bc = s_rule(HeadB, _)
        % ;
        % Bc = d_rule(HeadB, _)
    % ), !,
    % % member(HeadA, [explorar(_), expansion(_), ~explorar(_), ~expansion(_)]),
    % % member(HeadB, [explorar(_), expansion(_), ~explorar(_), ~expansion(_)]), 
    
    % posibleMetaNeg(HeadB), % con esto checkeo que sólo entren las posibles metas
    % % writeln([Ac | Acs]),
    % % writeln([Bc | Bcs]),
    % member(s_rule(argValue(ValA),true), Acs), !,
    % % writeln(ValA),
    % member(s_rule(argValue(ValB),true), Bcs), !,
    % % writeln(ValB),
    % % writeln('greater'),
    % resolveConflict(ValA, ValB, [Ac | Acs], [Bc | Bcs]).
    
% % resolveConflict(+ValA, +ValB, +Ac, +Bc)
% % Los Vals son los argValues de los argumentos.
% % Los otros son las secuencias argumentativas
% resolveConflict(ValA, ValB, _, _) :-
    % ValA > ValB, !.
    
% resolveConflict(Val, Val, Ac, Bc) :- % este predicado toma las metas y con eso llama a equalArgValues
    % member(d_rule(HeadA, _), Ac),
    % posibleMeta(HeadA, _), !,
    % member(d_rule(HeadB, _), Bc),
    % posibleMeta(HeadB, _), !,
    % equalArgValues(HeadA, HeadB).
     
% equalArgValues(HeadA, HeadB) :- % Son la misma meta. Comparo por los argumentos
    % HeadA =.. [Meta | ArgAs],
    % HeadB =.. [Meta | ArgBs],
    % ArgAs @< ArgBs. % esta es la comparación de términos. 

% equalArgValues(HeadA, HeadB) :- % Son metas distintas. Veo cuál está primero en el orden de prioridad
    % % write('hola'), % Quinto año de la carrera y write 'hola' (diría el vasco :P)
    % % HeadA =.. [MetaA | ArgAs],
    % % HeadB =.. [MetaB | ArgBs],
    % posibleMeta(HeadA, ValueA),
    % posibleMeta(HeadB, ValueB),    
    % ValueA > ValueB.
