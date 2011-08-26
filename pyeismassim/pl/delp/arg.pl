:- dynamic mejorMeta/2.

:- [interpreter/delp]. % interprete

:- consult('arg.delp'). % reglas de argumentacion
   % consult('mundo2.delp'). % hechos asertados en una situacion del mundo particular

% criterios de comparacion

% greaterArgValue esta definido mas abajo, y lo que hace es buscar los argValue de los argumentos, si es que existen, para compararlos
:- % comparison_on(greaterArgValue),
% defeater2assumption es un criterio que establece derrota cuando uno de los argumentos es una asuncion, es derrotado por cualquier otra cosa que tenga algo.
   comparison_on(defeater2assumption),
% more_specific es la espeficidad de siempre.
   comparison_on(more_specific).

doNotFail(X) :-
    call(X), !.

doNotFail(_).
   
% PREDICADO IMPORTANTE
% meta(+Meta)
% Calcula la mejor meta con el mundo actual.
% Realiza todo el ciclo de argumentacion, teniendo previamente todo lo que necesita asertado.
   
meta(X) :-     
    assert(mejorMeta(_, -1000)), % meta con "menos infinito"
    % foreach(b(posibleExpansion(N)), doNotFail(calcMeta(expansion(N)))),
    foreach(b(posibleExplorar(N)), doNotFail(calcMeta(explorar(N)))),
    % foreach(b(posibleAumento(N)), doNotFail(calcMeta(aumento(N)))),
    currentStep(Step),
    myName(Name),
    position(Step, Name, Position),
    doNotFail(calcMeta(quedarse(Position))),
    rolMetas, % predicado definido en cada rol
    % % foreach(posibleProbear(N), doNotFail(calcMeta(probear(N)))),
    mejorMeta(X, _),
    retract(mejorMeta(_, _)).
    
calcMeta(X) :-
    writeln(X),
    X =.. [Meta, Nodo | _],
    Query =.. [Meta, Value, Nodo],
    answer(Query, Answer),
    writeln(Answer), 
    Answer = yes, !,
    writeln(Value),
    mejorMeta(_, CurrentValue), !,
    Value > CurrentValue,
    retract(mejorMeta(_, CurrentValue)),
    assert(mejorMeta(X, Value)).

% todos los predicados que siguen son operaciones aritmeticas y de comparacion, para que los use delp.
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
is_a_built_in(explorarValue(_Dist, _Positivo, _Negativo, _Value)).
is_a_built_in(expansionValue(_Dist2, _DifPuntos2, _Value2)).
is_a_built_in(aumentoValue(_Dist3,  _DifPuntos3, _Value3) ).

% Operaciones aritmeticas
mult(X,Y,Z)    :- Z is X * Y.
add(X,Y,Z)     :- Z is X + Y.
sust(X,Y,Z)    :- Z is X - Y.
power(X,Y,Z)   :- Z is X ** Y.
greater(X,Y)   :- X > Y.
less(X,Y)      :- X < Y.
greaterEq(X,Y) :- X >= Y.
lessEq(X,Y)    :- X =< Y.
equal(X,Y)     :- X =:= Y. % este es el igual, pero no instancia, solo chequea igualdad. (O sea, no es el mismo que el =, que si instancia.)
notEqual(X,Y)  :- X \= Y.


% posibleMeta(+Meta, -Prioridad)
% La Meta tendra una prioridad unica, que le dara su orden de importancia
% Valor mas alto => mayor prioridad.
posibleMeta(explorar(_), 2).
posibleMeta(expansion(_), 1).

% posibleMetaNeg(+Meta)
% Chequea que el parametro ingresado sea una meta, asi este negada.
posibleMetaNeg(explorar(_)).
posibleMetaNeg(expansion(_)).
posibleMetaNeg(~explorar(_)).
posibleMetaNeg(~expansion(_)).

% criterio de comparacion greaterArgValue
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
    
    % posibleMetaNeg(HeadB), % con esto checkeo que solo entren las posibles metas
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
    % ArgAs @< ArgBs. % esta es la comparacion de terminos.
 

% equalArgValues(HeadA, HeadB) :- % Son metas distintas. Veo cual esta primero en el orden de prioridad
    % % write('hola'), % Quinto año de la carrera y write 'hola' (diria el vasco :P)
    % % HeadA =.. [MetaA | ArgAs],
    % % HeadB =.. [MetaB | ArgBs],
    % posibleMeta(HeadA, ValueA),
    % posibleMeta(HeadB, ValueB),
    
    % ValueA > ValueB.
