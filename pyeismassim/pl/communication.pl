:- dynamic valorDeMeta/2.

communicateAndResolveConflicts(MyAction, _NewAction) :-
    intention(Intention),
    write(  '    Comm: Broadcasting intention:'),write(Intention), write(' and action: '), writeln(MyAction),
    broadcast(d3lp0r, mapc, [Intention, MyAction]),
    writeln('    Comm: Receiving teammate action list'),
    recibirTodoSimple(IntentionActionList, 1),
    writeln('    Comm: Setting priorities'),
    phase(Phase),
    setPriorities(Phase),                
    writeln('    Comm: Ordering ActionList using priorities... original IntentionActionList is:'),writeln(IntentionActionList),
    sort(IntentionActionList, OrderedIAList),
    write(  '    Comm: Solving conflicts with '),writeln(OrderedIAList),
    solveConflicts(IntentionActionList, MyNewAction),
    write(  '    Comm: Done resolving conflicts; my action is now '), writeln(MyNewAction).

setPriorities(exploracion) :-
    retractall(valorDeMeta(_,_)),
    assert(valorDeMeta(0,  bloquear     )),
    assert(valorDeMeta(1,  probear      )),
    assert(valorDeMeta(2,  explorar     )),
    assert(valorDeMeta(3,  aumento      )),
    assert(valorDeMeta(4,  expansion    )),
    assert(valorDeMeta(5,  atacar       )),
    assert(valorDeMeta(6,  reparar      )),
    assert(valorDeMeta(7,  defender     )),
    assert(valorDeMeta(8,  auxilio      )),
    assert(valorDeMeta(9,  inspectar    )),
    assert(valorDeMeta(11, romperzonas  )),
    assert(valorDeMeta(12, quedarse     )).

solveConflicts([], NewAction) :- 
    writeln('End of IntentionActionList.').

solveConflicts([[Intention, Action, Agent] | T], NewAction) :-
    writeln('Checking member...'),
    write('Intention is...'), writeln(Intention),
    write('Action is...'), writeln(Action),
    write('Agent is...'), writeln(Agent),
    member([Intention, OtherAction, OtherAgent], T),
    (
        Intention = probear(X)
        ;
        Intention = reparar(X)
        ;
        Intention = inspectar(X)
    ),
    writeln('    Comm: two agents are trying to do the same!'),
    solveConflicts(T, NewAction).

solveConflicts([[Intention, Action, Agent] | T], NewAction) :-
    solveConflicts(T, NewAction).


    % ordenar la lista de acciones de acuerdo al orden de prioridad, que va a depender de la fase del juego
    % el orden de prioridad va a tomar en consideracion las metas de cada agente
    % si la meta es igual, se usara orden lexicografico
    %
    % prioridad de metas para cada fase:
    % metas: 
    %   probear
    %   explorar (survey)
    %   aumento
    %   expandirse
    %   atacar
    %   reparar
    %   inspectar
    %   bloquear
    %   defender
    %   romperzonas
    %
    % exploracion
    %   probear
    %   bloquear
    %   reparar
    %   inspectar
    %   explorar (survey)
    %   aumento
    %   expandirse
    %   romperzonas
    %   defender
    %   atacar
    %
    % establecimiento
    %   bloquear
    %   posicionar
    %       
    %   probear
    %   explorar (survey)
    %   aumento
    %   expandirse
    %   atacar
    %   reparar
    %   inspectar
    %   defender
    %   romperzonas
    %
    % expansion
    %   aumento
    %   expandirse
    %   romperzonas
    %   defender
    %   probear
    %   explorar (survey)
    %   atacar
    %   reparar
    %   inspectar
    %   bloquear
    %       
    % para cada accion en la lista ordenada, si la accion es potencialmente
    % dañina, te fijas el mundo hipotetico a partir de esa accion
    % acciones potencialmente dañinas son moverse, 
    % si la accion no es dañina, es probe, inspect, survey, o repair, cheqeuar
    % que no haya conflicto. 
    %
    % por cada accion que pasa el checqueo, guardas cuantos puntos da
    % si la accion es efectivamente dañina o sea reduce en 10% puntos o esta en
    % conflicto, no se hace y se pasa a la accion siguiente considerando que la
    % accion que se realizara sera recharge
    %
    % por cada accion que pasa el checqueo, guardas cuantos puntos da
    % si la accion es efectivamente dañina o sea reduce en 10% puntos o esta en
    % conflicto, no se hace y se pasa a la accion siguiente considerando que la
    % accion que se realizara sera recharge
    % si la accion que se cancela es la mia, hay que devolver que sera recharge
    % o algo mejor pero que no entre conflicto

    % todo: hacer que se broadcastee la meta del agente
