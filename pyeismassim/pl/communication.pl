:- dynamic valorDeMeta/2.

communicateAndResolveConflicts(MyAction, _NewAction) :-
    intention(Intention),
    write('    Comm: Broadcasting intention:'),write(Intention), write(' and action: '), writeln(MyAction),
    broadcast(d3lp0r, mapc, [Intention, MyAction]),
    writeln('    Comm: Receiving teammate action list'),
    recibirTodoSimple(GoalActionList, 1),
    writeln('    Comm: Setting priorities'),
    phase(Phase),
    setPriorities(Phase),
    write('    Comm: Solving conflicts with '),writeln(GoalActionList),
    solveConflicts(GoalActionList),
    writeln('    Comm: Done :D').

setPriorities(exploracion) :-
    retractall(valorDeMeta(_,_)),
    assert(valorDeMeta(1,  probear      )),
    assert(valorDeMeta(2,  explorar     )),
    assert(valorDeMeta(3,  aumento      )),
    assert(valorDeMeta(4,  expandirse   )),
    assert(valorDeMeta(5,  atacar       )),
    assert(valorDeMeta(6,  reparar      )),
    assert(valorDeMeta(7,  inspectar    )),
    assert(valorDeMeta(8,  bloquear     )),
    assert(valorDeMeta(9,  defender     )),
    assert(valorDeMeta(10, romperzonas  )).

setPriorities(expansion) :-
    retractall(valorDeMeta(_,_)),
    assert(valorDeMeta(1,  aumento      )),
    assert(valorDeMeta(2,  expandirse   )),
    assert(valorDeMeta(3,  romperzonas  )),
    assert(valorDeMeta(4,  defender     )),
    assert(valorDeMeta(5,  probear      )),
    assert(valorDeMeta(6,  explorar     )),
    assert(valorDeMeta(7,  atacar       )),
    assert(valorDeMeta(8,  reparar      )),
    assert(valorDeMeta(9,  inspectar    )),
    assert(valorDeMeta(10, bloquear     )).

solveConflicts([]) :- 
    writeln('End of GoalActionList.').

solveConflicts([[Goal, Action] | T]) :-
    write("Goal is "), write(Goal), write(" and Action is "), write(Action),nl,
    solveConflicts(T).


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
    % si la accion que se cancela es la mia, hay que devolver que sera recharge
    % o algo mejor pero que no entre conflicto

    % todo: hacer que se broadcastee la meta del agente
    % todo: asertar la informacion que viene en el sim start para decidir los
    % cambios de fases
    % todo: asegurarse que la info de los agentes enemigoss se aserta
    % todo: hacer que se comparta el health y max health energy max energy

