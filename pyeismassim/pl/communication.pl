communicateAndResolveConflicts(MyGoal, MyAction, NewAction) :-
    broadcast(d3lp0r, mapc, [MyGoal, MyAction]),
    recibirTodoSimple(GoalActionList, 1),

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

