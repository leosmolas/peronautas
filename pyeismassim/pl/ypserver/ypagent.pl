:-consult('primitivas/ypa.pl').
:-consult('lib.pl').
:-set_ypa_address(localhost,8000). % get_ypa_address(Host,Port)

cartelito :-    nl,nl,
    writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
    writeln('%%                        Agente Yellow Pages                                %%'),
    writeln('%%                           Localhost:8000                                  %%'),
    writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
    nl,nl,
    write('>>   '), writeln('conectar              (+Nombre)'),
    write('>>   '), writeln('enviar                (+Receptor, +Data)'),
    write('>>   '), writeln('broadcast             (+Caracteristica, +Ontologia, +Data)'),
    write('>>   '), writeln('recibir               (-Emisor, -Receptor, -Datos)'),
    write('>>   '), writeln('recibir               (-Emisor, -Receptor, -Datos, +TimeOut)'),
    write('>>   '), writeln('recibirTodo           (-ListaDeMensajes, +TimeOut)'),
    write('>>   '), writeln('registrar             (+ListaDeCaracteristicas, +Ontolgia)'),
    write('>>   '), writeln('registrar             (+Caracteristica, +Ontologia)'),
    write('>>   '), writeln('which_agents          (-ListaDeAgentes, +Caracteristica, +Ontologia, -Error)'),
    write('>>   '), writeln('which_characteristics (-ListaDeCaracteristicasTotales, +Ontologia, -Error)'),
    write('>>   '), writeln('deregister            (+Caracteristicas, +Ontologia, -Error)'),
    nl,writeln('llamar conectar, registrar.'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: conectar/1 
%abstrae: connect/3
%template: conectar(+Nombre)
%descripcion: conecta al agente al servidor de paginas amarillas.

conectar(Nombre) :- 
    connect(Nombre, Error), 
    writeln(Error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: enviar/2 
%abstrae: send/1 template:send([receiver(+Receptor),content(+Data)])
%template: enviar(+Receptor, +Data) o enviar(+ListaDeReceptor, +Data)
%descripcion: envia un dato a un receptor en particular.

%para implementar fipa-acl bien deberiamos tener dentro de content el tipo de acto comunicativo q estamos teniendo

enviar(Receptor, Data) :-
    send([receiver(Receptor), content(Data)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: broadcast/1 
%abstrae: send/1 template:send([receiver(+Receptor),content(+Data)])
%template: enviar(+Data)
%descripcion: envia un dato a todos los agentes registrados en el servidor de paginas amarillas.

broadcast(Caracteristica, Ontolgia, Data) :-
    which_agents(ListofAgents, Caracteristica, Ontolgia, _Error), 
    ip:name(Name),
    delete(ListofAgents, Name, ListofAgentsOutMe),
    send([receiver(ListofAgentsOutMe), content(Data)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: recibir/3 
%abstrae: recv/1 template:recv([sender(-Sender), receiver(-Receiver), content(-Content)]).
%template: recibir(-Emisor, -Receptor, -Data)
%descripcion: recibe un mensaje dejado en el servidor. Si no tiene nada cuelga esperando que le llegue.

recibir(Emisor, Receptor, Datos) :-
    recv(Data), 
    member(content(Datos),Data),
    member(receiver(Receptor),Data),
    member(sender(Emisor),Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: recibir/4 
%abstrae: recv/2 template:recv([sender(-Sender), receiver(-Receiver), content(-Content)], +TimeOut).
%template: recibir(-Emisor, -Receptor, -Data, +TimeOut)
%descripcion: recibe un mensage que le llegue, esperando el tiempo dado por +TimeOut. Luego de eso sigue.

recibir(Emisor, Receptor, Datos, TimeOut) :-
    recv(Data, TimeOut), 
    member(content(Datos),Data),
    member(receiver(Receptor),Data),
    member(sender(Emisor),Data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: recibirTodo/4 
%abstrae:
%template: recibirTodo(-ListaDeMensajes, +TimeOut)
%descripcion: recibe todos los mensajes que se encuentran en en servidor, esperando el tiempo dado por +TimeOut. Luego de eso sigue.

%ejemplodeuso:  recibirTodo(D, 0.0000000000001).

recibirTodo(ListaDeMensajes, TimeOut):-
    recv(Message,TimeOut),
    (
        Message = timeout,
        ListaDeMensajes = []
    ;
        is_list(Message),
        recibirTodo(ListaDeMensajesNuevos, TimeOut),
        append(ListaDeMensajesNuevos, [Message], ListaDeMensajes)
    ),
    writeln(ListaDeMensajes).

functor2list(A,    A) :-
    atom(A),
    !.
functor2list(Term, [Name, Argument]) :- 
    functor(Term, Name, 1),
    arg(1, Term, Argument).

recibirTodoSimple(ListaDeMensajes, TimeOut):-
    recv(Message,TimeOut),
    (
        Message = timeout,
        ListaDeMensajes = []
    ;
        (
            is_list(Message),
            recibirTodo(ListaDeMensajesNuevos, TimeOut),
            member(content(Datos), Message),
            member(sender(Emisor), Message),
            (
                functor2list(Datos, [Action, Parameter]),
                append(ListaDeMensajesNuevos, [[Emisor, Action, Parameter]], ListaDeMensajes)
            ;
                functor2list(Datos, Action),
                append(ListaDeMensajesNuevos, [[Emisor, Action]], ListaDeMensajes)
            )
        )
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: registrar/2 
%abstrae: register/3
%template: registrar(+ListofCaracteristics, +Ontolgy) o registrar(+Caracteristic, +Ontolgy)
%descripcion: registra al agente con ciertas caracteristicas y una ontologia en el servidor.

registrar(Caracteristics, Ontology) :-
    register(Caracteristics, Ontology, Error), 
    writeln(Error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: which_agents/4 
%abstrae:
%template: which_agents(-List_Of_Agents, +Characteristic, +Ontology, -Error)
%descripcion: busca los nombres de los agentes con cierta caracteristica y ontologia.

%implementado en ypa.pl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: which_characteristics/3
%abstrae:
%template: which_characteristics(-List_Of_MASs, +Ontology, -Error)
%descripcion: retorna todos los tipos de caracteristicas de los agentes registrados bajo una cierta ontologia.

%implementado en ypa.pl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%metodo: deregister/3
%abstrae:
%template: deregister(+Characteristics, +Ontology, -Error)
%descripcion: elimina el conjunto de caracteristicas con las cuales un agente fue registradas.

%implementado en ypa.pl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%predicados auxiliares pq sino no anda. no vi todavia pq se necesitan.
connected(Agent) :-
    write(connected(Agent)),nl.
    
disconnected(Agent) :-
    write(disconnected(Agent)),nl.
    
