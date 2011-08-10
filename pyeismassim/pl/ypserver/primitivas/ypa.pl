/*  $Id$

    Module for SWI-Prolog

    Author:  Mariano Tucat
    E-mail:  mt@cs.uns.edu.ar
    WWW:     http://cs.uns.edu.ar/~mt

*/

:- module(ypa,
	  [ connect/2,
	    disconnect/1,
	    register/3,
	    deregister/3,
	    which_agents/4,
	    which_characteristics/3,
	    send/1,
	    recv/1,
	    nrecv/1,
	    recv/2,
	    bind/2,
	    unbind/1,
	    get_ypa_address/2,
	    set_ypa_address/2
	  ]).

:- [interface].

:- dynamic address/2.

% connect(+Host, +Port, -Error)
% 
% connects the agent to a existent YPA
connect(Name, Error) :-
    writeln(1),
	get_ypa_address(YPAHost,YPAPort),	
    writeln(2),
	set_name(Name),
    writeln(3),
	connect_agent(ypa,YPAHost,YPAPort),
    writeln(4),
	gethostname(Host),
    writeln(5),
	start_server(Port),
    writeln(6),
	send_msg([receiver(ypa),performative(request),ontology(ypa),protocol(connect),
    writeln(7),
		  content(host(Host,Port))]),
    writeln(8),
	recv_msg([sender(ypa),performative(Answer),ontology(ypa),protocol(connect)],1000),
    writeln(9),
	(   Answer = agree,
	    Error = no_error
	;   Answer = refuse,
    writeln(12),
	    write(refuse),nl,
    writeln(13),
%	    member(Error,R),
	    disconnect_agent(ypa),
    writeln(1),
	    close_server
	).

set_ypa_address(Host,Port) :-
	retractall(address(_Host,_Port)),
	assert(address(Host,Port)).

get_ypa_address(Host,Port) :-
	address(Host,Port).

disconnect(no_error) :-
	disconnect_agent(ypa).

% register(+Characteristics, +Ontology, -Error)
% 
register([], _Ontology, no_error).
register([Ch|Tail], Ontology, E) :-
	register_characteristic(Ch,Ontology),
	register(Tail,Ontology,E).
register(Characteristic, Ontology, no_error) :-
	register_characteristic(Characteristic,Ontology).

register_characteristic(Characteristic,Ontology) :-
	send_msg([receiver(ypa),performative(request),ontology(ypa),protocol(register),
		  content(register(Characteristic,Ontology))]),
	recv_msg([sender(ypa),performative(Answer),ontology(ypa),protocol(register)|R]),
	(   Answer = agree
	;   Answer = refuse,
	    member(_Error,R)
	).


% deregister(+Characteristics, +Ontology, -Error)
% 
deregister([], _Ontology, no_error).
deregister([Ch|Tail], Ontology, E) :-
	deregister_characteristic(Ch,Ontology),
	deregister(Tail,Ontology,E).
deregister(Characteristic, Ontology, no_error) :-
	deregister_characteristic(Characteristic,Ontology).

deregister_characteristic(Characteristic, Ontology) :-
	send_msg([receiver(ypa),performative(request),ontology(ypa),protocol(deregister),
		  content(deregister(Characteristic,Ontology))]),
	recv_msg([sender(ypa),performative(Answer),ontology(ypa),protocol(deregister)|R]),
	(   Answer = agree
	;   Answer = refuse,
	    member(_Error,R)
	).

% which_characteristics(-List_Of_MASs, +Ontology, -Error)
% 
which_characteristics(List_Of_MASs, Ontology, Error) :-
	send_msg([receiver(ypa),performative(request),ontology(ypa),protocol(registered_ontologies),
		  content(registered(Ontology))]),
	recv_msg([sender(ypa),performative(Answer),ontology(ypa),protocol(registered_ontologies),
		  content(ontologies(List_Of_MASs))]),
	(   Answer = agree,
	    Error = no_error
	;   Answer = refuse,
	    Error = unknown
	).

% which_agents(-List_Of_Agents, +Characteristics, +Ontology, -Error)
% 
which_agents(List_Of_Agents, Characteristics, Ontology, no_error) :-
	send_msg([receiver(ypa),performative(request),ontology(ypa),protocol(registered_agents),
		  content(registered(Characteristics,Ontology))]),
	recv_msg([sender(ypa),performative(Answer),ontology(ypa),protocol(registered_agents),
		  content(agents(List_Of_Agents))]),
	(   Answer = agree,
	    Error = no_error
	;   Answer = refuse,
	    Error = unknown
	).

% agent_location(+Agents, -Host, -Port)
% 
agent_location(Agent,Host,Port):-
	send_msg([receiver(ypa),
		  performative(request),
		  ontology(ypa),
		  protocol(agent_location),
		  content(agent(Agent))
		 ]),
	recv_msg([sender(ypa),
		  performative(Answer),
		  ontology(ypa),
		  protocol(agent_location),
		  content(Content)
		 ]),
	(   Answer = agree,
	    Content = agent(Agent,Host,Port)
	;   Answer = refuse,
	    !,fail
	).
	
	
send(Msg) :-
	send_msg(Msg),!.

send(Msg) :-
	member(receiver(List),Msg),
	is_list(List),!,
	msend(Msg).

send(Msg) :-
	member(receiver(Agent),Msg),!,
	agent_location(Agent,Host,Port),
	connect_agent(Agent,Host,Port),!,
	send_msg(Msg).

msend(Msg):-
	member(receiver([]),Msg),!.
msend(Msg):-
	delete(Msg,receiver([Head|Tail]),NMsg),
	append(NMsg,[receiver(Head)],SMsg),
	send(SMsg),
	append(NMsg,[receiver(Tail)],MMsg),
	msend(MMsg).

recv(Msg) :-
	recv_msg(Msg).

nrecv(Msg) :-
	nrecv_msg(Msg).

recv(Msg,Time) :-
	recv_msg(Msg,Time).

bind(Msg,Pred) :-
	bind_msg(Msg,Pred).

unbind(Msg) :-
	unbind_msg(Msg).
