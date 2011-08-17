/*  $Id$

    Module for SWI-Prolog

    Author:  Mariano Tucat
    E-mail:  mt@cs.uns.edu.ar
    WWW:     http://cs.uns.edu.ar/~mt

*/


:- module(ip,
	  [ start_server/1,
	    close_server/0,
	    quit/0,
	    connect_agent/3,
	    disconnect_agent/1,
	    disconnect_all/0,
	    send_msg/1,
	    recv_msg/1,
	    nrecv_msg/1,
	    recv_msg/2,
	    bind_msg/2,
	    unbind_msg/1,
	    which_agents/1,
	    set_name/1
	  ]).

:- use_module(library(streampool)).


:- dynamic(stream/4).          %stream(Name,ReadStream,WriteStream,Socket)
                               %keeps track of the connected agents

:- dynamic(msgs/1).	           %msgs(Message)
                               %represent the queued messages

:- dynamic(waiting/2).	       %waiting(Thread,msgs(Msg))
                               %represents the blocked threads waiting for a specific message

:- dynamic(binds/2).	       %binds(Pattern,Predicate)
                               %holds the associations made to the arrival of specific messages and
                               %the call of determined predicates

:- dynamic(server_id/2).       %server_id(Socket,In)
			       %holds the server information

:- dynamic(streampool/0).      %streampool
			       %determines whether the stream pool loop is already created or not

:- dynamic(messages_queue/2).  %messages_queue(Thread_Id,messages)
                               %keeps the Id of the thread handling the message queue

:- dynamic(binds_queue/2).     %binds_queue(Thread_Id_List,binds_queue)
                               %keeps the Ids of the thread handling the binds and the name of the queue

:- dynamic(name/1).            %name(Name)
			       %holds the name of the agent


%the number of threads that will be created to handle the binds
number_of_threads(5).

%:- dynamic(disconnected(Name)).
% This predicate will be called whenever an agents is disconnected, and
% may be implemented when using this library.

%:- dynamic(connected(Name)).
% This predicate will be called whenever an agent connects to the server
% and may be implemented by the server agent in order to act accordingly




% start_server(+Port)
%
% starts a server in a given Port
% each connection adds the corresponding stream to the streampool
%
start_server(_Port) :-
	server_id(_Socket,_Stream),
	write('Existent Server'),nl.

start_server(Port) :-
        tcp_socket(Socket),
        tcp_bind(Socket, Port),
        tcp_listen(Socket, 5),
	tcp_open_socket(Socket,In,_Out),
	add_stream_to_pool(In,listen(Socket)),
	assert(server_id(Socket,In)),
	create_stream_pool_loop,
	create_messages_handler,
	number_of_threads(Threads),
	create_binds_handler(Threads).

% close_server/0
%
% Stops listening for incomming connections.
%
close_server :-
	retract(server_id(Socket, Stream)),
	delete_stream_from_pool(Stream),
	tcp_close_socket(Socket).

% quit/0
%
% Stops listening for connections and closes all the existent
% connections.
%
quit :-
	close_server,
	disconnect_all.


% connect_agent(+Name, +Host, +Port)
%
% starts a tcp connection with the corresponding Host:Port
% assigning the corresponding Name
%
connect_agent(Name, Host, Port) :-
	tcp_socket(Socket),
	catch(socket:tcp_connect(Socket, Host:Port),_,fail),
	create_messages_handler,
	tcp_open_socket(Socket, ReadFd, WriteFd),
	add_stream_to_pool(ReadFd,handle_service(ReadFd)),
	create_stream_pool_loop,
	asserta(stream(Name, ReadFd, WriteFd, Socket)),
	number_of_threads(Threads),
	create_binds_handler(Threads).


% disconnect_agent(+Name)
%
% close the corresponding connection
%
disconnect_agent(Name) :-
	retract(stream(Name, ReadFd, WriteFd, Socket)),
	delete_stream_from_pool(ReadFd),
	close(WriteFd),
	tcp_close_socket(Socket).

% disconnect_all/0
%
% closes all the existent connections
%
disconnect_all:-
	which_agents([]).

disconnect_all:-
	which_agents([A|_R]),
	disconnect_agent(A),
	disconnect_all.


% send_msg(+Name,+Msg)
%
% sends a message to the specifyied agent (server or client)
%
send_msg(Msg) :-
	member(receiver(Name),Msg),!,
        stream(Name, _ReadFd, WriteFd, _Socket),!,
	add_name(Msg,NMsg),
	term_to_atom(NMsg,AMsg),
	format(WriteFd,  '~a~n', [AMsg]),
	flush_output(WriteFd).

% add_name(Msg,NewMsg)
%
% Adds the name of the sender to the msg (when needed)
add_name(Msg,Msg):-
	member(sender(Name),Msg),
	name(Name),!.
add_name(Msg,[sender(Name)|NMsg]) :-
	member(sender(InvName),Msg),
	delete(Msg,sender(InvName),NMsg),
	name(Name),!.
add_name(Msg,[sender(Name)|Msg]) :-
	name(Name),!.
add_name(Msg,[sender(unknown),Msg]).



% recv_msg(?Name, ?Msg)
%
% Searchs the message queue for the corresponding message
% In the case that it doesnt find it, it blocks until it arrives.
%
recv_msg(Msg) :-
	thread_self(Thread),
	thread_send_message(messages,req(Thread,msgs(Msg))),
	thread_get_message(msgs(Msg)).


% nrecv_msg(?Name, ?Msg, +Timer)
%
% The same as recv_msg/2, with the only difference that it
% is non blocking.

nrecv_msg(Message) :-
	thread_self(Thread),
	thread_send_message(messages,reqnonblock(Thread,msgs(Message))),
	thread_get_message(msgs(Result)),!,
	Message=Result.


% recv_msg(?Name, ?Msg, +Timer)
%
% The same as recv_msg/2, with the only difference that it
% blocks for Timer seconds.
%
recv_msg(Message,Timer) :-
	thread_self(Thread),
	thread_send_message(messages,req(Thread,msgs(Message),Timer)),
	thread_get_message(msgs(Result)),!,
	Message=Result.


bind_msg(Msg,Pred) :-
	assert(binds(Msg,Pred)).

unbind_msg(Msg) :-
	retract(binds(Msg,_Pred)).

% which_agents(-List)
%
% return the list of connected agents (clients or servers)
%
which_agents(L) :-
	findall(Name,stream(Name, _ReadFd, _WriteFd, _Socket),L).

set_name(Name) :-
	assert(name(Name)).

user:prolog_exception_hook(error(domain_error(file_stream, Stream),_),_,_,_) :-
	delete_stream_from_pool(Stream).


% create_stream_pool_loop/0
%
% If not already created, it creates a thread and executes
% the stream pool loop,
%
create_stream_pool_loop :-
	streampool.

create_stream_pool_loop :-
	assert(streampool),
	thread_create(stream_pool_main_loop,_Id,[alias(streampool)]).


% create_messages_handler/0
%
% If not already created, it creates the message queue and
% a thread, executing the message handling loop.
%
create_messages_handler :-
	messages_queue(_,_).

create_messages_handler :-
	message_queue_create(messages),
	thread_create(handle_messages(messages),Messages_Thread,[alias(msg_handler)]),
	assert(messages_queue(Messages_Thread,messages)).

% handle_messages/1
%
% Obtain the messages from the queue and process them.
%
handle_messages(Queue) :-
	thread_get_message(Queue,Msg),
	process_messages(Msg),
	handle_messages(Queue).


% create_binds_handler/0
%
% If not already created, it creates the binds queue and
% all the threads asociated with it.
%
create_binds_handler(_Count):-
	binds_queue(_,_).

create_binds_handler(Count) :-
	message_queue_create(binds_queue),
	create_threads(Count,Ids),
	assert(binds_queue(Ids,binds_queue)).

create_threads(0,[]):-!.
create_threads(Count,[NewId|Ids]):-
	swritef(SAlias, 'handle_create_%w', [Count]),
	term_to_atom(Alias,SAlias),
	thread_create(handle_binds(binds_queue),NewId,[alias(Alias)]),
	C1 is Count - 1,
	create_threads(C1,Ids).

handle_binds(Queue) :-
	thread_get_message(Queue,Pred),
	call(Pred),!,
	handle_binds(Queue).


% process_messages(msgs/2)
%
% Process the incomming messages, searching for a thread waiting for
% them and unblocks it, or queue the received message.
% Threads wainting for messages with instantiated senders have priority
% over uninstantiated ones.
%
process_messages(msgs(Msg)) :-
	search_binds(Msg,Pred),!,
	thread_send_message(binds_queue,Pred).
process_messages(msgs(Msg)) :-
	search_instantiated_waiting_for_message(Thread,Msg,Pattern),!,
	thread_send_message(Thread,msgs(Pattern)).
process_messages(msgs(Msg)) :-
	search_waiting_for_message(Msg,Thread,Pattern),!,
	thread_send_message(Thread,msgs(Pattern)).
process_messages(msgs(Msg)) :-
	assert(msgs(Msg)).

% process_messages(req/2)
%
% Process incomming requirements for messages.
% If the requested message is queued, it returns inmediatly, otherwise
% it blocks until the desired message arrives.
%
process_messages(req(Thread,msgs(Msg))) :-
	search_message(Msg),!,
	thread_send_message(Thread,msgs(Msg)).
process_messages(req(Thread,msgs(Msg))) :-
	assert(waiting(Thread,msgs(Msg))).


% process_messages(reqnonblock/2)
%
% Process incomming requirements for messages.
% If the requested message is queued, it returns inmediatly, otherwise
% it blocks until the desired message arrives.
%
process_messages(reqnonblock(Thread,msgs(Msg))) :-
	search_message(Msg),!,
	thread_send_message(Thread,msgs(Msg)).
process_messages(reqnonblock(Thread,msgs(_Msg))) :-
	thread_send_message(Thread,msgs(timeout)).


% process_messages(req/3)
%
% Process incomming requirements for messages with timeout.
% If the requested message is queued, it returns inmediatly, otherwise
% it blocks until the desired message arrives for Time seconds.
%
process_messages(req(Thread,msgs(Msg),_Time)) :-
	search_message(Msg),!,
	thread_send_message(Thread,msgs(Msg)).
process_messages(req(Thread,msgs(Msg),Time)) :-
	assert(waiting(Thread,msgs(Msg))),
	remove_alarm_if(Thread),
	alarm(Time,thread_send_message(messages,timeout(Thread)),_Id).

% process_messages(timeout/1)
%
% Process incomming timeouts, in order to unblock the corresponding
% thread.
%
process_messages(timeout(Thread)) :-
	retract(waiting(Thread,_)),
	thread_send_message(Thread,msgs(timeout)).
process_messages(timeout(_Thread)).

% remove_alarm_if/1
%
% Removes the alarm of the corresponding thread if exists.
%
remove_alarm_if(Thread):-
	current_alarm(_Time,Goal,Id,scheduled),
	Goal=thread_send_message(messages,timeout(Thread)),
	remove_alarm(Id).
remove_alarm_if(_Thread).

% instantiated_waiting/2
%
% Determines whether exists a thread waiting for the message with the
% sender instantiated.
%
search_instantiated_waiting_for_message(Msg,Thread,Pattern) :-
	waiting(Thread,msgs(Pattern)),
	member(sender(Sender),Pattern),
	nonvar(Sender),
	match(Pattern,Msg),
	retract(waiting(Thread,msgs(Pattern))).

search_waiting_for_message(Msg,Thread,Pattern) :-
	waiting(Thread,msgs(Pattern)),
	match(Pattern,Msg),
	retract(waiting(Thread,msgs(Pattern))).


search_message(Pattern) :-
	msgs(Msg),
	match(Pattern,Msg),
	retract(msgs(Msg)).

search_binds(Msg,Pred) :-
	binds(Pattern,Pred),
	match(Pattern,Msg).



% listen(+Socket)
%
% accept incomming connections and adds the stream to the streampool
listen(Socket) :-
	get_name(Name),
        tcp_accept(Socket, Client, _Peer),
        dont_fail(connected(Name)),
	tcp_open_socket(Client, ReadFd, WriteFd),
	add_stream_to_pool(ReadFd,handle_service(ReadFd)),
	asserta(stream(Name, ReadFd, WriteFd, Client)).


% handle_service(+Stream)
%
% Receives incomming messages and queues them.
% In the case that the stream is closed, the stream is deleted from the
% streampool.
%
handle_service(ReadFd) :-
	read_line_to_codes(ReadFd, List_of_codes),
	string_to_list(AMsg,List_of_codes),
	check_quit(AMsg,no),!,
	term_to_atom(Msg,AMsg),
	stream(Name,ReadFd,_WriteFd,_Client),
	check_msg_name(Name, Msg),                         %checks name consistency or updates it
	thread_send_message(messages,msgs(Msg)).

handle_service(ReadFd) :-
	retract(stream(Name, ReadFd, WriteFd, _Socket)),
	dont_fail(disconnected(Name)),
	delete_stream_from_pool(ReadFd),
%	close(ReadFd),
	close(WriteFd).
%	tcp_close_socket(Socket).

check_msg_name(Name,Msg) :-
	member(sender(Name),Msg).

check_msg_name(Name,Msg) :-
	member(sender(NewName),Msg),
	retract(stream(Name, ReadFd, WriteFd, Client)),
	assert(stream(NewName, ReadFd, WriteFd, Client)).


% check_quit(+Msg, -Result)
%
% Determines whether the message corresponds to a closed stream
% returning in Result 'yes' or 'no' correspondingly
check_quit(Msg,no) :-
	string_to_atom(Msg,Atom),
	Atom \= end_of_file.

check_quit(_,yes).



get_name(New_Name) :-
	stream(Last_Name, _ReadFd, _WriteFd, _Socket),
	Last_Name =.. [Name,Count],
	New_Count is Count + 1,
	New_Name =.. [Name,New_Count].

get_name(name(0)).


dont_fail(X):-call(X).
dont_fail(_).


%match(?MsgPatter,+Msg).
%
%
match(_,Msg):-var(Msg),!,fail.
match(Msg,Msg):-!.
match([],_Msg):-!,fail.
match(Pattern,Msg):-
	rmatch(Pattern,Msg).


rmatch(Msg,Msg):-!.

rmatch([],_Msg):-!.

rmatch([MsgElement|R],Msg) :-
%	MsgElement =.. [_Parameter,_Value],   Si quiero asegurarme que sea el formato
	select(MsgElement,Msg,NewMsg),!,
	rmatch(R,NewMsg).
	
