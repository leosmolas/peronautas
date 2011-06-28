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
	    connect/3,
	    disconnect/1,
	    disconnect_all/0,
	    send_msg/2,
	    recv_msg/2,
	    recv_msg/3,
	    which_agents/1
	  ]).

:- use_module(library(streampool)).

:- dynamic(stream/4). %stream(Name,ReadStream,WriteStream,Socket)
:- dynamic(msgs/2).
:- dynamic(server_id/2).
:- dynamic(streampool/0).
:- dynamic(messages_queue/2).
:- dynamic(waiting/2).
%:- dynamic(disconnected/1).
%:- dynamic(connected/1).




% quit/0
%
% stops listening for connections.
quit :-
	close_server,
	disconnect_all.

close_server :-
	retract(server_id(Socket, Stream)),
	delete_stream_from_pool(Stream),
	tcp_close_socket(Socket).

disconnect_all:-
	which_agents([]).

disconnect_all:-
	which_agents([A|_R]),
	disconnect(A),
	disconnect_all.

user:prolog_exception_hook(error(domain_error(file_stream, Stream),_),_,_,_) :-
	delete_stream_from_pool(Stream).

create_stream_pool_loop :-
	streampool.

create_stream_pool_loop :-
	assert(streampool),
	thread_create(stream_pool_main_loop,_Id,[alias(streampool)]).


create_messages_handler :-
	messages_queue(_,_).

create_messages_handler :-
	message_queue_create(messages),
	thread_create(handle_messages(messages),Messages_Thread,[alias(msg_handler)]),
	assert(messages_queue(Messages_Thread,messages)).

handle_messages(Queue) :-
	thread_get_message(Queue,Msg),
	process_messages(Msg),
	handle_messages(Queue).

process_messages(msgs(Sender,Msg)) :-
	retract(waiting(Thread,msgs(Sender,Msg))),
	thread_send_message(Thread,msgs(Sender,Msg)).
process_messages(msgs(Sender,Msg)) :-
	assert(msgs(Sender,Msg)).

process_messages(req(Thread,msgs(Sender,Msg))) :-
	retract(msgs(Sender,Msg)),
	thread_send_message(Thread,msgs(Sender,Msg)).
process_messages(req(Thread,msgs(Sender,Msg))) :-
	assert(waiting(Thread,msgs(Sender,Msg))).

process_messages(req(Thread,msgs(Sender,Msg),_Time)) :-
	retract(msgs(Sender,Msg)),
	thread_send_message(Thread,msgs(Sender,Msg)).
process_messages(req(Thread,msgs(Sender,Msg),Time)) :-
	assert(waiting(Thread,msgs(Sender,Msg))),
	remove_alarm_if(Thread),
	alarm(Time,thread_send_message(messages,timeout(Thread)),_Id).

process_messages(timeout(Thread)) :-
	retract(waiting(Thread,_)),
	thread_send_message(Thread,timeout).
process_messages(timeout(_Thread)).

remove_alarm_if(Thread):-
	current_alarm(_Time,Goal,Id,scheduled),
	Goal=thread_send_message(messages,timeout(Thread)),
	remove_alarm(Id).
remove_alarm_if(_Thread).


% start_server(+Port)
%
% starts a server in a given Port
% each connection adds the corresponding stream to the streampool
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
	create_messages_handler.


% listen(+Socket)
%
% accept incomming connections and adds the stream to the streampool
listen(Socket) :-
	get_name(Name),
        tcp_accept(Socket, Client, _Peer),
        dont_fail(connected(Name)),
	tcp_open_socket(Client, ReadFd, WriteFd),
	add_stream_to_pool(ReadFd,handle_service(Name, ReadFd)),
	asserta(stream(Name, ReadFd, WriteFd, Client)).


% handle_service(+Name, +Stream)
%
% receives incomming messages and queues them
handle_service(Name, ReadFd) :-
	read_line_to_codes(ReadFd, List_of_codes),
	string_to_list(AMsg,List_of_codes),
	check_quit(AMsg,no),
	term_to_atom(Msg,AMsg),
	deliver_msg(Name, Msg).

% in the case that the stream is close, the stream is deleted from the
% from the streampool.
handle_service(Name, ReadFd) :-
	dont_fail(disconnected(Name)),
	close(ReadFd),
	retract(stream(Name, ReadFd, WriteFd, _Client)),
	close(WriteFd),
	delete_stream_from_pool(ReadFd).

deliver_msg(Sender, Msg) :-
	thread_send_message(messages,msgs(Sender,Msg)).


% check_quit(+Msg, -Result)
%
% determines whether the message corresponds to a closed stream
% returning in Result 'yes' or 'no' correspondingly
check_quit(Msg,no) :-
	string_to_atom(Msg,Atom),
	Atom \= end_of_file.

check_quit(_,yes).


% connect(+Name, +Host, +Port)
%
% starts a tcp connection with the corresponding Host:Port
% assigning the corresponding Name
connect(Name, Host, Port) :-
        tcp_socket(Socket),
	catch(socket:tcp_connect(Socket, Host:Port),_,fail),
%	catch_connect(Socket, Host, Port, true),
%        tcp_connect(Socket, Host:Port),
%	catch(tcp_connect(Socket, Host:Port),error(_,_),(!,fail)),
	create_messages_handler,
        tcp_open_socket(Socket, ReadFd, WriteFd),
	add_stream_to_pool(ReadFd,handle_service(Name, ReadFd)),
	create_stream_pool_loop,
	asserta(stream(Name, ReadFd, WriteFd, Socket)).




% disconnect(+Name)
%
% close the corresponding connection
disconnect(Name) :-
	retract(stream(Name, ReadFd, WriteFd, Socket)),
	delete_stream_from_pool(ReadFd),
	close(WriteFd),
	tcp_close_socket(Socket).


send_msg(Name,Msg) :-
        stream(Name, _ReadFd, WriteFd, _Socket),
	term_to_atom(Msg,AMsg),
	format(WriteFd,  '~a~n', [AMsg]),
	flush_output(WriteFd).

recv_msg(Name,Msg) :-
	thread_self(Thread),
	thread_send_message(messages,req(Thread,msgs(Name,Msg))),
	thread_get_message(msgs(Name,Msg)).

recv_msg(Name,Msg,Timer) :-
	thread_self(Thread),
	thread_send_message(messages,req(Thread,msgs(Name,Msg),Timer)),
	thread_get_message(Message),
	check_message(Message,msgs(Name,Msg)).

check_message(timeout, msgs(_,timeout)).
check_message(Msg, Msg).

which_agents(L) :-
	findall(Name,stream(Name, _ReadFd, _WriteFd, _Socket),L).

get_name(New_Name) :-
	stream(Last_Name, _ReadFd, _WriteFd, _Socket),
	Last_Name =.. [Name,Count],
	New_Count is Count + 1,
	New_Name =.. [Name,New_Count].

get_name(name(0)).


dont_fail(X):-call(X).
dont_fail(_).
