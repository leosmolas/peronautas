:- use_module(interface).
:- dynamic registered/3. %registered(Name,Characteristic,Ontology).
:- dynamic names/3.



% connected (Agent)
% displays information when an agent establish a connection with the YPA
connected(Agent) :-
	write(connected(Agent)),nl.
	
% disconnected (Agent)
% displays a message informing when an agent ends the connection 
% with the YPA and removes the registered information
disconnected(Agent) :-
	retractall(registered(Agent,_Charact,_Ontology)),
	retract(names(Agent,_Host,_Port)),
	write(disconnected(Agent)),nl.
	
% start(Port)
% inits the YPA agent, initiating the server for incomming 
% connections and binding all the possible incomming messages
start(YPAPort) :-
	set_name(ypa),
	start_server(YPAPort),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(connect),
		  content(host(Host,Port))],
		 try_connect(Name,Host,Port)),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(register),
		  content(register(Characteristic,Ontology))]
		,register(Name,Characteristic,Ontology)),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(deregister),
		  content(deregister(Characteristic,Ontology))]
		,deregister(Name,Characteristic,Ontology)),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(registered_ontologies),
		  content(registered(Ontology))],
		 ontologies(Name,Ontology)),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(registered_agents),
		  content(registered(Characteristic,Ontology))],
		 agents(Name,Characteristic,Ontology)),
	bind_msg([sender(Name),
		  performative(request),
		  ontology(ypa),
		  protocol(agent_location),
		  content(agent(Agent))],
		 agent_location(Name,Agent)).

% try_connect (+Name,+Host,+Port)
% 
% adds the recently connected agent whenever the chosen name is not
% already used by another agent.
try_connect(Name,Host,Port) :-
	add_connected(Name,Host,Port),
	send_msg([receiver(Name),
		  performative(agree),
		  ontology(ypa),
		  protocol(connect),
		  content(done)]),
	write(connected_agent(Name)),nl.

try_connect(Name,_Host,_Port) :-
	send_msg([receiver(Name),
		  performative(refuse),
		  ontology(ypa),
		  protocol(connect),
		  content(name_used(Name))]),
	write(failed_connection(Name)),nl.

% add_connected(+Name,+Host,+Port)
% 
% checks whether the chosen names is already used or not, using 
% it in the latter case
add_connected(Name,_Host,_Port):-
	names(Name,_HostN,_PortN),!,
	fail.

add_connected(Name,Host,Port):-
	asserta(names(Name,Host,Port)).

% register(+Name,+Charact,+Ontology)
%
% register a specific characteristic corresponding a determined ontology
% in the case that the characteristic is already registered it returns
% a refusal
register(Name,Charact,Ontology) :-
	registered(Name,Charact,Ontology),!,
	write(already_registered(Name,Charact,Ontology)),nl,
	send_msg([receiver(Name),performative(refuse),ontology(ypa),protocol(register),
		  content(already_registered(Charact,Ontology))]).
	
register(Name,Charact,Ontology) :-
	assert(registered(Name,Charact,Ontology)),
	write(registered(Name,Charact,Ontology)),nl,
	send_msg([receiver(Name),performative(agree),ontology(ypa),protocol(register),
		  content(registered(Charact,Ontology))]).

% deregister(+Name,+Charact,+Ontology)
%
% deregister a specific characteristic corresponding a determined 
% ontology in the case that the characteristic is not already 
% registered it returns a refusal
deregister(Name,Charact,Ontology) :-
	retract(registered(Name,Charact,Ontology)),
	write(deregistered(Name,Charact,Ontology)),nl,
	send_msg([receiver(Name),performative(agree),ontology(ypa),protocol(deregister),
		  content(deregistered(Charact,Ontology))]).

deregister(Name,Charact,Ontology) :-
	write(not_registered(Name,Charact,Ontology)),nl,
	send_msg([receiver(Name),performative(refuse),ontology(ypa),protocol(deregister),
		  content(not_registered(Charact,Ontology))]).
	


ontologies(Name,Ontology) :-
	findall(Charact,registered(_Agent,Charact,Ontology),List_Of_MASs),
	write(registered_characteristics(List_Of_MASs)),nl,
	send_msg([receiver(Name),performative(agree),ontology(ypa),protocol(registered_ontologies),
		  content(ontologies(List_Of_MASs))]).


agents(Name,Charact,Ontology) :-
	findall(Agent,registered(Agent,Charact,Ontology),List_Of_Agents),
	write(agents(List_Of_Agents)),nl,
	send_msg([receiver(Name),performative(agree),ontology(ypa),protocol(registered_agents),
		  content(agents(List_Of_Agents))]).


agent_location(Name,Agent):-
	names(Agent,Host,Port),
	send_msg([receiver(Name),
		  performative(agree),
		  ontology(ypa),
		  protocol(agent_location),
		  content(agent(Agent,Host,Port))
		 ]),
	write(agent_location(to(Name),of(Agent,Host,Port))),nl.

agent_location(Name,Agent):-
	send_msg([receiver(Name),
		  performative(refuse),
		  ontology(ypa),
		  protocol(agent_location),
		  content(unexistent_agent(Agent))
		 ]),
	write(unexistent_agent(Agent)),nl.


