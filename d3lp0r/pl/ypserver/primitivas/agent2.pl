:- [ypa].

connected(Agent) :-
	write(connected(Agent)),nl.
	
disconnected(Agent) :-
	write(disconnected(Agent)),nl.
	
s :-
	set_ypa_address(localhost,8000).
	
c :-
	connect(tuks,_E).

r(C):-
	register(characteristic(C),ontology,_Error).

d(C):-
	deregister(characteristic(C),ontology,_Error).

wm:-
	which_characteristics(L,ontology,_Error),
	write(ontologies(L)),nl.

wa(C):-
	which_agents(L,characteristic(C),ontology,_Error),
	write(agents(L,characteristic(C),ontology)),nl.

mr(0).
mr(C):-
	r(C),
	Cn is C-1,
	mr(Cn).
	
md(0).
md(C):-
	d(C),
	Cn is C-1,
	md(Cn).

mwa(0).
mwa(C):-
	wa(C),
	Cn is C-1,
	mwa(Cn).


:- s.









