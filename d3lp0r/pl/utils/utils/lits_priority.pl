% <A,h> is better than <B,k> ifff there is a literal in A that is preferred 
% to (at least) one literal in B, and the contrary does not occur

:- dynamic prefer/2.

lits_priority(arg(ArgA,_),arg(ArgB,_)):-

	(
	member(d_rule(HeadA,_),ArgA)
	;
	member(s_rule(HeadA,_),ArgA)
	),

	(
	member(d_rule(HeadB,_),ArgB)
	;
	member(s_rule(HeadB,_),ArgB)
	),
	prefer(HeadA,HeadB),!,
	
	\+ lits_priority_no_reverse(arg(ArgB,_),arg(ArgA,_)).
	


lits_priority_no_reverse(arg(ArgA,_),arg(ArgB,_)):-

	(
	member(d_rule(HeadA,_),ArgA)
	;
	member(s_rule(HeadA,_),ArgA)
	),

	(
	member(d_rule(HeadB,_),ArgB)
	;
	member(s_rule(HeadB,_),ArgB)
	),
	prefer(HeadA,HeadB),!.
