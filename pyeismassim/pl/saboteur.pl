exec(Action) :- action(Action).

action(fight(Agent)) :-
    energy(X),
    X > 1,
    my_name(Name),
    kposition(Name, Position),
    kposition(Agent, Position),
    teamOfAgent(Agent, Team),
    Team \= d3lp0r, !.


action(goto(X)) :-
    my_name(Name),
    kposition(Name, Position),
    energy(E),
    kedge(Position, X, Cost),
    E >= Cost, !.
    
action(recharge).

