exec(Action) :- action(Action).


action(inspect(Agent)) :-
    energy(X),
    X > 1,
    my_name(Name),
    kposition(Name, Position),
    kposition(Agent, Position),
    teamOfAgent(Agent, Team),
    Team \= d3lp0r, !.

action(goto(Vertex)) :-
    % Random walking
    % select a neighbouring vertex
    Vertex = something.

action(recharge).

