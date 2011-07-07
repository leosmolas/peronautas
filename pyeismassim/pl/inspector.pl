step(Action) :-
    handle_messages,
    handle_percepts,
    action(Action).

action(recharge) :-
    % Plan: recharge.
    % get energy level.
    % if at full charge, stop recharging
    % if energy < maxenergy / 3, recharge
    true.

action(buy(battery)) :-
    % Plan: buy battery.
    % get money
    % if money < 10, we do not have enough money
    % otherwise but battery
    true.

action(inspect(Agent)) :-
    % Inspect if necessary.
    % if there is a visible entity
    % get the parameters
    % ignore from the same team
    % and ignore not adjacent
    % select agent id
    Agent = something.

action(goto(Vertex)) :-
    % Random walking
    % select a neighbouring vertex
    Vertex = something.

action(skip).

