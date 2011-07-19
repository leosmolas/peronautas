%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Sentinel                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

reachableNode(Node, [[_, unknown] | T]) :-
    reachableNode(Node, T),
    !.

reachableNode(Node, [[Node, Cost] | _T]) :-
    Cost \= unknown,
    myName(Name),
    currentStep(Step),
    energy(Step, Name, Energy),
    Energy >= Cost,
    !.

reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    currentStep(Step),
    write(3),nl,
    myName(Name),
    write(3.1),nl,
    energy(Step, Name, Energy),
    write(3.2),nl,
    Energy > 0,
    write(3.3),nl,
    position(Step, Name, Position),
    write(3.4),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.5),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%

action([goto, X]) :-
    currentStep(Step),
    write(5),nl,
    myName(Name),
    write(5.1),nl,
    position(Step, Name, Position),
    write(5.2),nl,
    energy(Step, Name, Energy),
    write(5.3),nl,
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(5.4),nl,
    Energy >= Cost, 
    write(5.5),nl,
    !.

%-------------------------------  Recharge  ------------------------------%
    
action([recharge]) :-
    write(6),nl.

%-------------------------------  Old Code  ------------------------------%

% step(Action) :-
%    handle_messages,
%    handle_percepts,
%    action(Action).

% action(recharge) :-
%    % Plan: recharge.
%    % get energy level.
%    % if at full charge, stop recharging
%    % if energy < maxenergy / 3, recharge
%    true.

% action(buy(battery)) :-
%    % Plan: buy battery.
%    % get money
%    % if money < 10, we do not have enough money
%    % otherwise but battery
%    true.

% action(survey) :-
    %   println("I know " + getAllBeliefs("visibleEdge").size() + " visible edges");
    %   println("I know " + getAllBeliefs("surveyedEdge").size() + " surveyed edges");
    %
    %   // get all neighbors
    %   LinkedList<LogicBelief> visible = getAllBeliefs("visibleEdge");
    %   LinkedList<LogicBelief> surveyed = getAllBeliefs("surveyedEdge");
    %
    %   String position = getAllBeliefs("position").get(0).getParameters().firstElement();
    %   
    %   int unsurveyedNum = 0;
    %   int adjacentNum = 0;
    %   
    %   for ( LogicBelief v : visible ) {
    %   
    %       String vVertex0 = v.getParameters().elementAt(0);
    %       String vVertex1 = v.getParameters().elementAt(1);
    %
    %       boolean adjacent = false;
    %       if ( vVertex0.equals(position) || vVertex1.equals(position) )
    %           adjacent = true;
    %       
    %       if ( adjacent == false) continue;
    %       adjacentNum ++;
    %       
    %       boolean isSurveyed = false;
    %       for ( LogicBelief s : surveyed ) {
    %           String sVertex0 = s.getParameters().elementAt(0);
    %           String sVertex1 = s.getParameters().elementAt(1);
    %           if ( sVertex0.equals(vVertex0) &&  sVertex1.equals(vVertex1) ) {
    %               isSurveyed = true;
    %               break;
    %           }
    %           if ( sVertex0.equals(vVertex1) &&  sVertex1.equals(vVertex0) ) {
    %               isSurveyed = true;
    %               break;
    %           }
    %       }
    %       if ( isSurveyed == false ) unsurveyedNum ++;
    %       
    %   }
    %
    %   println("" + unsurveyedNum + " out of " + adjacentNum + " adjacent edges are unsurveyed");
    %   
    %   if ( unsurveyedNum > 0 ) {
    %       println("I will survey");
    %       return Mars2011Util.surveyAction();
    %   }
    %   
    %   return null;
        

% action(goto(Vertex)) :-
    % Random walking
    % select neighbouring vertex
%    Vertex = something.

% action(skip).

