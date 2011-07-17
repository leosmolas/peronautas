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
    energy(X),
    X >= Cost,
    !.
reachableNode(Node, [_ | T]) :-
    reachableNode(Node, T).

%------------------------------  Repair  --------------------------------%

action([repair, Ally]):-
    write(2),nl,
    %obtengo mi energia
    energy(X),
    %chequeo si puedo realizar la accion de ataque { cost(attack)=2 }
    write(2.1),write(' energy: '),write(X),nl,
    X>1,
    %obtengo mi nombre
    myName(Name),
    %obtengo mi posicion
    write(2.2),nl,
    k(position(Name, Position)),
    %obtengo cual es mi equipo
    write(2.3),nl,
    agentTeam(Name, Team),
    %obtengo el nombre de un agente que se encuentra en mi posicion
    write(2.4),nl,
    k(position(Ally, Position)),
    Ally \= Name,
    %veo que sea de mi equipo
    write(2.5),nl,
    agentTeam(Ally, Team),
    %lo curo
    !.
    
%------------------------------  Survey  --------------------------------%

action([survey, Position]) :-
    write(3),nl,
    energy(X),
    write(3.1),nl,
    X > 0,
    write(3.2),nl,
    myName(Name),
    write(3.3),nl,
    k(position(Name, Position)),
    write(3.4),nl,
    hasAtLeastOneUnsurveyedEdge(Position), 
    write(3.5),nl,
    !.

%-------------------------------  Goto  ---------------------------------%

%-- First Node Goto --%

action([goto, X]) :-
    write(5),nl,
    myName(Name),
    write(5.1),nl,
    k(position(Name, Position)),
    write(5.2),nl,
    energy(E),
    write(5.3),nl,
    k(edge(Position, X, Cost)),
    Cost \= unknown,
    write(5.4),nl,
    E >= Cost, 
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

