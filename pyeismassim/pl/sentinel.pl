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

action(survey) :-
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
        

action(goto(Vertex)) :-
    % Random walking
    % select neighbouring vertex
    Vertex = something.

action(skip).

