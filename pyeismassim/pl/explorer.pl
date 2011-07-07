exec(Action) :- action(Action).

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

action(probe) :-
	%   private Action planProbe() {
	%   	LinkedList<LogicBelief> beliefs = null;
	%   	
	%   	beliefs =  getAllBeliefs("position");
	%   	if ( beliefs.size() == 0 ) {
	%   			println("strangely I do not know my position");
	%   			return Mars2011Util.skipAction();
	%   	}
	%   	String position = beliefs.getFirst().getParameters().firstElement();
	%   	
	%   	// probe current position if not known
	%   	boolean probed = false;
	%   	LinkedList<LogicBelief> vertices = getAllBeliefs("probedVertex");
	%   	for ( LogicBelief v : vertices) {
	%   		if ( v.getParameters().get(0).equals(position) ) {
	%   			probed = true;
	%   			break;
	%   		}
	%   	}
	%   	if ( probed == false ) {
	%   		println("I do not know the value of my position. I will probe.");
	%   		return Mars2011Util.probeAction();
	%   	}
	%   	else {
	%   		println("I know the value of my position");
	%   	}
	%   	
	%   	beliefs = getAllBeliefs("neighbor");
	%   	
	%   	// get unprobed neighbors
	%   	Vector<String> unprobed = new Vector<String>();
	%   	for ( LogicBelief n : beliefs ) {
	%   		probed = false;
	%   		String name = n.getParameters().firstElement();
	%   		for ( LogicBelief v : vertices) {
	%   			if ( v.getParameters().get(0).equals(name) ) {
	%   				probed = true;
	%   				break;
	%   			}		
	%   		}
	%   		if ( probed == false )
	%   			unprobed.add(name);
	%   	}
	%   	if ( unprobed.size() != 0 ) {
	%   		println("some of my neighbors are unprobed.");
	%   		Collections.shuffle(unprobed);
	%   		String neighbor = unprobed.firstElement();
	%   		println("I will go to " + neighbor);
	%   		return Mars2011Util.gotoAction(neighbor);
	%   	}
	%   	else {
	%   		println("all of my neighbors are probed");
	%   	}		
	%   
	%   	return null;
	%   }
    
action(survey) :-
	%   private Action planSurvey() {
	%   	println("I know " + getAllBeliefs("visibleEdge").size() + " visible edges");
	%   	println("I know " + getAllBeliefs("surveyedEdge").size() + " surveyed edges");
	%   	// get all neighbors
	%   	LinkedList<LogicBelief> visible = getAllBeliefs("visibleEdge");
	%   	LinkedList<LogicBelief> surveyed = getAllBeliefs("surveyedEdge");
	%   	String position = getAllBeliefs("position").get(0).getParameters().firstElement();
	%   	
	%   	int unsurveyedNum = 0;
	%   	int adjacentNum = 0;
	%   	
	%   	for ( LogicBelief v : visible ) {
	%   	
	%   		String vVertex0 = v.getParameters().elementAt(0);
	%   		String vVertex1 = v.getParameters().elementAt(1);
	%   		boolean adjacent = false;
	%   		if ( vVertex0.equals(position) || vVertex1.equals(position) )
	%   			adjacent = true;
	%   		
	%   		if ( adjacent == false) continue;
	%   		adjacentNum ++;
	%   		
	%   		boolean isSurveyed = false;
	%   		for ( LogicBelief s : surveyed ) {
	%   			String sVertex0 = s.getParameters().elementAt(0);
	%   			String sVertex1 = s.getParameters().elementAt(1);
	%   			if ( sVertex0.equals(vVertex0) &&  sVertex1.equals(vVertex1) ) {
	%   				isSurveyed = true;
	%   				break;
	%   			}
	%   			if ( sVertex0.equals(vVertex1) &&  sVertex1.equals(vVertex0) ) {
	%   				isSurveyed = true;
	%   				break;
	%   			}
	%   		}
	%   		if ( isSurveyed == false ) unsurveyedNum ++;
	%   		
	%   	}
	%   	println("" + unsurveyedNum + " out of " + adjacentNum + " adjacent edges are unsurveyed");
	%   	
	%   	if ( unsurveyedNum > 0 ) {
	%   		println("I will survey");
	%   		return Mars2011Util.surveyAction();
	%   	}
	%   	
	%   	return null;
	%   	
	%   }

action(move(X)) :-
	%   private Action planRandomWalk() {
	%   	LinkedList<LogicBelief> beliefs = getAllBeliefs("neighbor");
	%   	Vector<String> neighbors = new Vector<String>();
	%   	for ( LogicBelief b : beliefs ) {
	%   		neighbors.add(b.getParameters().firstElement());
	%   	}
	%   	
	%   	if ( neighbors.size() == 0 ) {
	%   		println("strangely I do not know any neighbors");
	%   		return Mars2011Util.skipAction();
	%   	}
	%   	
	%   	// goto neighbors
	%   	Collections.shuffle(neighbors);
	%   	String neighbor = neighbors.firstElement();
	%   	println("I will go to " + neighbor);
	%   	return Mars2011Util.gotoAction(neighbor);
	%   	
	%   }
