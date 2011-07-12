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

action(repair(Agent)) :-
    % Repairing agent
    % select needy agent in my actual position
    Agent = something.

action(goto(Vertex)) :-
    % Random walking
    % select neighbouring vertex
    Vertex = something.

action(skip).


%  public class SimpleRepairerAgent extends Agent {
%  
%  	int rechargeSteps = 0;
%  	
%  	public SimpleRepairerAgent(String name, String team) {
%  		super(name, team);
%  	}
%  
%  	@Override
%  	public void handlePercept(Percept p) {
%  	}
%  
%  	@Override
%  	public Action step() {
%  
%  		if ( rechargeSteps > 0 ) {
%  			rechargeSteps --;
%  			println("recharging...");
%  			return Mars2011Util.skipAction();
%  		}
%  		
%  		Collection<Message> messages = getMessages();
%  		Vector<String> needyAgents = new Vector<String>();
%  		for ( Message msg : messages ) {
%  			if (((LogicBelief)msg.value).getPredicate().equals("iAmDisabled"))
%  				needyAgents.add(msg.sender);
%  		}
%  		
%  		if ( needyAgents.size() == 0 ) {
%  			println("nothing for me to do");
%  			return Mars2011Util.skipAction();
%  		}
%  
%  		println("some poor souls need my help " + needyAgents);
%  		
%  		Collection<Percept> percepts = getAllPercepts();
%  		String position = null;
%  		for ( Percept p : percepts ) {
%  			if ( p.getName().equals("lastActionResult") && p.getParameters().get(0).toProlog().equals("failed") ) {
%  				println("my previous action has failed. recharging...");
%  				rechargeSteps = 10;
%  				return Mars2011Util.skipAction();
%  			} 
%  			if ( p.getName().equals("position") ) {
%  				position = p.getParameters().get(0).toString();
%  			}
%  		}
%  		
%  		// a needy one on the same vertex
%  		for ( Percept p : percepts ) {
%  			if ( p.getName().equals("visibleEntity") ) {
%  				String ePos = p.getParameters().get(1).toString();
%  				String eName = p.getParameters().get(0).toString();
%  				if ( ePos.equals(position) && needyAgents.contains(eName) ) {
%  					println("I am going to repair " + eName);
%  					Mars2011Util.repairAction(eName);
%  				}
%  			}
%  		}
%  		
%  		// maybe on an adjacent vertex?
%  		Vector<String> neighbors = new Vector<String>();
%  		for ( Percept p : percepts ) {
%  			if ( p.getName().equals("visibleEdge") ) {
%  				String vertex1 = p.getParameters().get(0).toString();
%  				String vertex2 = p.getParameters().get(1).toString();
%  				if ( vertex1.equals(position) ) neighbors.add(vertex2);
%  				if ( vertex2.equals(position) ) neighbors.add(vertex1);
%  			}
%  		}
%  		for ( Percept p : percepts ) {
%  			if ( p.getName().equals("visibleEntity") ) {
%  				String ePos = p.getParameters().get(1).toString();
%  				String eName = p.getParameters().get(0).toString();
%  				if ( neighbors.contains(ePos) && needyAgents.contains(eName) ) {
%  2					println("I am going to repair " + eName + ". move to " + ePos +" first.");
%  					Mars2011Util.gotoAction(ePos);
%  				}
%  			}
%  		}
%  		
%  		// goto neighbors
%  		if ( neighbors.size() == 0 ) {
%  			println("Strangely I do not know my neighbors");
%  			return Mars2011Util.skipAction();
%  		}
%  		Collections.shuffle(neighbors);
%  		String neighbor = neighbors.firstElement();
%  		println("I will go to " + neighbor);
%  		return Mars2011Util.gotoAction(neighbor);
%  
%  	}
%  
%  }
