%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Repairer                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Actions (priority order):
%                   -repair
%                   -survey
%                   -goto
%                   -parry  (no implemented)
%                   -buy    (no implemented)
%                   -rechage
%                   -skip   (no implemented)

%-----------------------------------------------------------------------%

exec(Action) :- 
    write(1),nl,
    action(Action).

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

%------------------------------  Repair  --------------------------------%

action([repair, Ally]):-
    write(2),nl,
    %obtengo mi nombre
    myName(Name),
    %obtengo el step del nivel actual
    currentStep(Step),
    %obtengo mi energia
    energy(Step, Name, Energy),
    %chequeo si puedo realizar la accion de ataque { cost(attack)=2 }
    write(2.1),write(' energy: '),write(Energy),nl,
    Energy > 1,
    %obtengo mi posicion
    write(2.2),nl,
    position(Step, Name, Position),
    %obtengo cual es mi equipo
    write(2.3),nl,
    team(Step, Name, Team),
    %obtengo el nombre de un agente que se encuentra en mi posicion
    write(2.4),nl,
    position(Step, Ally, Position),
    Ally \= Name,
    %veo que sea de mi equipo
    write(2.5),nl,
    team(Step, Ally, Team),
    %lo curo
    !.
    
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

% action(repair(Agent)) :-
%    % Repairing agent
%    % select needy agent in my actual position
%    Agent = something.

% action(goto(Vertex)) :-
%    % Random walking
%    % select neighbouring vertex
%    Vertex = something.

% action(skip).


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
