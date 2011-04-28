package massim.competition2011;

import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.log;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import massim.competition2011.scenario.ActionExecutor;
import massim.competition2011.scenario.DominatedArea;
import massim.competition2011.scenario.DominationCalculator;
import massim.competition2011.scenario.GraphEdge;
import massim.competition2011.scenario.GraphNode;
import massim.competition2011.scenario.TeamState;
import massim.framework.Action;
import massim.framework.Perception;
import massim.framework.SimulationConfiguration;
import massim.framework.simulation.AbstractSimulation;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.SimulationAgent;
import massim.framework.simulation.WorldState;

/**
 * This is the main class for the GraphSimulation (2011 Mars Scenario).
 */
public class GraphSimulation extends AbstractSimulation{
	
	
	GraphSimulationConfiguration config = null;
	GraphSimulationWorldState state = null;
	
	@Override
	public void configureSimulation(SimulationConfiguration c) {
		super.configureSimulation(c);
		config = (GraphSimulationConfiguration) c;
	}

	@Override
	public boolean isFinished() {
		return getSteps() >= config.maxNumberOfSteps;
	}

	@Override
	public WorldState getSimpleSimulationState() {
		return state;
	}

	@Override
	public void initializeSimpleSimulation() {
		// create graph
		state = new GraphSimulationWorldState(config);
		
		// add the agents
		SimulationAgent agents[] = this.getAgents();		
		for (int i = 0; i < agents.length; i++) {
			GraphSimulationAgent simAgent = (GraphSimulationAgent) agents[i];
			simAgent.initialize(config);
			state.addAgent( (GraphSimulationAgentState)simAgent.getAgentState());
		}
		for (TeamState team : state.teamsStates) {
			team.initAchievements(config.achievements);		
		}
	}

	@Override
	public void preSimulationStep() {
		// do nothing
		
	}

	@Override
	public void postSimulationStep() {
		DominationCalculator.calculate(state);
		state.currentStep = this.getSteps();
		for (TeamState ts : state.teamsStates){
			ts.calculateNewAchievements();
			ts.sumCurrent();
		}
		log(LOGLEVEL_NORMAL, "Simulation at step: " + this.getSteps());
	}

	@Override
	public void runAgents() {
		class MyPair {
			public Future<Action> action;
			public SimulationAgent agent;
		}
		
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		
				
		Vector<MyPair> actions = new Vector<MyPair>();
		
		HashMap<AgentState, GraphSimulationAgentPerception> perceptionsMap = 
				new HashMap<AgentState, GraphSimulationAgentPerception>();
		
		// Create perceptions.
		for(int i=0;i<agents.length;i++) {
			// Calculate private perceptions and put them in the map.
			perceptionsMap.put(
					agents[i].getAgentState(), 
					(GraphSimulationAgentPerception)agents[i].createPerception(simstate, agentstates));
		}
		
		// Compute shared perceptions from dominated areas.
		for( TeamState ts: state.teamsStates){
			for ( DominatedArea area : ts.areas) {
				GraphSimulationAgentPerception sharedPer = new GraphSimulationAgentPerception();
				
				// Should nodes (and edges) belonging to the area, but not in the the visibility range
				// of any agent in the team, be shown as part of shared perceptions?
				if (true){ // TODO define a configuration parameter.
					sharedPer.nodes.addAll(area.nodes);
					HashSet<GraphEdge> connectedEdges = new HashSet<GraphEdge>();
					for (GraphNode node: area.nodes){
						connectedEdges.addAll(state.getConnectedEdges(node));
					}
					for (GraphEdge edge: connectedEdges ){
						if (area.nodes.contains(edge.node1) && area.nodes.contains(edge.node2)){
							sharedPer.edges.add(edge);
						}
					}					
				}
				
				for (GraphSimulationAgentState agent : area.agents){
					sharedPer.addSharedPercept(perceptionsMap.get(agent));
				}
				for (GraphSimulationAgentState agent : area.agents){
					perceptionsMap.get(agent).addSharedPercept(sharedPer);
				}				
			}
		}		
		
		// Send perceptions to agents and retrieve future object for actions.
		for (int i=0;i<agents.length;i++) {		
			MyPair m = new MyPair();
			m.action = agents[i].getAgent().concurrentGetAction(
					perceptionsMap.get(agents[i].getAgentState()));
			m.agent = agents[i];
			actions.add(m);
		}
		try {
			for (int i=0;i<agents.length;i++) {
				MyPair m = actions.get(i);
				// Only set the action. It will get executed in simulationStep().
				((GraphSimulationAgent)m.agent).setAction(m.action.get());
			}
			
		} catch (ExecutionException e) {
			log(LOGLEVEL_CRITICAL,"Execution of getAction failed, shouldn't happen");
		} catch (InterruptedException e) {
			log(LOGLEVEL_CRITICAL,"Thread interrupted, shouldn't happen");
		}
		
	}

	@Override
	public void simulationStep() {
		ActionExecutor.execute(state);		
	}

		
	/**
	 * This method prepares and sends the initial perceptions to the agents for starting the simulation
	 */
	public void runInitAgents() {
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		// Let agents act.
		for(int i=0;i<agents.length;i++) {
			Perception p = agents[i].createInitialPerception(simstate,agentstates);
			// TODO: use concurretGetAction?
			agents[i].getAgent().getAction(p);
		}
	}
	
	/**
	 * This method prepares and sends the final perceptions to the agents when the simulation is finished
	 */
	public void runFinalAgents() {
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		// Let agents act.
		for(int i=0;i<agents.length;i++) {
			Perception p = agents[i].createFinalPerception(simstate,agentstates);
			// TODO: use concurretGetAction?
			agents[i].getAgent().getAction(p);
		}
	}

	@Override
	public String finalizeSimpleSimulation() {
		// Calculate ranking
		Vector<TeamState> rankings = new Vector<TeamState>(state.teamsStates);
		Collections.sort(rankings, new Comparator<TeamState>() {
			
			@Override
			public int compare(TeamState t1, TeamState t2) {
				if (t1.summedScore > t2.summedScore){
					return -1;
				}
				if (t1.summedScore < t2.summedScore){
					return 1;
				}
				
				if (t1.currAchievementPoints > t2.currAchievementPoints){
					return -1;
				}
				if (t1.currAchievementPoints < t2.currAchievementPoints){
					return 1;
				}
				
				return 0;
			}
		});
		
		int i = 1;
		for (TeamState teamState : rankings) {
			teamState.ranking = i++;
		}
		
		return rankings.firstElement().name;
	}

}
