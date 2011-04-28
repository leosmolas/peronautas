package massim.competition2011;

import java.util.LinkedList;
import java.util.List;
import java.util.Vector;

import massim.competition2011.scenario.GraphEdge;
import massim.competition2011.scenario.GraphNode;
import massim.competition2011.scenario.RoleConfiguration;
import massim.framework.Action;
import massim.framework.AgentParameter;
import massim.framework.FinalPerception;
import massim.framework.InitialStickyPerception;
import massim.framework.Perception;
import massim.framework.UniqueSimulationAgent;
import massim.framework.connection.UsernamePasswordAccount;
import massim.framework.simulation.AbstractSimulationAgent;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.WorldState;

/**
 * This class deals with the agent state, his actions and perceptions.
 */
public class GraphSimulationAgent extends AbstractSimulationAgent {

	
	private GraphSimulationAgentState agentstate = null;
	private GraphSimulationAgentAction action = null;
	// private GraphSimulationWorldState simulationstate = null;

	
	
	/**
	 * The constructor instantiates the agentstate and the action
	 */
	public GraphSimulationAgent() {		
		agentstate = new GraphSimulationAgentState();
		action = new GraphSimulationAgentAction();
	}
	
	
	/**
	 * Configures the agent according to the <code>AgentParameter</code> as parsed from the configuration file.
	 * <code>agentpar</code> must be an instance of <code>GraphSimulationAgentParameter</code>. The information
	 * included in <code>agentpar</code> is the name of the agent, the name of its role, and the name of its team
	 * 
	 * @param agentpar agent parameters to use
	 */
	public void setAgentParameter(AgentParameter agentpar) {
		
		super.setAgentParameter(agentpar);
		
		// get Team
		GraphSimulationAgentParameter graphPar = (GraphSimulationAgentParameter) agentpar;
		agentstate.team = graphPar.getTeam().toString();
		
		// get Username
		if (this.getAgent() instanceof UniqueSimulationAgent) {
			UniqueSimulationAgent agent = (UniqueSimulationAgent) this
					.getAgent();
			if (agent.getIdentifier() instanceof UsernamePasswordAccount) {
				UsernamePasswordAccount upa = (UsernamePasswordAccount) agent
						.getIdentifier();
				agentstate.name = upa.getUsername();
			} else {
				if (graphPar.name != null){
					agentstate.name = graphPar.name;
				} else {
					agentstate.name = "";
				}
			}
		} else {
			if (graphPar.name != null){
				agentstate.name = graphPar.name;
			} else {
				agentstate.name = "";
			}
		}
		agentstate.roleName = graphPar.roleName;
	}
	
	
	/**
	 * Initializes the agent internal values. The <code>AgentParameter</code> must have been set previously,
	 * by calling <code>setAgentParameter</code>.
	 * @param config the current configuration being used
	 */
	public void initialize(GraphSimulationConfiguration config) {
		
		RoleConfiguration roleConf = config.getRoleConf(agentstate.roleName);
		
		agentstate.maxEnergy = roleConf.maxEnergy;
		agentstate.maxEnergyDisabled = roleConf.maxEnergyDisabled;
		agentstate.energy = roleConf.maxEnergy;
		agentstate.maxHealth = roleConf.maxHealth;
		agentstate.health = roleConf.maxHealth;
		agentstate.strength = roleConf.strength;
		agentstate.visRange = roleConf.visRange;
		
		agentstate.lastAction = "skip";
		agentstate.lastActionResult = "successful";
		agentstate.param = "";
		
	}
	
	@Override
	public AgentState getAgentState() {
		return agentstate;
	}

	/**
	 * This method only calculates private agent perceptions. Shared perceptions must be added
	 * externally
	 */
	@Override
	public Perception createPerception(WorldState simstate,
			AgentState[] agentstates) {
		
		GraphSimulationWorldState worldState = (GraphSimulationWorldState) simstate;
		GraphSimulationAgentPerception perc = new GraphSimulationAgentPerception();
		
		perc.self = agentstate;
		perc.team = worldState.getTeamState(agentstate.team);
		perc.step = worldState.currentStep.intValue();

		// Add agent's node
		perc.nodes.add(agentstate.node);
		perc.agents.addAll(agentstate.node.agents);

		// Add nodes in visibility range
		LinkedList<GraphNode> thisStepNodes = new LinkedList <GraphNode>();
		LinkedList<GraphNode> prevStepNodes = new LinkedList <GraphNode>();
		prevStepNodes.add(agentstate.node);
		for (int i = 0; i < agentstate.visRange; i++) {

			for (GraphNode node : prevStepNodes) {

				// Add edges connecting nodes from previous step
				List<GraphEdge> neighborEdges = worldState.getConnectedEdges(node);
				for (GraphEdge edge : neighborEdges) {
					perc.edges.add(edge);
					
					GraphNode neighbor = opositeNode(edge, node);
					if (perc.nodes.add(neighbor)){
						thisStepNodes.add(neighbor);
						perc.agents.addAll(neighbor.agents);
					}
				}

			}			
			prevStepNodes = thisStepNodes;
			thisStepNodes = new LinkedList <GraphNode>();
		}

		
		// TODO merge this piece of code in previous loop? (with regular perceptions)
		if ("probe".equals(agentstate.lastAction) && "successful".equals(agentstate.lastActionResult) ){
			perc.probedNodes.add(agentstate.node);
		} else if ("survey".equals(agentstate.lastAction) && "successful".equals(agentstate.lastActionResult) ){
			List<GraphEdge> connectedEdges = worldState.getConnectedEdges(agentstate.node);
			perc.surveyedEdges.addAll(connectedEdges);
		} else if ("inspect".equals(agentstate.lastAction) && "successful".equals(agentstate.lastActionResult) ){
			for (GraphSimulationAgentState inspectedAgent : agentstate.node.agents) {
				if (!agentstate.team.equals(inspectedAgent.team)) {
					perc.inspectedAgents.add(inspectedAgent);
				}
			}
			for (GraphNode neighborNode: worldState.getNeighborNodes(agentstate.node)){
				for (GraphSimulationAgentState inspectedAgent : neighborNode.agents) {
					if (!agentstate.team.equals(inspectedAgent.team)) {
						perc.inspectedAgents.add(inspectedAgent);									
					}
				}
			}
		}
		return perc;
	}
	
	/**
	 * Returns the <code>GraphNode</code> of <code>edge</code> in the opposite side to <code>node</code>.
	 * If <code>node</code> is not one of the <code>edge</code> nodes, returns <code>null</code>
	 * @param edge
	 * @param node
	 */
	private GraphNode opositeNode(GraphEdge edge, GraphNode node) {
		if (node.equals(edge.node1)){
			return edge.node2;
		}		
		if (node.equals(edge.node2)){
			return edge.node1;
		}
		return null;
	}

	@Override
	public void processAction(Action a, WorldState simstate,
			AgentState[] agentstates) {
		// Do nothing
	}

	@Override
	public InitialStickyPerception createInitialPerception(WorldState simstate,
			AgentState[] agentstates) {

		GraphSimulationWorldState simulationstate = (GraphSimulationWorldState) simstate;
		GraphSimulationAgentInitialPerception perc = new GraphSimulationAgentInitialPerception();
		
		perc.self = agentstate;
		perc.steps = simulationstate.config.maxNumberOfSteps;
		perc.vertices = simulationstate.nodes.size();
		perc.edges = simulationstate.edges.size();
		
		perc.teamMembers = new Vector<GraphSimulationAgentState>();
		for (AgentState as : agentstates) {
			GraphSimulationAgentState otherAgent = (GraphSimulationAgentState)as;
			if (otherAgent.team.equals(agentstate.team) && !otherAgent.equals(agentstate)){
				perc.teamMembers.add(otherAgent);
			}
		}		
		return perc;
	}

	@Override
	public FinalPerception createFinalPerception(WorldState simstate,
			AgentState[] agentstates) {GraphSimulationWorldState simulationstate = (GraphSimulationWorldState) simstate;
		GraphSimulationAgentFinalPerception perc = new GraphSimulationAgentFinalPerception();
		perc.score = simulationstate.getTeamState(agentstate.team).summedScore;
		perc.ranking = simulationstate.getTeamState(agentstate.team).ranking;
		return perc;
	}


	/**
	 * Sets the action received from the client-side agent to the agent state, for execution in the current step.
	 * @param newAction
	 */
	public void setAction(Action newAction) {	
		if (newAction instanceof massim.framework.InvalidAction) {			
			//set invalid action
			this.action = new GraphSimulationAgentAction();
			this.action.type = "invalid";
			this.agentstate.action = action.type;
			this.agentstate.param = "";
		} 
		else if (newAction instanceof GraphSimulationAgentAction){
			//set action
			this.action = (GraphSimulationAgentAction) newAction;
			this.agentstate.action = action.type;
			this.agentstate.param = action.param;
		}	
	}



	
	

}
