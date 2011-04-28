package massim.competition2011;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;

import massim.competition2011.scenario.GraphEdge;
import massim.competition2011.scenario.GraphGenerator;
import massim.competition2011.scenario.GraphGeneratorTriangulation;
import massim.competition2011.scenario.GraphNode;
import massim.competition2011.scenario.TeamState;
import massim.framework.simulation.WorldState;
import massim.gridsimulations.SimulationWorldState;

/**
 * Holds the current state of a graph simulation (2011 Mars Scenario)
 */
public class GraphSimulationWorldState extends SimulationWorldState implements
		WorldState {


	private static final long serialVersionUID = -6316439899157240323L;
		
	// World model values
	/**
	 * The nodes conforming the map graph.
	 */
	protected Vector<GraphNode> nodes;
	
	/**
	 * The edges conforming the map graph.
	 */
	protected Vector<GraphEdge> edges;
	
	/**
	 * A map holding the list of nodes directly connected to each node.
	 * It is used as a cache to provide fast access to this information that is heavily accessed
	 * during the simulation execution.
	 */
	protected Map<GraphNode, List<GraphNode>> neighborsMap;
	
	/**
	 * A map holding the list of edges connected to each node.
	 * It is used as a cache to provide fast access to this information that is heavily accessed
	 * during the simulation execution.
	 */
	protected Map<GraphNode, List<GraphEdge>> connectedEdgesMap;
	
	/**
	 * A map from an agent's name to its current state.
	 * It is used as a cache to provide fast access to this information that is heavily accessed
	 * during the simulation execution.
	 */
	protected Map<String, GraphSimulationAgentState> agentNamesMap;
	
	/**
	 * A map from a node's name to its current state.
	 * It is used as a cache to provide fast access to this information that is heavily accessed
	 * during the simulation execution.
	 */
	protected Map<String, GraphNode> nodeNamesMap;
	
	
	
	/**
	 * A vector holding all agents that take part in the simulation.
	 */
	protected Vector<GraphSimulationAgentState> agents;
	
	
	/**
	 * The configuration of this simulation.
	 */
	protected GraphSimulationConfiguration config;
	
	/**
	 * A vector holding the states of all the teams that take part in the simulation.
	 */
	public Vector<TeamState> teamsStates;
	
	/**
	 * The width of the abstract grid to which the graph is subscribed.
	 */
	protected int sizeX;
	
	/**
	 * The height of the abstract grid to which the graph is subscribed. 
	 */
	protected int sizeY;

	// Used for randomly situating the agents in the map.
	private Random random;
	
	/**
	 * Creates a simulation state as defined by <code>config</code>
	 * @param config
	 */
	public GraphSimulationWorldState(GraphSimulationConfiguration config) {
		simulationName = config.simulationName;
		nodes = new Vector<GraphNode>();
		edges = new Vector<GraphEdge>();
		agents = new Vector<GraphSimulationAgentState>();
		agentNamesMap = new HashMap<String, GraphSimulationAgentState>();
		nodeNamesMap = new HashMap<String, GraphNode>();
		

		teamsStates = new Vector<TeamState>();
		
		this.config = config;
		
		
		// Check whether the number of nodes fits the grid, to avoid an infinite loop when creating the graph
		int numberOfNodes;
		if ( config.numberOfNodes > (config.gridWidth + 1) * (config.gridHeight + 1) ){
			numberOfNodes = (config.gridWidth + 1) * (config.gridHeight + 1);
		} else {
			numberOfNodes = config.numberOfNodes;
		}
		
		GraphGenerator generator = new GraphGeneratorTriangulation();
		generator.generate(nodes, edges, numberOfNodes, config.gridWidth, config.gridHeight,
				config.cellWidth, config.minNodeWeight, config.maxNodeWeight,
				config.minEdgeCost, config.maxEdgeCost);
		populateMapsCahes();
		
		sizeX = config.cellWidth * (config.gridWidth + 1);
		sizeY = config.cellWidth * (config.gridHeight + 1);
	}
	
	/**
	 * Initialize the maps used as cahced for faster data access.
	 */
	protected void populateMapsCahes(){
		neighborsMap = new HashMap<GraphNode, List<GraphNode>>(nodes.size());
		connectedEdgesMap = new HashMap<GraphNode, List<GraphEdge>>(nodes.size());
		for (GraphNode node: nodes){
			neighborsMap.put(node, new ArrayList<GraphNode>());
			connectedEdgesMap.put(node, new ArrayList<GraphEdge>());
			nodeNamesMap.put(node.name, node);
		}
		
		for (GraphEdge edge: edges){
			neighborsMap.get(edge.node1).add(edge.node2);
			neighborsMap.get(edge.node2).add(edge.node1);
			connectedEdgesMap.get(edge.node1).add(edge);
			connectedEdgesMap.get(edge.node2).add(edge);
		}
	}

	/**
	 * getter for the vector holding all agents that take part in the simulation.
	 * @return
	 */
	public Vector<GraphSimulationAgentState> getAgents() {
		return agents;
	}

	/**
	 * setter for the vector holding all agents that take part in the simulation.
	 * @param agents
	 */
	public void setAgents(Vector<GraphSimulationAgentState> agents) {
		this.agents = agents;
	}
	
	/**
	 * getter for the configuration object.
	 * @return
	 */
	public GraphSimulationConfiguration getConfig(){
		return config;
	}

	/**
	 * Returns the state of an agent given its name.
	 * @param agentName
	 * @return
	 */
	public GraphSimulationAgentState getAgent(String agentName) {
		return agentNamesMap.get(agentName);
	}

	/**
	 * Returns the node object representation given its name.
	 * @param nodeName
	 * @return
	 */
	public GraphNode getNode(String nodeName) {
		return nodeNamesMap.get(nodeName);
	}

	/**
	 * Returns the list of the nodes directly connected to <code>node</code>
	 * @param node
	 * @return
	 */
	public List<GraphNode> getNeighborNodes(GraphNode node){
		return Collections.unmodifiableList(neighborsMap.get(node));
	}
	
	/**
	 * Returns the list of the edges connected to <code>node</code>
	 * @param node
	 * @return
	 */
	public List<GraphEdge> getConnectedEdges(GraphNode node){		
		return Collections.unmodifiableList(connectedEdgesMap.get(node));
	}

	/**
	 * getter for the vector holding all nodes of the map.
	 * @return
	 */
	public Collection<GraphNode> getNodes() {
		return nodes;
	}

	/**
	 * Adds <code>agent</code> to the currently simulation, and situates it in a random node in the map.
	 * @param agent
	 */
	public void addAgent(GraphSimulationAgentState agent) {		
		int index = getRandom().nextInt(nodes.size());
		GraphNode node = nodes.get(index);
		node.agents.add(agent);
		agent.setNode(node);
		agents.add(agent);
		agentNamesMap.put(agent.name, agent);
		
		if (getTeamNr(agent.team) == -1){
			int idx = teamsStates.size();
			teamsStates.add(new TeamState(agent.team, idx));
		}		
	}
	
	/**
	 * Returns a numeric representation of the team name, -1 if the team has not been added
	 * to the teams list of the simulation.
	 * 
	 * The number representation is arbitrary, possibly affected by the order in which teams are added,
	 * and should remain for the duration of the match.
	 * 
	 * @param name
	 * @return
	 */
	// TODO stick to random team numbering?
	public int getTeamNr(String name) {
		TeamState ts = getTeamState(name);
		return ts != null?ts.teamIdx:-1;
	}
	
	/**
	 * Provides a numeric representation of the team name, null if the number does not correspond to any
	 * team.
	 * 
	 * The number representation is arbitrary, possibly affected by the order in which teams are added,
	 * and should remain for the duration of the match.
	 * 
	 * @param name
	 * @return
	 */
	// TODO stick to random team numbering?
	public String getTeamName(int number) {
		if (number >= 0 && number < teamsStates.size()) {
			assert (teamsStates.get(number).teamIdx == number);
			return teamsStates.get(number).name;
		}
		return null;
	}
	
	/**
	 * Returns the state of team given its name.
	 * @param name
	 * @return
	 */
	public TeamState getTeamState(String name){
		for(TeamState ts: teamsStates){
			if (ts.name.equals(name)){
				return ts;
			}
		}		
		return null;
	}
	
	/**
	 * returns the <code>Random</code> object. It always returns the same object
	 * to avoid creating new ones and initializing them with the same seed in
	 * successive calls that are performed to close together, resulting in repetition
	 * of the random number generated.
	 * @return
	 */
	private Random getRandom(){
		if (random == null){
			random = new Random(System.currentTimeMillis());
		}		
		return random;
	}

}
