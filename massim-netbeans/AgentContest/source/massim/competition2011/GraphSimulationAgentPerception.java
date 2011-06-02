package massim.competition2011;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import massim.competition2011.scenario.GraphEdge;
import massim.competition2011.scenario.GraphNode;
import massim.competition2011.scenario.TeamState;
import massim.framework.Perception;
import massim.framework.util.XMLCodec;

import org.w3c.dom.Element;


/**
 * This class holds the information that will be sent to an agent as a normal perception.
 */
public class GraphSimulationAgentPerception implements Perception,
		XMLCodec.XMLEncodable{

	private static final long serialVersionUID = -5791498406627754380L;
	
	/**
	 * The state of the agent to which this perception belongs.
	 */
	public GraphSimulationAgentState self;
	
	/**
	 * The state of the team of the agent owner of this perception.
	 */
	public TeamState team;
	
	
	// We use sets here to avoid checking for duplicates every time we add elements.	
	/**
	 * The set of nodes that are currently visible to the agent
	 */
	public Set<GraphNode> nodes;
	
	/**
	 * The set of edges that are currently visible to the agent
	 */
	public Set<GraphEdge> edges;
	
	/**
	 * The set of agent that are currently visible to the agent
	 */
	public Set<GraphSimulationAgentState> agents;
	
	/**
	 * The set of nodes that where probed in the previous step, for which the
	 * agent owner of this perception should get extended information.
	 */
	public Set<GraphNode> probedNodes;
	
	/**
	 * The set of edges that where surveyed in the previous step, for which the
	 * agent owner of this perception should get extended information.
	 */
	public Set<GraphEdge> surveyedEdges;
	
	/**
	 * The set of agents that where inspected in the previous step, for which the
	 * agent owner of this perception should get extended information.
	 */
	public Set<GraphSimulationAgentState> inspectedAgents;

	/**
	 * The current simulation step
	 */
	public int step;
	
	
	/**
	 * Constructs for an empty perception.
	 */
	public GraphSimulationAgentPerception(){
		nodes = new HashSet<GraphNode>();
		edges = new HashSet<GraphEdge>();
		agents = new HashSet<GraphSimulationAgentState>();
		probedNodes = new HashSet<GraphNode>();
		surveyedEdges = new HashSet<GraphEdge>();
		inspectedAgents = new HashSet<GraphSimulationAgentState>();
		step = 0;
		self = null;
		team = null;		
	}
	
	/**
	 * Encodes the contents of this perception object in the right XML format,
	 * in concordance to the protocol description.
	 */
	public void encodeToXML(Element target) {
		
		Element elSimulation = target.getOwnerDocument()
				.createElement("simulation");
		elSimulation.setAttribute("step", String.valueOf(step));
		target.appendChild(elSimulation);
		
		// Info about self
		Element elSelf = target.getOwnerDocument().createElement("self");
//		elSelf.setAttribute("name", self.name);
//		elSelf.setAttribute("team", self.team);
		elSelf.setAttribute("position", self.node.name);
		elSelf.setAttribute("strength", String.valueOf(self.strength));
		elSelf.setAttribute("energy", String.valueOf(self.energy));
		elSelf.setAttribute("maxEnergy", String.valueOf(self.maxEnergy));
		elSelf.setAttribute("maxEnergyDisabled", String.valueOf(self.maxEnergyDisabled));		
		elSelf.setAttribute("health", String.valueOf(self.health));
		elSelf.setAttribute("maxHealth", String.valueOf(self.maxHealth));
		elSelf.setAttribute("visRange", String.valueOf(self.visRange));
		elSelf.setAttribute("lastAction", String.valueOf(self.lastAction));
		elSelf.setAttribute("lastActionResult", String.valueOf(self.lastActionResult));
		elSelf.setAttribute("zoneScore", String.valueOf(team.getAreaValue(self)));
		target.appendChild(elSelf);
		
		// Info about team		
		Element elTeam = target.getOwnerDocument().createElement("team");
//		elTeam.setAttribute("name", team.name);
		elTeam.setAttribute("score", String.valueOf(team.summedScore));
		elTeam.setAttribute("money", String.valueOf(team.currAchievementPoints));
//		elTeam.setAttribute("usedMoney", String.valueOf(team.usedAchievementPoints));
		elTeam.setAttribute("lastStepScore", String.valueOf(team.getCurrent()));
		elTeam.setAttribute("zonesScore", String.valueOf(team.getAreasValue()));
		
		// Achievements
		Vector<String> achievementsVector = team.getNewlyAchieved();
		if (achievementsVector.size() > 0){
			Element elAchievements = target.getOwnerDocument()
					.createElement("achievements");	
			for (String achievementName : achievementsVector) {
				Element elAchievement = target.getOwnerDocument()
						.createElement("achievement");
				elAchievement.setAttribute("name", achievementName);
				elAchievements.appendChild(elAchievement);
			}
			elTeam.appendChild(elAchievements);
		}

		target.appendChild(elTeam);
		
		
		// Visible vertices
		Element elVertices = target.getOwnerDocument().createElement("visibleVertices");		
		for (GraphNode node : nodes) {
			Element elVertex = target.getOwnerDocument()
					.createElement("visibleVertex");
			elVertex.setAttribute("name", node.name);
			elVertex.setAttribute("team", (node.getDominatorTeam()==null?"none":node.getDominatorTeam()));
			elVertices.appendChild(elVertex);
		}
		target.appendChild(elVertices);
		
		// Visible edges
		Element elEdges = target.getOwnerDocument().createElement("visibleEdges");		
		for (GraphEdge edge : edges) {
			Element elEdge = target.getOwnerDocument()
					.createElement("visibleEdge");
			elEdge.setAttribute("node1", edge.node1.name);
			elEdge.setAttribute("node2", edge.node2.name);
			elEdges.appendChild(elEdge);
		}
		target.appendChild(elEdges);
		
		// Visible entities
		if (agents.size() > 0){
			Element elAgents = target.getOwnerDocument().createElement("visibleEntities");		
			for (GraphSimulationAgentState agent : agents) {
				Element elAgent = target.getOwnerDocument()
						.createElement("visibleEntity");
				elAgent.setAttribute("name", agent.name);
				elAgent.setAttribute("team", agent.team);
				elAgent.setAttribute("node", agent.node.name);
				elAgent.setAttribute("status", agent.health > 0 ? "normal": "disabled");
				elAgents.appendChild(elAgent);
			}
			target.appendChild(elAgents);
		}
		
		// Probed vertices
		if (probedNodes.size() > 0){
			Element elProbedVertices = target.getOwnerDocument().createElement("probedVertices");		
			for (GraphNode node : probedNodes) {
				Element elVertex = target.getOwnerDocument()
						.createElement("probedVertex");
				elVertex.setAttribute("name", node.name);
				elVertex.setAttribute("value", String.valueOf(node.weight));
				elProbedVertices.appendChild(elVertex);
			}
			target.appendChild(elProbedVertices);
		}
		
		// Surveyed edges
		if (surveyedEdges.size() > 0){
			Element elSurveyedEdges = target.getOwnerDocument().createElement("surveyedEdges");		
			for (GraphEdge edge : surveyedEdges) {
				Element elEdge = target.getOwnerDocument()
						.createElement("surveyedEdge");
				elEdge.setAttribute("node1", edge.node1.name);
				elEdge.setAttribute("node2", edge.node2.name);
				elEdge.setAttribute("weight", String.valueOf(edge.weight));
				elSurveyedEdges.appendChild(elEdge);
			}
			target.appendChild(elSurveyedEdges);
		}
		
		// Inspected entities
		if (inspectedAgents.size() > 0){
			Element elInspectedAgents = target.getOwnerDocument().createElement("inspectedEntities");		
			for (GraphSimulationAgentState agent : inspectedAgents) {
				Element elAgent = target.getOwnerDocument()
						.createElement("inspectedEntity");
				elAgent.setAttribute("name", agent.name);
				elAgent.setAttribute("team", agent.team);
				elAgent.setAttribute("node", agent.node.name);
				elAgent.setAttribute("role", agent.roleName);
				elAgent.setAttribute("strength", String.valueOf(agent.strength));
				elAgent.setAttribute("maxEnergy", String.valueOf(agent.maxEnergy));
				elAgent.setAttribute("health", String.valueOf(agent.health));
				elAgent.setAttribute("maxHealth", String.valueOf(agent.maxHealth));
				elAgent.setAttribute("energy", String.valueOf(agent.energy));
				elAgent.setAttribute("visRange", String.valueOf(agent.visRange));
				elInspectedAgents.appendChild(elAgent);
			}
			target.appendChild(elInspectedAgents);
		}

	}
	
	/**
	 * Adds all the perceived elements of <code>sharedPer</code> to the perceived elements of this perception.<br>
	 * <br>
	 * The main purpose of this method is to facilitate the management of shared perceptions: First, an empty
	 * perception object is created, and this method is called on it repeatedly to add private perceptions that
	 * are to be shared.<br>
	 * Then, this method is called on the private perception objects, passing the now populated shared perceptions
	 * object.
	 * 
	 * @param sharedPer
	 */
	public void addSharedPercept(GraphSimulationAgentPerception sharedPer){
		nodes.addAll(sharedPer.nodes);
		edges.addAll(sharedPer.edges);
		agents.addAll(sharedPer.agents);
		probedNodes.addAll(sharedPer.probedNodes);
		surveyedEdges.addAll(sharedPer.surveyedEdges);
		inspectedAgents.addAll(sharedPer.inspectedAgents);
	}
	
	
	
}
