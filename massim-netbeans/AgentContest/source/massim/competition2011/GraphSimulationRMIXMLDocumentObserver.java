package massim.competition2011;

import massim.competition2011.scenario.GraphEdge;
import massim.competition2011.scenario.GraphNode;
import massim.competition2011.scenario.TeamState;
import massim.framework.SimulationState;
import massim.framework.rmi.XMLDocumentObserver;
import massim.framework.simulation.SimulationStateImpl;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This RMIXMLDocumentObserver provides the simulation statistics for the webserver and the servermonitor.
 * 
 */
public class GraphSimulationRMIXMLDocumentObserver extends XMLDocumentObserver {
	
	/**
	 * Generates the XML representation of the simulation state to send to the observer. 
	 * @param doc The document to which the XML is generated.
	 * @param simstate The current simulation state.
	 */
	public void generateDocument(Document doc, SimulationState simstate) {
	
		Element el_root = doc.getDocumentElement();
		SimulationStateImpl simplestate = (SimulationStateImpl) simstate;
		GraphSimulationWorldState worldState = (GraphSimulationWorldState) simplestate.simulationState;
		
		// String[] teamName = new String[2];
		// teamName[0] = worldState.teamName[0];
		// teamName[1] = worldState.teamName[1];
		
		Element elState = doc.createElement("state");
		elState.setAttribute("simulation", worldState.simulationName);
		// el_state.setAttribute("teamname0", teamName[0]);
		// el_state.setAttribute("teamname1", teamName[1]);
		elState.setAttribute("step", Integer.toString(simplestate.steps));

		// el_state.setAttribute("number-of-agents", Integer.toString(worldState.numberOfAgents));
		elState.setAttribute("number-of-steps",""+worldState.maxNumberOfSteps);	
		
		
		Element elVertices = doc.createElement("vertices");		
		for (GraphNode node : worldState.nodes) {
			Element elVertex = doc.createElement("vertex");
			elVertex.setAttribute("name", node.name);
			elVertex.setAttribute("gridX", String.valueOf(node.gridX));
			elVertex.setAttribute("gridY",  String.valueOf(node.gridY));
			elVertex.setAttribute("x",  String.valueOf(node.x));
			elVertex.setAttribute("y",  String.valueOf(node.y));
			elVertex.setAttribute("weight", String.valueOf(node.weight));
			String dominator = node.getDominatorTeam();
			if (dominator != null){
				elVertex.setAttribute("dominatorTeam", dominator );
			}
			
			// Agents
			Element elAgents = doc.createElement("entities");		
			for (GraphSimulationAgentState agent : node.agents) {
				Element elAgent = doc.createElement("entity");
				elAgent.setAttribute("name", agent.name);
				elAgent.setAttribute("team", agent.team);
				elAgent.setAttribute("node", agent.node.name);
				elAgent.setAttribute("roleName", agent.roleName);
				elAgent.setAttribute("strength", String.valueOf(agent.strength));
				elAgent.setAttribute("maxEnergy", String.valueOf(agent.maxEnergy));
				elAgent.setAttribute("maxEnergyDisabled", String.valueOf(agent.maxEnergyDisabled));
				elAgent.setAttribute("health", String.valueOf(agent.health));
				elAgent.setAttribute("maxHealth", String.valueOf(agent.maxHealth));
				elAgent.setAttribute("energy", String.valueOf(agent.energy));
				elAgent.setAttribute("visRange", String.valueOf(agent.visRange));
				elAgent.setAttribute("lastAction", agent.lastAction);
				elAgent.setAttribute("lastActionParam", agent.lastActionParam);
				elAgent.setAttribute("lastActionResult", agent.lastActionResult);
				elAgents.appendChild(elAgent);
			}
			elVertex.appendChild(elAgents);	
			
			
			elVertices.appendChild(elVertex);
		}
		elState.appendChild(elVertices);
		
		Element elEdges = doc.createElement("edges");		
		for (GraphEdge edge : worldState.edges) {
			Element elEdge = doc.createElement("edge");
			elEdge.setAttribute("node1", edge.node1.name);
			elEdge.setAttribute("node2", edge.node2.name);
			elEdge.setAttribute("weight", String.valueOf(edge.weight));
			elEdges.appendChild(elEdge);
		}
		elState.appendChild(elEdges);
		
		
		Element elTeams = doc.createElement("teams");		
		for (TeamState team : worldState.teamsStates) {
			// Info about team		
			Element elTeam = doc.createElement("team");
			elTeam.setAttribute("name", team.name);
			elTeam.setAttribute("score", String.valueOf(team.summedScore));
			elTeam.setAttribute("achievementPoints", String.valueOf(team.currAchievementPoints));
			elTeam.setAttribute("usedAchievementPoints", String.valueOf(team.usedAchievementPoints));
			elTeam.setAttribute("stepScore", String.valueOf(team.getCurrent()));
			elTeam.setAttribute("zonesScore", String.valueOf(team.getAreasValue()));
			
			// Achievements
			Element elAchievements = doc.createElement("achievements");		
			for (String achievementName : team.getAchieved()) {
				Element elAchievement = doc.createElement("achievement");
				elAchievement.setAttribute("name", achievementName);
				elAchievements.appendChild(elAchievement);
			}
			elTeam.appendChild(elAchievements);
			
			// ProvedNodes
			Element elProved = doc.createElement("provedNodes");		
			for (String nodeName : team.getProbedNodes()) {
				Element elNode = doc.createElement("node");
				elNode.setAttribute("name", nodeName);
				elProved.appendChild(elNode);
			}
			elTeam.appendChild(elProved);
	
			elTeams.appendChild(elTeam);
		}
		elState.appendChild(elTeams);
		
		el_root.appendChild(elState);		
	}
	
}
