package massim.competition2011.scenario;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import massim.competition2011.GraphSimulationAgentState;
import massim.competition2011.GraphSimulationWorldState;


/**
 * This class is in charge of calculating who is dominating nodes and zones. After calculation, it updates this
 * information in the nodes themselves and in the teams information.
 */
public class DominationCalculator {
	
	public static void calculate(GraphSimulationWorldState world){
		Collection<GraphNode> nodes = world.getNodes();
		
		// First step: basic domination
		for (GraphNode node: nodes){			
			calculateBasicNodeDomination(node,world);			
		}
		
		// Second step: neighbor domination
		Map<GraphNode,String> m = new HashMap<GraphNode,String>();
		
		for (GraphNode node:nodes){
			if ((node.getDominatorTeam() == null) && isEmptyOfActiveAgents(node)){
				String domin = calculateDominatorFromNeighbours(node,world);
				if (domin != null){
					m.put(node,domin);
				}
			}
		}
		
		for (GraphNode node : m.keySet()){
			node.setDominatorTeam(m.get(node));
		}
		
		
		
		// third step: isolated zone domination
		ArrayList<GraphNode> emptyNodesList = getEmptyNodesList(nodes);
		HashSet<GraphNode> nodesChecked = new HashSet<GraphNode>();
		
		while (!emptyNodesList.isEmpty()){
			
			HashSet<GraphNode> subgraphNodes;
			ArrayList<GraphNode> nodesToCheck;
			boolean isolated = true;
			String dominatorTeam = null;
			
			GraphNode node = emptyNodesList.remove(0);
			subgraphNodes = new HashSet<GraphNode>();			
			nodesToCheck = new ArrayList<GraphNode>();
			nodesToCheck.add(node);
			
			while (!nodesToCheck.isEmpty()){
				node = nodesToCheck.remove(0);
				subgraphNodes.add(node);
				List<GraphNode> neighbors = world.getNeighborNodes(node);
				for (GraphNode neighbor: neighbors){
					if (neighbor.getDominatorTeam() == null && isEmptyOfActiveAgents(neighbor)){
						if (!nodesChecked.contains(neighbor) && !nodesToCheck.contains(neighbor)){
							nodesToCheck.add(neighbor);
							emptyNodesList.remove(neighbor);
						}
					}
					else if (isolated && neighbor.getDominatorTeam() != null 
							&& (dominatorTeam == null || neighbor.getDominatorTeam().equals(dominatorTeam))){
						dominatorTeam = neighbor.getDominatorTeam();
					}
					else {
						isolated = false;
						dominatorTeam = null;
					}
				}
				nodesChecked.add(node);
			}
			
			if (isolated && dominatorTeam != null){
				for (GraphNode isolatedNode: subgraphNodes){			
					isolatedNode.setDominatorTeam(dominatorTeam);			
				}
			}
		}
		
		
		// Now compute the zones that have been created, and add them to the world state.
		// First clear previous step information
		for (TeamState ts : world.teamsStates){
			ts.areas.clear();
		}
		
		LinkedList<GraphSimulationAgentState> allAgents =
				new LinkedList<GraphSimulationAgentState>(world.getAgents()); //Agents to review
		
		while (!allAgents.isEmpty()){
			
			
			GraphSimulationAgentState agent = allAgents.removeFirst();
			String team =agent.team;
			
			if (team.equals(agent.node.getDominatorTeam())) {
				
				int score = 0;
				
				LinkedList<GraphSimulationAgentState> zoneAgents =
						new LinkedList<GraphSimulationAgentState>();
				LinkedList<GraphNode> zoneNodes = new LinkedList <GraphNode>();
				LinkedList<GraphNode> nodesToCheck = new LinkedList <GraphNode>();
				
				zoneNodes.add(agent.node);
				nodesToCheck.add(agent.node); // this nodes always belong to the zone
				
				while(!nodesToCheck.isEmpty()){
					GraphNode node = nodesToCheck.removeFirst();
					score += node.weight;
					
					// If node has agents, add all agents from team
					for (GraphSimulationAgentState agentInNode : node.agents) {
						if (team.equals(agentInNode.team)){
							// Agent belongs to zone
							zoneAgents.add(agentInNode);							
						}
						allAgents.remove(agentInNode);
					}
	
					for (GraphNode neighbor : world.getNeighborNodes(node)) {
						if (neighbor.getDominatorTeam() == team
								&& !zoneNodes.contains(neighbor)) {
							// Node belongs to zone
							zoneNodes.add(neighbor);
							nodesToCheck.add(neighbor);
						}	
					}
				}
				
				if (zoneNodes.size() > 1) {
					DominatedArea da = new DominatedArea(agent.team,
							new ArrayList<GraphSimulationAgentState>(zoneAgents),
							new ArrayList<GraphNode>(zoneNodes),
							zoneNodes.size(), score);
					world.getTeamState(agent.team).areas.add(da);
				}
			}
		}		
		// TODO maybe consider merging with node domination calculation?
	}
	
	protected static ArrayList<GraphNode> getEmptyNodesList(Collection<GraphNode> nodes) {
		ArrayList<GraphNode> freeNodes =  new ArrayList<GraphNode>();
		for (GraphNode node: nodes){			
			if (node.getDominatorTeam() == null && isEmptyOfActiveAgents(node)) {
				freeNodes.add(node);
			}
		}
		return freeNodes;
	}
	

	protected static void calculateBasicNodeDomination (GraphNode node, GraphSimulationWorldState world){
		int [] numAgents = new int[world.teamsStates.size()];
		int activeAgents = 0;
		for (int i = 0; i < numAgents.length; i++) {
			numAgents[i]=0;
		}
		
		for (GraphSimulationAgentState agent : node.agents){
			if (agent.health > 0){
				numAgents[world.getTeamNr(agent.team)]++;
				activeAgents++;
			}
		}
		
		int max = 0;
		int dominatorTeam=-1;			
		for (int i = 0; i < numAgents.length; i++) {
			if (numAgents[i] > max){
				dominatorTeam = i;
				max = numAgents[i];
			}
			else if (numAgents[i] == max){
				dominatorTeam=-1;
			}
		}
		if (dominatorTeam!=-1){
			if (max * 2 < activeAgents){
				dominatorTeam = -1;
			}
		}
		node.setDominatorTeam(world.getTeamName(dominatorTeam));
	}

	
	protected static String calculateDominatorFromNeighbours(GraphNode node, GraphSimulationWorldState world) {				
		int [] numNNodes = new int[world.getConfig().numberOfTeams];
		for (int i = 0; i < numNNodes.length; i++) {
			numNNodes[i]=0;
		}		
		List<GraphNode> neighbours = world.getNeighborNodes(node);
		//int size = neighbours.size();
		for (GraphNode neighbour : neighbours){
			if(neighbour.getDominatorTeam() != null){
				numNNodes[world.getTeamNr(neighbour.getDominatorTeam())]++;
			}
		}		
		int max = 0;
		int dominatorTeam=-1;			
		for (int i = 0; i < numNNodes.length; i++) {
			if (numNNodes[i] > max){
				dominatorTeam = i;
				max = numNNodes[i];
			}
			else if (numNNodes[i] == max){
				dominatorTeam=-1;
			}
		}		
		

		if (dominatorTeam != -1 && max < 2){
			dominatorTeam = -1;
		}
		
		return  world.getTeamName(dominatorTeam);		
	}
	
	protected static boolean isEmptyOfActiveAgents(GraphNode node){
		for (GraphSimulationAgentState agent : node.agents) {
			if (agent.health > 0){
				return false;
			}
		}
		return true;
	}

	
}
