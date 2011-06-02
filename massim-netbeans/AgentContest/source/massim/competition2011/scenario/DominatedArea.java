package massim.competition2011.scenario;

import java.io.Serializable;
import java.util.Collection;

import massim.competition2011.GraphSimulationAgentState;

/**
 * This class holds information about a Dominated Area, i.e. a connected part of 
 * the graph that belongs to a team according to the <code>DominationCalculator</code>.
 */
public class DominatedArea implements Serializable{

	private static final long serialVersionUID = -5265576592655218606L;
	
	public String team;
	public int size;
	public int totalScore;
	public Collection<GraphSimulationAgentState> agents;
	public Collection<GraphNode> nodes;
	
	/**
	 * 
	 * @param team
	 * @param agents
	 * @param nodes
	 * @param size
	 * @param score
	 */
	public DominatedArea(String team,
			Collection<GraphSimulationAgentState> agents,
			Collection<GraphNode> nodes,
			int size, int score) {
		super();
		this.team = team;
		this.size = size;
		this.totalScore = score;
		this.agents = agents;
		this.nodes = nodes;
	}
	
	/**
	 * Returns the score of the area, summing the full weight of each node in the area that has been probed,
	 * and 1 for every node in the area that hasn't been probed yet.
	 * @param probedNodes The collection of nodes that the team has probed.
	 * @return The score of the area.
	 */
	public int calculateProbedScore(Collection<String> probedNodes){
		int score = 0;
		for (GraphNode node : nodes) {
			if (probedNodes.contains(node.name)){
				score += node.weight;
			} else {
				score ++;
			}
		}
		return score;
	}
	
	/**
	 * @param ag
	 * @return <code>true</code> iff the agent <code>ag</code> is on one of the nodes conforming the area.
	 */
	public boolean cointainsAgent(GraphSimulationAgentState ag){
		return agents.contains(ag);
	}
	

}
