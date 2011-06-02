package massim.competition2011.scenario;

import java.io.Serializable;
import java.util.Vector;

import massim.competition2011.GraphSimulationAgentState;

/**
 * This class represents a node in the graph that forms the map.
 */
public class GraphNode implements Serializable {
	
	private static final long serialVersionUID = -6260869513436004934L;
	
	public static final String NODE_NAME_PREFIX = "vertex";
	
	public String name;
	public int x;
	public int y;
	public int weight;
	public Vector<GraphSimulationAgentState> agents;
	private String dominatorTeam = null;
	
	public int gridX;
	public int gridY;
	
	public GraphNode(int nameNr, int weight, int gridX, int gridY, int x, int y) {
		
		this.name = NODE_NAME_PREFIX + nameNr;
		this.weight = weight;
		this.x = x;
		this.y = y;
		this.gridX = gridX;
		this.gridY = gridY;
		agents = new Vector<GraphSimulationAgentState>();
			
	}
	
	
	/**
	 * Two nodes are considered equal if they have the same name or the same raw coordinates.
	 */
	public boolean equals(Object obj) {
		
		if ( obj == this )
			return true;
		
		if ( obj == null )
			return false;
		
		if( (obj instanceof GraphNode) == false ) 
			return false;
		
		GraphNode n = (GraphNode)obj;
		
		// Use raw coordinates here to avoid repeating nodes in the generation algorithms.
		return ( (this.name.equals(n.name)) || (this.x == n.x && this.y == n.y) );
		
	}

	/**
	 * @return the dominatorTeam
	 */
	public String getDominatorTeam() {
		return dominatorTeam;
	}

	/**
	 * @param dominatorTeam the dominatorTeam to set
	 */
	public void setDominatorTeam(String dominatorTeam) {
		this.dominatorTeam = dominatorTeam;
	}



	
}
