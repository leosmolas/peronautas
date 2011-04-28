package massim.competition2011.scenario;

import java.io.Serializable;

/**
 * This class represents and Edge in the graph that forms the map.
 */
public class GraphEdge implements Serializable {

	private static final long serialVersionUID = -2064294215950641200L;
	
	public GraphNode node1;
	public GraphNode node2;
	public int weight;
	
	public GraphEdge(int weight, GraphNode node1, GraphNode node2) {
		
		this.weight = weight;
		this.node1 = node1;
		this.node2 = node2;
		
	}
	
	public float getLength() {
		
		return (float) Math.sqrt( (node1.x - node2.x) * (node1.x - node2.x) + (node1.y - node2.y) * (node1.y - node2.y) );
		
	}
	

}