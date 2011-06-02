package massim.competition2011.scenario;

import java.util.Vector;

/**
 * This abstract class should be overridden by the classes that will be used to generate different variations
 * of graphs (maps).
 */
public abstract class GraphGenerator {

	/**
	 * Generates a graph.
	 * @param nodes an empty <code>Vector&lt;GraphNode&gt;</code> where the nodes of the new graph shall be added.
	 * @param edges an empty <code>Vector&lt;GraphEdge&gt;</code> where the edges of the new graph shall be added.
	 * @param nodesNum Number of node that the generated graph will have.
	 * @param gridWidth 
	 * @param gridHeight
	 * @param cellWidth
	 * @param minNodeWeight
	 * @param maxNodeWeight
	 * @param minEdgeCost
	 * @param maxEdgeCost
	 */
	public abstract void generate(Vector<GraphNode> nodes, Vector<GraphEdge> edges,
			int nodesNum, int gridWidth, int gridHeight, int cellWidth,
			int minNodeWeight, int maxNodeWeight, int minEdgeCost, int maxEdgeCost);
	
}
