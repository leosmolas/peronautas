package massim.competition2011.scenario;

import java.util.Collections;
import java.util.Comparator;
import java.util.Random;
import java.util.Vector;

/**
 * This class is a graph generation that uses an imperfect triangulation algorithm over a grid, with an ad-hoc
 * Heuristic to improve performance when generating big maps.
 *
 */
public class GraphGeneratorTriangulation extends GraphGenerator {

	@Override
	public void generate(Vector<GraphNode> nodes, Vector<GraphEdge> edges,
			int nodesNum, int gridWidth, int gridHeight, int cellWidth,
			int minNodeWeight, int maxNodeWeight, int minEdgeCost, int maxEdgeCost) {
		
		Random random = new Random(System.currentTimeMillis());
		
		int i = 0;
		while ( nodes.size() < nodesNum ) {

			int weight = minNodeWeight + random.nextInt(maxNodeWeight - minNodeWeight);
			int gridX = random.nextInt(gridWidth+1);
			int gridY = random.nextInt(gridHeight+1);			
			int x = gridX * cellWidth  + cellWidth/2;// + random.nextInt(10) - 5;
			int y = gridY * cellWidth + cellWidth/2;// + random.nextInt(10) - 5;
			
			GraphNode n = new GraphNode(i, weight, gridX, gridY,x,y);

			if ( !nodes.contains(n) ){
				nodes.add(n);
				i++;
			}
		
		}
		
		int c = 4;
		double limit = c *
				Math.sqrt(
					Math.pow( ((double)(cellWidth * gridHeight) / Math.sqrt(((double)gridHeight/(double)gridWidth)*(double)nodesNum)), 2) +
					Math.pow( ((double)(cellWidth * gridWidth) / Math.sqrt(((double)gridWidth/(double)gridHeight)*(double)nodesNum)), 2)
				);
		
		// create a full graph... the edges will be sorted by length
		Vector<GraphEdge> edgesFullSorted = new Vector<GraphEdge>();
		for ( int a = 0 ; a < nodes.size() ; a++ ) {
			
			for ( int b = a+1 ; b < nodes.size() ; b++ ) {
				
				GraphNode n1 = nodes.elementAt(a);
				GraphNode n2 = nodes.elementAt(b);
				
				int weight = minEdgeCost + random.nextInt(maxEdgeCost - minEdgeCost);
				GraphEdge e = new GraphEdge(weight,n1,n2);
				//edgesFull.add(e);
				
				
				
				//only add if shorter than some value
				if(e.getLength() < limit){	
					edgesFullSorted.add(e);
				}
			}
		}
		
		Collections.sort(edgesFullSorted,
						 new Comparator<GraphEdge>() {
							@Override
							public int compare(GraphEdge o1, GraphEdge o2) {
								if(o1.getLength() < o2.getLength()){
									return 1;
								} else if(o1.getLength() > o2.getLength()){
									return -1;
								} else {
									return 0;
								}
							}
						 });
		
		// filter remaining edges
		Vector<GraphEdge> remove = new Vector<GraphEdge>();
		for( int a=0 ; a < edgesFullSorted.size() ; a++ ) {
	
			// should be sorted descending
			if( a+1 != edgesFullSorted.size() )
			  assert edgesFullSorted.get(a).getLength() >= edgesFullSorted.get(a+1).getLength();

			// if intersects remove edge
			for( int b = a + 1 ; b < edgesFullSorted.size() ; b++ ) {
		
				// do not intersect adjacent edges
				GraphNode id1 = edgesFullSorted.get(a).node1;
				GraphNode id2 = edgesFullSorted.get(a).node2;
				GraphNode id3 = edgesFullSorted.get(b).node1;
				GraphNode id4 = edgesFullSorted.get(b).node2;
				
				if( id1.equals(id3)){
					if (checkSameLine(id1,id2,id4)){
						remove.add(edgesFullSorted.get(a));
						break; // stop looking for intersections
					} else {
						continue;
					}
				}
				if( id1.equals(id4)){
					if (checkSameLine(id1,id2,id3)){
						remove.add(edgesFullSorted.get(a));
						break; // stop looking for intersections
					} else {
						continue;
					}
				}
				if( id2.equals(id3)){
					if (checkSameLine(id2,id1,id4)){
						remove.add(edgesFullSorted.get(a));
						break; // stop looking for intersections
					} else {
						continue;
					}
				}
				if( id2.equals(id4)){
					if (checkSameLine(id2,id1,id3)){
						remove.add(edgesFullSorted.get(a));
						break; // stop looking for intersections
					} else {
						continue;
					}
				}

				// intersection algorithm stolen from:
				// http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
				double x1 = id1.x;
				double y1 = id1.y;
				double x2 = id2.x;
				double y2 = id2.y;					
				double x3 = id3.x;
				double y3 = id3.y;
				double x4 = id4.x;
				double y4 = id4.y;
				
				double ua = 
					((x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)) /
					((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1));
				double ub =
					((x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)) /
					((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1));
			
				boolean intersect = ua > 0.0f && ua < 1.0f && ub > 0.0f && ub < 1.0f; 
			
				if( intersect ) {
					
					remove.add(edgesFullSorted.get(a));
					break; // stop looking for intersections
					
				}
			}
			  
		}
		
		// remaining edges are the triangulation
		for( GraphEdge e : edgesFullSorted) {
			
			if( !remove.contains(e) ){
				edges.add(e);
			}
			
		}
	}
	
	private boolean checkSameLine(GraphNode commonNode, GraphNode secondary1, GraphNode secondary2) {
		double x1 = commonNode.x;
		double y1 = commonNode.y;
		double x2 = secondary1.x;
		double y2 = secondary1.y;
		double x3 = secondary2.x;
		double y3 = secondary2.y;
		
		return (x1 == x2 && x1 == x3 && (y2-y1)*(y3-y1)>0)
		    || (y1 == y2 && y1 == y3 && (x2-x1)*(x3-x1)>0)
		    || ((x2-x1)/(y2-y1) == (x3-x1)/(y3-y1) && (x2-x1)*(x3-x1)>0);
	}

	@Override
	public String toString() {
		return "Triangulation";
	}
	
	
	
	
}
