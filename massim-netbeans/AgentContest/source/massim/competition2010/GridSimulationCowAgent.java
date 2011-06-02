package massim.competition2010;

import java.io.Serializable;
import java.util.Collections;
import java.util.Vector;
import massim.cowsimulations.GridSimulationCell;

/**
 * This class is used to simulate cows moving at the same time. It contains the cow Movement Algorithm and some 
 * information about the specific cow.
 *
 */
public class GridSimulationCowAgent implements Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public int posx;
	public int posy;
	public int oldPosX;
	public int oldPosY;
	public String direction;
	public String old_direction;
	public boolean actionDone = true;
	public String ID;
	public int cowTurn;
	double old_vector[] = {0,0};
	GridSimulationWorldState w_state;
	
	
	
	public void computerCowMove2(GridSimulationWorldState state){
		w_state = state;
		
		Vector<Double> weight_vec = new Vector<Double>();
		Vector<int[]> pos_vec = new Vector<int[]>();
		double max_weight =-Double.MAX_VALUE;
		
		for(int i = -1 ; i<=1;i++){
			for(int j = -1 ; j<=1;j++){
				int x = posx+i;
				int y = posy+j;
				//we take care only about the empty cell around cow and cow's cell self
				if(x>=0 && y>=0 && x<state.sizex && y<state.sizey && 
						(state.board[x][y].freeCell()||(x == posx && y == posy)) ){
					int pos[] = {x,y};
					
					double weight = calcWeight(state.board[x][y],pos);
					
					weight_vec.add(weight);
					
					pos_vec.add(pos);
					
					if(max_weight<weight) 
						max_weight=weight;
				}
				
			}
		}
			
		int i = 0;
		 while (i<weight_vec.size()) {
		   if(weight_vec.get(i) < max_weight-state.epsilon){
		     pos_vec.remove(i);
		   weight_vec.remove(i);
		   } else {
		     i++;
		   }
		 }
		 
		Collections.shuffle(pos_vec);
		
		int move_pos[] = pos_vec.firstElement();
		
		direction = getDir(move_pos);
		
	}
	
	private String getDir(int[] move_pos) {
		String dir = "";
		if(move_pos[0] == posx && move_pos[1] == posy) 
			return "skip";
		
		if(move_pos[1]>posy)
			dir +="south";
		else if(move_pos[1]<posy)
			dir +="north";
		
		if(move_pos[0]> posx)
			dir +="east";
		else if(move_pos[0]<posx)
			dir +="west";
		return dir;
	}

/**
 * cell is a cell around the cow
 * pos_a is the coordinate of the cell 
 * @param cell
 * @param pos_a
 * @return
 */
	private double calcWeight(GridSimulationCell cell, int[] pos_a) {
		//empty cell has empty weight als base weight 
		//double base_weight = w_state.emptyWeight*1.0;
		double base_weight = 0.0;
		int cowSight = w_state.cowSight-1;
		for (int i = -cowSight; i <= cowSight; ++i){
			
			for (int j = -cowSight; j <= cowSight; ++j){
		
				int x = pos_a[0] +i; int y = pos_a[1] +j;
				if(x>=0 && y>=0 && x<w_state.sizex && y<w_state.sizey){
					
					int weight = cell_weight(w_state.board[x][y],x,y);
				
					int pos_b[] = {x,y};
					double dis = step_distance(pos_a, pos_b);
					if(dis != 0){
						
					double new_weight = weight/dis;
					base_weight += new_weight;
					}
				}
			}
		}
		return base_weight;
	}
	private double step_distance(int[] pos_a, int[] pos_b) {
		//step distance
		/*
		 * we map pos_a and pos_b in a new coordinate system where pos_a is the origin (0,0)
		 * 
		 */
		int step = 0;
		int x = pos_b[0] - pos_a[0];
		int y = pos_b[1] - pos_a[1];
		/**
		 * convert pos_b in positive coordinate 
		 */
		x = Math.abs(x);
		y = Math.abs(y);
		
		/*
		 * push pos_b to the origin
		 */
		while(true){
			if(x == 0) {step += y ; break;}
			if(y == 0) {step += x ; break;}
			x = x-1;
			y = y-1;
			step = step +1;
		}
		return step*1.0;
	}
	
	private int cell_weight(GridSimulationCell cell, int x, int y) {
		int w= w_state.emptyWeight;
		if(cell.agent) 
			w= w_state.agentWeight;
		
		else if(cell.cow){
		
			if(x == posx && y == posy) 
				w = w_state.emptyWeight;
			
			else{
				
				int a = x-posx ; int b = y - posy;
				int max = Math.max(Math.abs(a), Math.abs(b));
				
				if(max < w_state.cowPrivateField) 
					w= w_state.cowScareWeight;
				else 
					w = w_state.cowAttractedWeight;	
			}
		}
		else if((cell.fence && !cell.open)|| cell.obstacle || cell.switcher)
			w = w_state.obstacleWeight;
		
		return w;
	}
	
	public double[] calcVec(int x, int y, GridSimulationCell cell, double[] vec, GridSimulationWorldState state)
	{
		double length = Math.sqrt( x*x + y*y);
		if (length == 0)
		{
			// if the length is zero, the current cell is the cell, the cow is standing in.
			// this cell has no effect on the vector
			// this way a division through zero bug is prevented
			return vec;
		}
		else
		{
		double y1,x1;
		x1 = x / length;
		y1 = y / length;

		// maxXY is important for the private range detection
		
		int maxXY = Math.max(Math.abs(x), Math.abs(y));
		double theWeight = weight(maxXY, cell, state);
		
		x1 = x1*theWeight;///sumxy;
		y1 = y1*theWeight;///sumxy;
		
		//System.out.println(x1);
		//System.out.println(y1);
		
		vec[0] = vec[0] + x1;
		vec[1] = vec[1] + y1;
		return vec;
		}
	}
	
	public double weight(int max, GridSimulationCell cell, GridSimulationWorldState state)
	{
		double weight = 1.0;
		if (cell.agent)
		{
			weight = state.agentWeight*1.0;
			
			
		}
		else if (cell.cow)
		
		{
			if (max <= state.cowPrivateField)
			{
				weight = state.cowScareWeight * 1.0;
			}
			else
			{
				weight = state.cowAttractedWeight * 1.0;
			}
		}
		else if (cell.obstacle)
		{
			weight = state.obstacleWeight * 1.0;
		}
		else if(cell.fence && !cell.open){
			weight = state.obstacleWeight * 1.0;
		}
		else if(cell.switcher){
			weight = state.obstacleWeight * 1.0;
		}
		
		else if (cell.noObject())
		{
			weight = state.emptyWeight * 1.0;
		}
		return weight;
	}
	
}
