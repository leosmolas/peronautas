package massim.competition2009;
import java.util.Collections;
import java.util.Vector;
import massim.cowsimulations.GridSimulationCell;

/**
 * this class implements the algorithm of Tristan Behrens at tu-clausthal 
 *
 */
public class GridSimulationCowAgent2
{
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

	
	/**
	 * compute cow's direction. 
	 * @param state
	 */
	public void computeCowMove(GridSimulationWorldState state){
		w_state = state;
		
		Vector<Integer> weight_vec = new Vector<Integer>();
		Vector<int[]> pos_vec = new Vector<int[]>();
		int max_weight =Integer.MIN_VALUE ;
		
		for(int i = -1 ; i<=1;i++){
			for(int j = -1 ; j<=1;j++){
				int x = posx+i;
				int y = posy+j;
				//we take care only about the empty cell around cow and cow's cell self
				if(x>=0 && y>=0 && x<state.sizex && y<state.sizey && 
						(state.board[x][y].freeCell()||(x == posx && y == posy)) ){
					int pos[] = {x,y};
					
					int weight = calcWeight(state.board[x][y],pos);
					
					weight_vec.add(weight);
					
					pos_vec.add(pos);
					
					if(max_weight<weight) max_weight=weight;
				}
				
			}
		}
		
		int i = 0;
		 while (i<weight_vec.size()) {
		   if(weight_vec.get(i) < max_weight){
		     pos_vec.remove(i);
		   weight_vec.remove(i);
		   } else {
		     i++;
		   }
		 }
		 
		
		Collections.shuffle(pos_vec);
		int move_pos[] = pos_vec.get(0);
		
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
 * calculate the weight of the cell that cow intends to move in 
 * @param cell :cell cow want to move in 
 * @param pos_a: position of cell
 * @return weight :  of cell
 */
	private int calcWeight(GridSimulationCell cell, int[] pos_a) {
		//empty cell has empty weight als base weight 
		int base_weight = w_state.emptyWeight;
		Vector<Integer> distance = new Vector<Integer>();
		for (int i = -w_state.cowSight; i <= w_state.cowSight; ++i){
			
			for (int j = -w_state.cowSight; j <= w_state.cowSight; ++j){
		
				int x = posx +i; int y = posy +j;
				if(x>=0 && y>=0 && x<w_state.sizex && y<w_state.sizey){
					
					int weight = cell_weight(w_state.board[x][y],i,j);
				
					int pos_b[] = {x,y};
					int dis = step_distance(pos_a, pos_b);
					distance.add(dis);
					if(dis != 0){
						
					weight = weight/dis;
					base_weight += weight;
					}
				}
			}
		}
		return base_weight;
	}
	/**
	 * return the step distance of 2 position. Also the number of step that
	 * we need to go from position a to position b
	 * @param pos_a
	 * @param pos_b
	 * @return
	 */
	private int step_distance(int[] pos_a, int[] pos_b) {
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
		return step;
	}
	
	private int cell_weight(GridSimulationCell cell, int i, int j) {
		int w = 0;
		if(cell.agent) 
			w= w_state.agentWeight;
		
		else if(cell.cow){
			int max = Math.max(Math.abs(i), Math.abs(j));
			
			if(max < w_state.cowPrivateField && max != 0) 
				w= w_state.cowScareWeight;
			
			else if(max != 0)
				
				w = w_state.cowAttractedWeight;
		}
		else if((cell.fence && !cell.open)|| cell.obstacle || cell.switcher)
			w = w_state.obstacleWeight;
		
		return w;
	}

	private String findNewDir(int x, int y, GridSimulationWorldState state) {
		Vector<String> randomdir = new Vector<String>();
		if(state.board[x+1][y].freeCell()){
			randomdir.add("east");
		}
		if(state.board[x-1][y].freeCell()){
			randomdir.add("weast");
		}
		if(state.board[x][y-1].freeCell()){
			randomdir.add("north");
		}
		if(state.board[x][y+1].freeCell()){
			randomdir.add("south");
		}
		if(state.board[x+1][y+1].freeCell()){
			randomdir.add("southeast");
		}
		if(state.board[x-1][y+1].freeCell()){
			randomdir.add("southweast");
		}
		if(state.board[x-1][y-1].freeCell()){
			randomdir.add("northweast");
		}
		if(state.board[x+1][y-1].freeCell()){
			randomdir.add("northeast");
		}
		randomdir.add("skip");

			Collections.shuffle(randomdir);
			String dir = randomdir.get(0);	
		
		return dir;
	}
	
}
