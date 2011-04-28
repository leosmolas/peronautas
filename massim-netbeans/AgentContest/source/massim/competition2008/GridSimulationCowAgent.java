package massim.competition2008;
import java.util.Collections;
import java.util.Vector;

import massim.cowsimulations.GridSimulationCell;



/**
 * @category Massim08 Simulation Support Class
 * This class is used to simulate cows moving at the same time. It contains the cow Movement Algorithm and some 
 * information about the specific cow.
 *
 */
public class GridSimulationCowAgent
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
	
	public void computeCowMove(GridSimulationWorldState state)
	{
		direction = "skip";
		
		//int x,y;	
		GridSimulationCell emptyCell = new GridSimulationCell();
		double vec[] = {0,0};
		vec[0] = 0.0;
		vec[1] = 0.0;
		
		
		for (int i = -state.cowSight; i <= state.cowSight; ++i)
		{
			for (int j = -state.cowSight; j <= state.cowSight; ++j)
			{
				if((posx+i) <= state.sizex-1 && 0 <= (i+posx) && (posy+j) <= state.sizey-1 && 0 <= (posy+j))
				{
					//if cell is inside, calculate the weight
					vec = calcVec(i,j, state.board[posx+i][posy+j],vec, state);
					
				}
				else
				{
					//Cells outside of the grid will not be considered.
					//cells outside the grid are seen as empty cells
					//vec = calcVec(i, j, emptyCell, vec, state);
				}
			}
		}
	
		direction = makeStep(posx, posy, vec,state);
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
	
	public String makeStep(int x, int y, double[] vec,GridSimulationWorldState state)
	{
		String movedir = "";
		int newX, newY;
		newX = x;
		newY = y;
		
		if(Math.sqrt( old_vector[0] * old_vector[0] + old_vector[1] * old_vector[1] ) < state.epsilon){
			old_vector[0]=0.0;
			old_vector[1]=0.0;
		}
		//vec[0] = vec[0] + state.weight*old_vector[0];
		//vec[1] = vec[1] + state.weight*old_vector[1];
		
		old_vector[0] = vec[0];
		old_vector[1] = vec[1];
		
		double gk, hyp, sin;
		//gegenkante 
		//gk = Math.min(Math.abs(vec[0]), Math.abs(vec[1]));
		gk = Math.abs(vec[1]);
		//hypotenuse
		hyp = Math.sqrt( vec[0] * vec[0] + vec[1] * vec[1] );
		
		
		// this hyp < 0.01 is a check, that only vectors result a step
		// if we wont do this, empty cowperceptions would cause a random step
		if (hyp < 0.01)
		{
			
			return "skip";
		}
		
		sin = gk / hyp;
		//0.923879 = sin(67,5) grad and 0.382683 = sind(22.5) grad
		//cow moves diagonal

		if(0.923879>sin && sin >0.382683){
			if(vec[1] < 0){
				movedir +="north";
				newY = newY - 1;
			}
			else{
				
				movedir +="south";
				newY = newY + 1;
			}
			if(vec[0]>0){
				movedir +="east";
				newX =newX +1;
			}
			else {
				movedir +="west";
				newX = newX -1;
			}
			
		}
		//cow moves parallel with x
		else if(sin <= 0.382683){
			if(vec[0]> 0){
				movedir +="east"; 
				newX = newX +1;
			}
			else {
				movedir +="west";
				newX = newX -1;
			}
		}
		//cow moves parallel with y
		else if(sin >= 0.923879){
			if(vec[1] < 0){
				movedir +="north";
				newY = newY-1;
				
			}
			else{
				movedir +="south";
				newY = newY+1;
			}
		}
		
		if(!state.board[newX][newY].freeCell()){
			movedir = findNewDir(x,y,state);
		}
		return movedir;
		
//		if (sin < 0.4) // sin > 0.4 => angle is in the fields to result a diagonal step
//		{
//			if (Math.abs(vec[0]) > Math.abs(vec[1]))
//			{
//				if(vec[0] < 0)
//				{
//					movedir += "west";
//					newX = newX - 1;
//				}
//				if(vec[0] > 0)
//				{
//					movedir += "east";
//					newX = newX + 1;
//				}
//			}
//			else
//			{
//				if(vec[1] < 0)
//				{
//					movedir += "north";
//					newY = newY - 1;
//				}
//				if(vec[1] > 0)
//				{
//					movedir += "south";
//					newY = newY + 1;
//				}
//				
//			}
//		}
//		else
//		{
//			if(vec[1] < 0)
//			{
//				movedir += "north";
//				newY = newY - 1;
//			}
//			if(vec[1] > 0)
//			{
//				movedir += "south";
//				newY = newY + 1;
//			}
//			if(vec[0] < 0)
//			{
//				movedir += "west";
//				newX = newX - 1;
//			}
//			if(vec[0] > 0)
//			{
//				movedir += "east";
//				newX = newX + 1;
//			}
//
//		}	
		
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
