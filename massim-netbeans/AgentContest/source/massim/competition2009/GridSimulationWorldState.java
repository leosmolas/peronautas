package massim.competition2009;
import java.util.Random;
import java.util.Vector;

import massim.cowsimulations.GridSimulationCell;
import massim.gridsimulations.SimulationWorldState;
import massim.server.Server;

/**
 * This class describes the SimulationWorldState.
 * 
 */
public class GridSimulationWorldState extends SimulationWorldState {

	private static final long serialVersionUID = 7294929408559440428L;
	public GridSimulationCell[][] board;
	public int numberOfCows;

	public Integer[] stable1X = { 0, 0 };
	public Integer[] stable1Y = { 0, 0 };
	public Integer[] stable2X = { 0, 0 };
	public Integer[] stable2Y = { 0, 0 };

	// Speed information
	public int cowSpeed;
	public int agentSpeed;

	// perception information
	public int lineOfSight;
	public int fogprobability = 10;

	// Cow behavior information
	public int cowPrivateField;
	public int cowSight;
	public int agentWeight;
	public int cowAttractedWeight;
	public int cowScareWeight;
	public int obstacleWeight;
	public int emptyWeight;
	public double weight;
	public double epsilon;

	// fence informations
	public int numberOfFences;
	public int[] switchX;
	public int[] switchY;
	public int[] fenceLength;
	public String[] fenceDirection;
	// cow_manager hat all informations of cows in Worldstate
	public Vector<GridSimulationCowAgent> cows_manager;
	public String outputFolder;
	/**
	 * This constructor is for hand crafted simulations.
	 * 
	 * @param config
	 */

	public GridSimulationWorldState(
			GridSimulationConfigurationHandCrafted config) {

		this.sizex = (config.sizex <= 200) ? config.sizex : 200;
		this.sizey = (config.sizey <= 200) ? config.sizey : 200;

		this.numberOfCows = config.numberOfCows;
		this.numberOfObstacles = config.numberOfObstacles;
		this.numberOfAgents = config.numberOfAgents;
		this.simulationName = config.simulationName;
		this.tournamentName=config.tournamentName;
		
		this.board = new GridSimulationCell[config.sizex][config.sizey];
		this.maxNumberOfSteps = config.maxNumberOfSteps;
		String[] teams = { config.teamName0, config.teamName1 };
		this.teamName = teams;

		this.cowSpeed = config.cowSpeed;
		this.agentSpeed = config.agentSpeed;

		this.lineOfSight = config.lineOfSight;

		this.cowPrivateField = config.cowPrivateField;
		this.cowSight = config.cowSight;

		this.cowAttractedWeight = config.cowAttractedWeight;
		this.cowScareWeight = config.cowScareWeight;
		this.agentWeight = config.agentWeight;
		this.obstacleWeight = config.obstacleWeight;
		this.emptyWeight = config.emptyWeight;
		this.epsilon = config.epsilon;
		this.weight = config.weight;
		
		this.numberOfFences = config.numberOfFences;
		switchX = new int[this.numberOfFences];
		switchY = new int[this.numberOfFences];
		fenceLength = new int[this.numberOfFences];
		fenceDirection = new String[this.numberOfFences];
		
		// Initialize the board
		board = new GridSimulationCell[sizex][sizey];
		for (int i = 0; i < sizex; i++) {
			for (int j = 0; j < sizey; j++) {
				board[i][j] = new GridSimulationCell();
			}
		}

		// Spread Obstacles
		int i = 0;
		while (i < numberOfObstacles) {
			int x = config.obstaclePositionX[i];
			int y = config.obstaclePositionY[i];
			board[x][y].obstacle = true;
			i++;
		}


		i = 0;
		cows_manager = new Vector<GridSimulationCowAgent>();

		while (i < numberOfCows) {
			int x = config.cowPositionX[i];
			int y = config.cowPositionY[i];
			board[x][y].cow = true;
			board[x][y].cowID = "" + i;
			double rdouble = Math.random();
			if (rdouble > 0.66) {
				board[x][y].cowturn = 0;
			} else if (rdouble < 0.33) {
				board[x][y].cowturn = 1;
			} else {
				board[x][y].cowturn = 2;
			}
			// add cow in cows_manager
			GridSimulationCowAgent cow = new GridSimulationCowAgent();
			cow.posx = x;
			cow.posy = y;
			cow.oldPosX = x;
			cow.oldPosY = y;
			cow.ID = "" + i;
			cow.cowTurn = board[x][y].cowturn;
			cows_manager.add(cow);

			i++;
		}
		/* spread fence */
		this.spreadFence(config);

		int k;
		for (i = config.stable1X[0]; i <= config.stable1X[1]; i++) {
			for (k = config.stable1Y[0]; k <= config.stable1Y[1]; k++) {
				board[i][k].setStable1(true);
			}
		}
		stable1X[0] = config.stable1X[0];
		stable1X[1] = config.stable1X[1];
		stable2X[0] = config.stable2X[0];
		stable2X[1] = config.stable2X[1];

		stable1Y[0] = config.stable1Y[0];
		stable1Y[1] = config.stable1Y[1];
		stable2Y[0] = config.stable2Y[0];
		stable2Y[1] = config.stable2Y[1];

		for (i = config.stable2X[0]; i <= config.stable2X[1]; i++) {
			for (k = config.stable2Y[0]; k <= config.stable2Y[1]; k++) {
				board[i][k].setStable2(true);			}
		}
		fogprobability = config.fogprobability;

	}

	/**
	 * This constructor is for normal simulations.
	 * 
	 * @param config
	 */
	public GridSimulationWorldState(GridSimulationConfiguration config) {

		this.sizex = (config.sizex <= 200) ? config.sizex : 200;
		this.sizey = (config.sizey <= 200) ? config.sizey : 200;
		this.numberOfCows = config.numberOfCows;
		this.numberOfObstacles = config.numberOfObstacles;
		this.numberOfAgents = config.numberOfAgents;
		this.board = new GridSimulationCell[config.sizex][config.sizey];
		this.maxNumberOfSteps = config.maxNumberOfSteps;
		String[] teams = { config.teamName0, config.teamName1 };
		this.teamName = teams;

		this.cowSpeed = config.cowSpeed;
		this.agentSpeed = config.agentSpeed;

		this.lineOfSight = config.lineOfSight;

		this.cowSight = config.cowSight;

		this.cowPrivateField = config.cowPrivateField;

		this.cowAttractedWeight = config.cowAttractedWeight;
		this.cowScareWeight = config.cowScareWeight;
		this.agentWeight = config.agentWeight;
		this.obstacleWeight = config.obstacleWeight;
		this.emptyWeight = config.emptyWeight;
		this.epsilon = config.epsilon;
		this.weight = config.weight;
		
		// Initialize the board
		board = new GridSimulationCell[sizex][sizey];
		for (int i = 0; i < sizex; i++) {
			for (int j = 0; j < sizey; j++) {
				board[i][j] = new GridSimulationCell();
			}
		}
		// Spread Obstacles
		Random r = new Random();
		int i = 0;
		while (i < numberOfObstacles) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				board[x][y].obstacle = true;
				i++;
			}
		}
		// Spread Cow
		i = 0;
		while (i < numberOfCows) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				board[x][y].cow = true;
				double rdouble = Math.random();
				if (rdouble > 0.66) {
					board[x][y].cowturn = 0;
					// System.out.println(0);
				} else if (rdouble < 0.33) {
					board[x][y].cowturn = 1;
					// System.out.println(1);
				} else {
					board[x][y].cowturn = 2;
					// System.out.println(2);
				}
				// add cow in cows_manager
				GridSimulationCowAgent cow = new GridSimulationCowAgent();
				cow.posx = x;
				cow.posy = y;
				cow.oldPosX = x;
				cow.oldPosY = y;
				cow.ID = "" + i;
				cow.cowTurn = board[x][y].cowturn;
				cows_manager.add(cow);
				i++;
			}
		}
		boolean[] setstables = { false, false };
		while (!(setstables[0] && setstables[1])) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				if (!setstables[0]) {
					board[x][y].setStable1(true);
					setstables[0] = true;
				} else {
					board[x][y].setStable2(true);
					setstables[1] = true;
				}
			}
		}
	}

	public GridSimulationWorldState() {

	}
	
	
/**
 * This method spreads the fences in worldstate, the fence will spread in
 * one of 4 directions : up , down , left, right with the configured length
 * the spreading will be broken when there is another object on the spreading way
 * Example:
 * S = switch
 * direction = right
 * length = 6
 * F = fence.
 * O = object on the spreading way
 * Normal spreading: SFFFFFF
 * Broken spreading: SFFFO  
 * @param config
 */
private void spreadFence(GridSimulationConfigurationHandCrafted config){
	
	
	
	for(int i = 0; i<this.numberOfFences;i++){

		switchX[i] = config.switchX[i];
		switchY[i] = config.switchY[i];
		fenceDirection[i] = config.fenceDirections[i];
		fenceLength[i] = config.fenceLength[i];
		
		if(!Server.recoverstep.equalsIgnoreCase("")){
			
		fenceLength[i] =	this.spreadFenceRecovery(switchX[i],switchY[i],fenceDirection[i],fenceLength[i],config);
			
		}
		else{
			
			String dir = fenceDirection[i];
			int x = switchX[i];
			int y = switchY[i];
			
			if (board[x][y].noObject()) {
				
				board[x][y].switcher=true;
				
				for(int j = 1; j< fenceLength[i]+1; j++){
					if(dir.equalsIgnoreCase("left") &&board[x-j][y].noObject()){
						board[x-j][y].fence=true;	
					}
					else if(dir.equalsIgnoreCase("right")&&board[x+j][y].noObject()){
						board[x+j][y].fence=true;	
					}
					else if(dir.equalsIgnoreCase("up")&&board[x][y-j].noObject()){
						board[x][y-j].fence=true;
					}
					else if(dir.equalsIgnoreCase("down")&&board[x][y+j].noObject()){
						board[x][y+j].fence=true;
					}
					else {
						fenceLength[i] = j;
						break;
					}
				}
		}

		}
		
	}
}
	private int spreadFenceRecovery(int switchx, int switchy, String dir, int length, GridSimulationConfigurationHandCrafted config) {
		
		if(board[switchx][switchy].noObject()){
			board[switchx][switchy].switcher = true;
		
		boolean agentatFence = false;
			for(int i = 1; i< length+1; i++){
				if(dir.equalsIgnoreCase("left")&&board[switchx-i][switchy].noObject()){
					board[switchx-i][switchy].fence=true;
					if(checkAgentatFence(switchx-i,switchy,config)) agentatFence = true;
				}
				else if(dir.equalsIgnoreCase("right")&& board[switchx+i][switchy].noObject()){
					board[switchx+i][switchy].fence=true;
					if(checkAgentatFence(switchx+i, switchy, config)) agentatFence = true;
				}
				else if(dir.equalsIgnoreCase("up")&& board[switchx][switchy-i].noObject()){
					board[switchx][switchy-i].fence=true;
					if(checkAgentatFence(switchx, switchy-i, config)) agentatFence = true;
				}
				else if(dir.equalsIgnoreCase("down")&& board[switchx][switchy+i].noObject()){
					board[switchx][switchy+i].fence=true;
					if(checkAgentatFence(switchx, switchy+i, config)) agentatFence = true;
				}
				else{
					length = i;
				}
			}
			for(int i = 1; i< length+1; i++){
				if(dir.equalsIgnoreCase("left")&& agentatFence){
					board[switchx-i][switchy].open=true;
				}
				if(dir.equalsIgnoreCase("right")&& agentatFence){
					board[switchx+i][switchy].open=true;
				}
				if(dir.equalsIgnoreCase("up")&& agentatFence){
					board[switchx][switchy-i].open=true;
				}
				if(dir.equalsIgnoreCase("down") && agentatFence){
					board[switchx][switchy+i].open=true;
				}
			}
		}	
		return length;
}

	private boolean checkAgentatFence(int x, int y,
			GridSimulationConfigurationHandCrafted config) {
		int[] agentx=config.agentPositionX;
		int[] agenty=config.agentPositionY;
		for(int i = 0 ; i<agentx.length;i++){
			for(int j = 0; j<agenty.length;j++){
				if(agentx[i]==x && agenty[j]==y) return true;
			}
		}
		
		return false;
	}

	/**
	 * this function copies the board of the parameter state to the board of
	 * this state. It basically just sets agents, cows and obstacle flags.
	 * 
	 * @param commitState
	 *            : state to be copied
	 */

	public void commit(GridSimulationWorldState commitState) {
		int i, k;
		for (i = 0; i < sizex; i++) {
			for (k = 0; k < sizey; k++) {

				board[i][k].cow = commitState.board[i][k].cow;

				board[i][k].agent = commitState.board[i][k].agent;

				board[i][k].cowturn = commitState.board[i][k].cowturn;

				board[i][k].cowID = commitState.board[i][k].cowID;

				board[i][k].obstacle = commitState.board[i][k].obstacle;

			}
		}

	}

}
