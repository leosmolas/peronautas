package massim.competition2007;

import java.util.Random;

import massim.goldsimulations.GridSimulationCell;
import massim.gridsimulations.SimulationWorldState;

/**
 * This class describes the SimulationWorldState.
 *
 */
public class GridSimulationWorldState extends SimulationWorldState{

	private static final long serialVersionUID = 7294929408559440428L;
	public GridSimulationCell[][] board;
	public Integer numberOfGoldItems;
	public Integer depotx;
	public Integer depoty;

	public Integer maxNumberOfCarriedGoldItems;
	public Integer informationDistortionProbability;
	public Integer actionFailureProbability;
	public Integer maxActionFailureProbability;
	

	/**
	 * This constructor is for normal simulations.
	 * @param config
	 */
	public GridSimulationWorldState(GridSimulationConfiguration config) {
		
		configureWorldState(config);
		// Initialize the board
		initializeBoard();
		// Spread Obstacles
		Random r = new Random();
		spreadObstacles(r);
		// Spread Gold
		spreadGold(r);
		// Spread Depot
		spreadDepot(r);
	}

	/**
	 * This constructor is for hand crafted simulations.
	 * @param config
	 */
	public GridSimulationWorldState(GridSimulationConfigurationHandCrafted config) {
		
		configureWorldState(config);
		// Initialize the board
		initializeBoard();
		// Spread Obstacles
		spreadObstacles(config);
		// Spread Gold
		spreadGold(config);
		// Spread Depot
		spreadDepot(config);
		
	}

	private void configureWorldState(GridSimulationConfiguration config) {
		this.sizex = (config.sizex <= 100) ? config.sizex : 100;
		this.sizey = (config.sizex <= 100) ? config.sizex : 100;
		this.numberOfGoldItems = config.numberOfGoldItems;
		this.maxNumberOfCarriedGoldItems = config.maxNumberOfGoldItems;
		this.numberOfObstacles = config.numberOfObstacles;
		this.numberOfAgents = config.numberOfAgents;
		this.board = new GridSimulationCell[config.sizex][config.sizey];
		this.informationDistortionProbability = config.informationDistortionProbability;
		this.actionFailureProbability = config.actionFailureProbability;
		this.maxActionFailureProbability = config.maxActionFailureProbability;
		this.numberOfSteps = config.maxNumberOfSteps;
		String[] teams = {config.teamName0, config.teamName1};
		this.teamName = teams;
	}

	private void initializeBoard() {
		board = new GridSimulationCell[sizex][sizey];
		for (int i = 0; i < sizex; i++) {
			for (int j = 0; j < sizey; j++) {
				board[i][j] = new GridSimulationCell();
			}
		}
	}

	private void spreadDepot(GridSimulationConfigurationHandCrafted config) {
		this.depotx = config.data.depotx;
		this.depoty = config.data.depoty;
		board[depotx][depoty].depot = true;
	}

	private void spreadDepot(Random r) {
		boolean placed = false;
		while (!placed) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				board[x][y].depot = true;
				depotx = x;
				depoty = y;
				placed = true;
			}
		}
	}

	private void spreadGold(GridSimulationConfigurationHandCrafted config) {
		int i;
		i = 0;
		while (i < numberOfGoldItems) {
			int x = config.data.GoldPositionX[i];
			int y = config.data.GoldPositionY[i];
			board[x][y].gold = true;
			i++;
		}
	}

	private void spreadGold(Random r) {
		int i = 0;
		while (i < numberOfGoldItems) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				board[x][y].gold = true;
				i++;
			}
		}
	}

	private void spreadObstacles(GridSimulationConfigurationHandCrafted config) {
		int i = 0;
		while (i < numberOfObstacles) {
			int x = config.data.obstaclePositionX[i];
			int y = config.data.obstaclePositionY[i];
			board[x][y].obstacle = true;
			i++;
		}
	}

	private void spreadObstacles(Random r) {
		int i = 0;
		while (i < numberOfObstacles) {
			int x = Math.abs(r.nextInt()) % (sizex - 1);
			int y = Math.abs(r.nextInt()) % (sizey - 1);
			if (board[x][y].noObject()) {
				board[x][y].obstacle = true;
				i++;
			}
		}
	}

}
