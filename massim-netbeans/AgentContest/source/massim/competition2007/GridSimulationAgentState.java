package massim.competition2007;

import java.util.Random;

import massim.gridsimulations.SimulationAgentState;
/**
 * This class represents the AgentState.
 *
 */
public class GridSimulationAgentState extends SimulationAgentState {
	private static final long serialVersionUID = -5803961944118937846L;
	
	public Integer currentItems = 0;
	//public Integer actionFailureProbability = 0;
	public Integer timeInDepot = 0;
	public Boolean wasPushed = false;
	public Boolean actionDone = false;
	
	public Boolean agentHoldsGold = false;
	public Boolean agentInDepot = false;

	/**
	 * This method spreads the agents over the grid.
	 * @param state The WorldState (grid)
	 */
	public void spreadAgent(GridSimulationWorldState state) {
		
		// Spread Agent
		boolean placed = false;
		Random r = new Random();
		while (!placed) {
			int x = Math.abs(r.nextInt()) % (state.sizex - 1);
			int y = Math.abs(r.nextInt()) % (state.sizey - 1);
			
			if (state.board[x][y].noObject() && !state.board[x][y].agent) {
				posx = x;
				posy = y;
				state.board[x][y].agent = true;
				state.board[x][y].agentTeam = team;
				placed = true;
			}
		}
	}

	/**
	 * This Method set the agents positions.
	 * @param state The WorldState
	 * @param config The config
	 * @param i Agentnumber
	 */
	public void spreadAgent(GridSimulationWorldState state, GridSimulationConfigurationHandCrafted config, int i) {
		// Spread Agent
		posx = config.data.agentPositionX[i];
		posy = config.data.agentPositionY[i];
		state.board[posx][posy].agent = true;
		state.board[posx][posy].agentTeam = team;
	}
}