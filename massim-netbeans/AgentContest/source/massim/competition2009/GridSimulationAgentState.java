package massim.competition2009;

import java.util.Random;

import massim.gridsimulations.SimulationAgentState;
import massim.server.Server;

/**
 * This class represents the AgentState.
 *
 */
public class GridSimulationAgentState extends SimulationAgentState {
	
	private static final long serialVersionUID = -5803961944118937846L;
	
	public Integer actionFailureProbability = 0;
	//public Boolean agentInDepot = false;
	public Boolean actionFailed = false;
	public Boolean wasPushed = false;
	public Boolean actionDone = false;
	public String direction = "";//up, down, left, right, upleft, downleft, upright, downright
	/**
	 * This method spreads the agents over the grid.
	 * @param state The WorldState (grid)
	 */
	public void spreadAgent(GridSimulationWorldState state) {
		
		// Spread Agent
		boolean placed = false;
		Random r = new Random();
		
		if(!Server.recoverstep.equalsIgnoreCase("")){
			int[] posx_y = Server.agent_pos.get(this.name);
			posx = posx_y[0];
			posy = posx_y[1];
			
		}
		else{
		while (!placed) {
			int x = Math.abs(r.nextInt()) % (state.sizex - 1);
			int y = Math.abs(r.nextInt()) % (state.sizey - 1);
			
			if (state.board[x][y].noObject() && !state.board[x][y].agent) {
				posx = x;
				posy = y;
				placed = true;
			}
		}
		}
		
		state.board[posx][posy].agent = true;
		state.board[posx][posy].agentTeam = team;
	}
	
	/**
	 * This Method set the agents positions.
	 * @param state The WorldState
	 * @param config The config
	 * @param i Agentnumber
	 */
	public void spreadAgent(GridSimulationWorldState state, GridSimulationConfigurationHandCrafted config, int i) {
		
		// Spread Agent
		if(!Server.recoverstep.equalsIgnoreCase("")){
			int[] posx_y = Server.agent_pos.get(this.name);
			posx = posx_y[0];
			posy = posx_y[1];
			
		}
		else{
		posx = config.agentPositionX[i];
		posy = config.agentPositionY[i];
		}
		oldPosx = posx;
		oldPosy = posy;
		state.board[posx][posy].agent = true;
		state.board[posx][posy].agentTeam = team;
	}
}
