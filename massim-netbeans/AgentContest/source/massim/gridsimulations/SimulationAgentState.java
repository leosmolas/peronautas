package massim.gridsimulations;

import massim.framework.simulation.AgentState;

/**
 * This class represents the AgentState.
 *
 */
public abstract class SimulationAgentState implements AgentState {
	
	private static final long serialVersionUID = -5803961944118937846L;
	public Integer posx = 0, oldPosx = 0;
	public Integer posy = 0, oldPosy = 0;
	public Integer score = 0;
	public boolean actionFailed = false;
	public String team;
	public String name;
	public String lastAction = "skip";
	public String currentAction = "none";
	public String param ="";
	
}