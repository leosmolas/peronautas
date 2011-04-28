package massim.cowsimulations;

import massim.gridsimulations.AbstractGridSimulationAgentInitialPerception;

/**
 * This class is the initial perception, which is sent to the agent.
 *
 */
public class GridSimulationAgentInitialPerception extends AbstractGridSimulationAgentInitialPerception {
	
	private static final long serialVersionUID = -2107830240215498719L;
	//extra attribute for this simulation
	public int corralx0;
	public int corraly0;
	public int corralx1;
	public int corraly1;
	public int lineOfSight;
}