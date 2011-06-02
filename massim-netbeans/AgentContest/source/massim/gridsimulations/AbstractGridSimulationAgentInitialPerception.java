package massim.gridsimulations;

import massim.framework.InitialStickyPerception;

/**
 * This class is the initial perception, which is send to the agent.
 *
 */
public abstract class AbstractGridSimulationAgentInitialPerception implements InitialStickyPerception {
	
	private static final long serialVersionUID = -2107830240215498719L;
	
	public String opponent;
	public int steps;
	public int gsizex;
	public int gsizey;
}