package massim.competition2011;

import massim.framework.FinalPerception;

/**
 * This class holds the information that is sent to the agent as final perception when simulation ends.
 */
public class GraphSimulationAgentFinalPerception implements FinalPerception {

	private static final long serialVersionUID = -4593915560115551878L;
	
	/**
	 * The final score of the team.
	 */
	public long score;
	
	/**
	 * The ranking of the team, according to team scores, 1 being first (winner).
	 */
	public int ranking;
	

}
