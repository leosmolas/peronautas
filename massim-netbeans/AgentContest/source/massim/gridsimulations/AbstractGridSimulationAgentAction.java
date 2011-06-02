package massim.gridsimulations;

import massim.framework.Action;

/**
 * This class contains the action the agent sends to the server.
 *
 */
public abstract class AbstractGridSimulationAgentAction implements Action {

	private static final long serialVersionUID = -7284135303106342780L;
	public String type = "skip";
	public String param = "";
}

