package massim.gridsimulations;

import massim.framework.simulation.WorldState;

/**
 * This class describes the SimulationWorldState.
 *
 */
public abstract class SimulationWorldState implements WorldState {

	private static final long serialVersionUID = 7294929408559440428L;
	public Integer sizex;
	public Integer sizey;
	public Integer informationDistortionProbability;
	public Integer actionSuccessProbability;
	public Integer numberOfAgents;
	public Integer numberOfSteps;
	public Integer maxNumberOfSteps;
	public Integer currentStep = 0;
	public Integer[] teamScore = { 0, 0 };
	public Integer numberOfObstacles;
	public String[] teamName ={"",""};
	public String simulationName;
	public String tournamentName;
	public String outputFolder = "";

}
