package massim.competition2010;


import massim.gridsimulations.SimulationConfigurationImpl;

/**
 * This class describes the not hand crafted configuration.
 *
 */
public class GridSimulationConfiguration extends SimulationConfigurationImpl {
	
	public int numberOfCows;
	//Speed information
	public int cowSpeed;
	public int agentSpeed;
	//Cow behavior information

	
	
	public int agentWeight;
	public int cowAttractedWeight;
	public int cowScareWeight;
	public int obstacleWeight;
	public int emptyWeight;
	public double epsilon;
	public int cowSight;
	public int cowPrivateField;	
	public int fogprobability;
	public int actionFailure;
	public String id;
	//public String outputFolder;
	//Informations about fence
	public int numberOfFences;


}
