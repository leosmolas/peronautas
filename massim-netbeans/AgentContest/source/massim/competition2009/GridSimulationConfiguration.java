package massim.competition2009;

//import massim.server.ServerSimulationConfiguration;
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
	public double weight;
	public double epsilon;
	public int cowSight;
	public int cowPrivateField;	
	public int fogprobability;
	public String id;
	//public String outputFolder;
	//Informations about fence
	public int numberOfFences;


}
