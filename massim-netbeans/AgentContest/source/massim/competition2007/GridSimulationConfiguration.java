package massim.competition2007;

import massim.gridsimulations.SimulationConfigurationImpl;

/**
 * This class describes the not hand crafted configuration.
 *
 */
public class GridSimulationConfiguration extends SimulationConfigurationImpl {
	public int numberOfObstacles;
	public int numberOfGoldItems;
	public int maxNumberOfGoldItems;
	public int informationDistortionProbability;
	public int actionFailureProbability;
	public int maxActionFailureProbability;
	public int goldGenerationProbability;
	public int goldGenerationFrequency; //for example all ten steps
	public int goldGenerationNumber;
	
}