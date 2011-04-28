package massim.competition2008;

/**
 * This class is derived from GridSimulationConfiguration.
 * The agent positions, gold positions, obstacle positions and the depot position are added. 
 *
 */
public class GridSimulationConfigurationHandCrafted extends
		GridSimulationConfiguration {

	public int[] agentPositionX;
	public int[] agentPositionY;

	public int[] cowPositionX;
	public int[] cowPositionY;
	
	
	
	public int[] obstaclePositionX;
	public int[] obstaclePositionY;
	
	public int[] stable1X;
	public int[] stable1Y;
	public int[] stable2X;
	public int[] stable2Y;
//Infor about fences
	public int[] switchX;
	public int[] switchY;
	public String[] fenceDirections;
	public int[] fenceLength;
	

}