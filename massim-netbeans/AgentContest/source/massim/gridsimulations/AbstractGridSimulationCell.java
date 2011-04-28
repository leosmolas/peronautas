package massim.gridsimulations;


import java.io.Serializable;

/**
 * This class describes a single GridCell.
 *
 */
public abstract class AbstractGridSimulationCell implements Serializable {
	private static final long serialVersionUID = -4580023204848715935L;
	
	public boolean obstacle;
	public boolean agent;
	public String object = "";
	public String agentTeam;
	
	abstract public boolean noObject();
	abstract public boolean freeCell();
	
}