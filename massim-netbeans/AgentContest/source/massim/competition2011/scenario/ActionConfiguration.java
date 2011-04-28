package massim.competition2011.scenario;

import java.io.Serializable;

/**
 * This class holds the configuration of an action.
 */
public class ActionConfiguration implements Serializable {

	private static final long serialVersionUID = -7387300848628264823L;
	
	public String name;
	public int energyCost;
	public int energyCostFailed;
	public int energyCostDisabled; 
	public int energyCostFailedDisabled;
	public int healthCost;
	public int healthCostFailed;
	public int healthCostDisabled;
	public int healthCostFailedDisabled;
	public int pointsCost;
	public int pointsCostFailed;
	public int pointsCostDisabled;
	public int pointsCostFailedDisabled;
	
}
