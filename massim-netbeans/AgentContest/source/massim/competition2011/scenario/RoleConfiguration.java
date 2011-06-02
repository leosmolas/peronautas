package massim.competition2011.scenario;

import java.io.Serializable;
import java.util.Vector;

/**
 * This class represent a set of configuration options for one of the defined roles.
 */
public class RoleConfiguration implements Serializable {

	private static final long serialVersionUID = -3861520735115295259L;
	
	public String name;
	public int maxEnergy;
	public int maxBuyEnergy;
	public int rateBuyEnergy;
	public int maxEnergyDisabled;
	public int rateBuyEnergyDisabled;
	public int maxHealth;
	public int maxBuyHealth;
	public int rateBuyHealth;
	public int strength;
	public int maxBuyStrength;
	public int rateBuyStrength;
	public int visRange;
	public int maxBuyVisRange;
	public int rateBuyVisRange;
	public Vector<String> actions = new Vector<String>();
	public Vector<String> actionsDisable = new Vector<String>();

}
