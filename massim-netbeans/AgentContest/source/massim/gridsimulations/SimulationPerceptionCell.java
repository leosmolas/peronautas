package massim.gridsimulations;

import java.io.Serializable;

/**
 * This class is a single Perception cell.
 *
 */
public class SimulationPerceptionCell implements Serializable {
	
	private static final long serialVersionUID = 2341480499926890895L;
	public boolean obstacle;
	public boolean agent;
	public boolean unknown;
	public String agentType;
}

