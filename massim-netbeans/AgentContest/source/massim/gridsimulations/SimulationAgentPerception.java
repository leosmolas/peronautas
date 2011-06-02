package massim.gridsimulations;

import massim.framework.Perception;

/**
 * This class produce the normal perception, which is send to the agent.
 *
 */
public abstract class SimulationAgentPerception implements Perception,
		massim.framework.util.XMLCodec.XMLEncodable {

	private static final long serialVersionUID = -5791498406627754380L;
	public int step;
	public int posx;
	public int posy;
	
}