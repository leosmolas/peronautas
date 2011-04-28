package massim.framework;

import java.util.LinkedList;

/**
 * This class provides a good start to build agent provider that are based on another agent provider which will just filter 
 * it's offers by some criteria based upon an agent parameter.
 *
 */
public abstract class AbstractAgentFilter implements AgentProvider {
	private AgentProvider base;
	protected AbstractAgentFilter(AgentProvider b) {
		base=b;
	}
	public AgentProvider getBase() {return base;}
	
	public UniqueSimulationAgent[] getAgents(AgentParameter p) {
		UniqueSimulationAgent[] r=base.getAgents(p);

		LinkedList<UniqueSimulationAgent> l = new LinkedList<UniqueSimulationAgent>();
		for (int i=0;i<r.length;i++) {
			if (isSuitable(r[i],p)) 
				l.add(r[i]);
		}
		r=new UniqueSimulationAgent[l.size()];
		return l.toArray(r);
	}
	
	/**
	 * This method determines if an agent suits the requirements found in agent parameters p.
	 * @param a agent candidate
	 * @param p agent requirements.
	 * @return true iff a suits p.
	 */
	protected abstract boolean isSuitable(UniqueSimulationAgent a, AgentParameter p);
	
	/**
	 * Allocates an agent. An allocated agent won't be offered by this provider until it's remove method is called.
	 */
	public void allocateAgent(UniqueSimulationAgent agent) {
		base.allocateAgent(agent);
	}

	public void freeAgent(UniqueSimulationAgent agent) {
		base.freeAgent(agent);
	}
	

}
