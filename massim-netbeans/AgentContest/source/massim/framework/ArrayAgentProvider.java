package massim.framework;

import java.util.Vector;

/**
 * This class is able to serve as an AgentProvider. It will provide agents that were given
 * to it by an array.
 *
 */

public class ArrayAgentProvider implements AgentProvider, Component {
	private Vector<UniqueSimulationAgent> agents; // vector of available agents
	
	public ArrayAgentProvider() {
		this.agents = new Vector<UniqueSimulationAgent>();
	}
	
	public ArrayAgentProvider(UniqueSimulationAgent agents[]) {
		//add wrappers to initial agents and add them to a vector
		this.agents = new Vector<UniqueSimulationAgent>();
		for (int i=0;i<agents.length;i++) this.agents.add(agents[i]);
	}
	
	public UniqueSimulationAgent[] getAgents(AgentParameter agentparameter) {
		//convert vector to array and return it
		return agents.toArray(new UniqueSimulationAgent[0]);
	}

	public void setAgents(UniqueSimulationAgent agents[]) {
		this.agents = new Vector<UniqueSimulationAgent>();
		for (int i=0;i<agents.length;i++) this.agents.add(agents[i]);
	}
	
	public void allocateAgent(UniqueSimulationAgent agent) {
		agents.remove(agent);
	}

	public void freeAgent(UniqueSimulationAgent agent) {
		agents.add(agent);
	}

	public void start() {
	}

	public void stop() {
	}
}
