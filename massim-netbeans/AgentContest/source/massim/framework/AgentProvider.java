package massim.framework;

/**
 * This interface offers methods to retrieve a set of agents from the implementing object and to allocate agents from it.
 * Those agents won't be available until they are removed via SimulationAgent.remove().
 */
public interface AgentProvider {
	
	/**
	 * Retrieve a list of available agents, fulfilling requirements as mentioned in agentparameter;
	 * @param p requirements the returned agents should fulfill.
	 * @return an array of suitable agents
	 */
	UniqueSimulationAgent[] getAgents(AgentParameter agentparameter);
	
	/**
	 * Allocate an agent. This means that it won't be offered anymore via getAgents until the agent is removed via it's remove method.
	 * @param agent to allocate.
	 */
	void allocateAgent(UniqueSimulationAgent agent);
	
	/**
	 * Free an agent
	 */
	void freeAgent(UniqueSimulationAgent agent);
}
