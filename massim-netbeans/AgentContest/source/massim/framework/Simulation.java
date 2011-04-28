package massim.framework;

/**
 * This interface defines some methods that must be provided by any simulation
 * to be used in the MASSim framework.
 *
 */

public interface Simulation extends Component {
	
	/**
	 * This method will be automatically called when the simulation starts. 
	 * Simulation implementors are encouraged to override this method. They may
	 * or may not create and remove agents and even ask them to act. It is 
	 * however recommended to do initialization here.
	 */
	void startSimulation();
	
	/**
	 * This method will be automatically called when the simulation is supposed
	 * to do a simulation step. 
	 * Simulation implementors are encouraged to override this method. They may
	 * or may not create and remove agents and ask them to act.
	 */
	void stepSimulation();

	/**
	 * This method will be automatically called once when the simulation has been started and it
	 * is to end. It's not mandatory that startSimulation or stepSimulation returned false.
	 */
	String endSimulation();
	
	/**
	 * This method returns true iff the simulation is in a final state.
	 * @return true iff simulation is in final state
	 */
	boolean isFinished();
	
	/**
	 * This method will be automatically called as soon as the simulation is
	 * being configured. This method may only be called when the simulation is
	 * not running and must be called before the simulation will be started.
	 * 
	 * Simulation implementors are encouraged to override this method.
	 * @param config configuration message
	 */	
	void configureSimulation(SimulationConfiguration config);
	
	/**
	 * This method will retrieve the whole simulation state. In general this
	 * state object should contain any information about the simulation game
	 * state though this is not a requirement. So it's up to you what
	 * information is put here.
	 * @return the simulation state
	 */
	SimulationState getSimulationState();
	
	/**
	 * Retrieve the AgentManager that will be used for agent creation by this simulation.
	 * Simulation implementors should not use getAgentManager and access it directly, but
	 * use the Agent object and createAgent instead.
	 * @return Returns the agentmanager.
	 */
	AgentManager getAgentManager();

	/**
	 * @param agentmanager The agentmanager to set.
	 */
	void setAgentManager(AgentManager agentmanager);
}
