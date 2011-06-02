package massim.framework;

import java.util.Vector;
import java.util.concurrent.Future;

/**
 * This class is a good starting point to create a new simulation. It provides
 * some methods to create and remove agents aswell as methods to ask agents to
 * act according to some perception.
 *
 */

public abstract class DefaultSimulation implements Simulation {
	private AgentManager	agentmanager;
	private Controller		controller;

	private Vector<SimulationAgent>	agentlist;

	public DefaultSimulation() {
		agentlist = new Vector<SimulationAgent>();
	}
	
	/** Create an agent, represented by an Agent object.
	 * This method should be used by simulation implementors.
	 * @param parameter agent creation parameters
	 * @return according agent object
	 */
	protected SimulationAgent createAgent(AgentParameter parameter) {
		SimulationAgent a = agentmanager.createAgent(parameter);
		agentlist.add(a);
		return a;
	}
	
	/**
	 * This method does the same as createAgent, but it won't block and return a Future<Agent> instead.
	 * @param parameter agent creation parameters
	 * @return according agent future
	 */
	@SuppressWarnings("unchecked")
	protected Future<SimulationAgent> concurrentCreateAgent(AgentParameter parameter) {
		return agentmanager.concurrentCreateAgent(parameter);
	}
	
	/**
	 * Remove an agent. This method will remove an agent from the simulation and
	 * make thus make it possible that system resources can be freed. It's
	 * imperative to remove all created agents before the simulation ends. 
	 * @param a agent to remove
	 */
	//TODO agent autokill should be added.
	protected void removeAgent(SimulationAgent a) {
		a.remove();
	}
	
	/**
	 * Remove all agents that were created by this simulation. It's
	 * imperative to remove all created agents before the simulation ends.
	 */
	protected void removeAllAgents() {
		synchronized(agentlist) {
			for (int i=0;i<agentlist.size();i++) {
				agentlist.get(i).remove();
			}
		}
	}
	
	/**
	 * This method will deliver a perception to an agent, getting the agents reaction in return.
	 * @param perception perception to deliver to the agent
	 * @param a agent to access
	 * @return action as returned by the agent
	 */
	protected Action getAction(Perception perception, SimulationAgent a) {
		return a.getAction(perception);
	}

	/**
	 * This is the non-blocking version of getAction.
	 * @param perception perception to deliver to the agent
	 * @param a agent to access
	 * @return future of the action returned by the agent
	 */
	protected Future<Action> concurrentGetAction(Perception perception, SimulationAgent a) {
		return a.concurrentGetAction(perception);
	}

	//dummy implementation
	public void start() {
	}
	//dummy implementation
	public void stop() {
	}

	/**
	 * This method can be used by simulation managers to configure a simulation.
	 */
	// Since simulations are not forced to have configurable features we can
	// save them some work and provide an implementation that just does nothing
	public void configureSimulation(SimulationConfiguration m) {}
	
	/**
	 * Retrieve the AgentManager that will be used for agent creation by this simulation.
	 * Simulation implementors should not use getAgentManager and access it directly, but
	 * use the Agent object and createAgent instead.
	 * @return Returns the agentmanager.
	 */
	public AgentManager getAgentManager() {
		return agentmanager;
	}

	/**
	 * @param agentmanager The agentmanager to set.
	 */
	public void setAgentManager(AgentManager agentmanager) {
		this.agentmanager = agentmanager;
	}

	/**
	 * @return Returns the controller.
	 */
	public Controller getController() {
		return controller;
	}

	/**
	 * @param controller The controller to set.
	 */
	public void setController(Controller controller) {
		this.controller = controller;
	}
}
