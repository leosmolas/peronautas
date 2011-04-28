package massim.framework.simulation;

import java.util.List;
import java.util.Vector;

/**
 * This class provides some reasonable defaults for relatively simple simulations and allows the reuse of classes written
 * for such simulations. In contrast to DefaultSimulation there are som import paradigm shifts.
 * 
 * There is now an array of agent states. Each agent state contains all agent specific state information. This might be the
 * agents condition, position or score. Additionally there is one world state. It contains all information about the world meaning everything
 * that is not directly associated with one agent. These two states describe the whole state of the simulation implementation specific
 * side.
 * 
 * Please note that those states are also exported via massim.Simulation.getSimulationState(). So they are supposed to be "clean", i.e.
 * not containing any helper structures that are just there to ease access to data inside the state, like hashtables or something similar. 
 *
 */
public abstract class AbstractSimulation extends massim.framework.DefaultSimulation {
	
	private List<SimulationAgent> agentList;
	private SimpleSimulationConfiguration.AgentConfiguration[] agentConfigurations;
	private int steps;
	
	public AbstractSimulation() {
	}
	
	public void startSimulation() {
		steps=0;
		agentList = new Vector<SimulationAgent>();
		for (SimpleSimulationConfiguration.AgentConfiguration e : agentConfigurations) {
			SimulationAgent simagent = null;
			try {
				simagent = (SimulationAgent) e.agentClass.newInstance();
			} catch (Exception e1)
			{
				e1.printStackTrace();
			}//FIXME
			massim.framework.SimulationAgent agent=createAgent(e.agentParameter);
			simagent.setAgent(agent);
			simagent.setAgentParameter(e.agentParameter);
			
			agentList.add(simagent);
		}
		initializeSimpleSimulation();
		runInitAgents();
	}
	
	public void stepSimulation() {
		steps++;
		preSimulationStep();
		runAgents();
		simulationStep();
		postSimulationStep();
	}
	
	public massim.framework.SimulationState getSimulationState() {
		SimulationStateImpl result = new SimulationStateImpl();
		result.simulationState = getSimpleSimulationState();
		result.agentStates = new AgentState[agentList.size()];
		for (int i=0;i<agentList.size();i++) {
			result.agentStates[i]=agentList.get(i).getAgentState();
		}
		result.steps=steps;
		return result;
	}
	
	public void configureSimulation(massim.framework.SimulationConfiguration config) {
		// create agent objects
		SimpleSimulationConfiguration c=(SimpleSimulationConfiguration) config;
		agentConfigurations = c.getAgentConfigurations();
	}
	
	/**
	 * This method is supposed to return the state of the world, excluding agents.
	 * @return state of the world
	 */
	public abstract WorldState getSimpleSimulationState();
	
	/**
	 * This method is supposed to initialize a simulation. It should setup a world state
	 * and make everything ready for the first step. When this method is called agents are
	 * already initialized and available. Possibly it will also have to initialize their states.
	 */
	public abstract void initializeSimpleSimulation();
	
	/**
	 * Retrieves an array of SimulationAgent objects that participate in this simulation.
	 * @return simulation agents
	 */
	public SimulationAgent[] getAgents() {
		SimulationAgent[] result = new SimulationAgent[agentList.size()]; 
		return agentList.toArray(result);
	}
	
	/**
	 * Retrieves the number of the steps that have been started to process in this simulation run.
	 * @return number of steps
	 */
	public int getSteps() {
		return steps;
	}
	
	/**
	 * Set number of the step. this is important when the server is started with recovery mode
	 * In recovery mode a simulation will run continuously at the step where he was stopped  
	 * @param steps
	 */
	public synchronized void setSteps(int steps) {
		this.steps = steps;
	}

	/**
	 * This method is supposed to make necessary changes to the world that must occur before
	 * agents act in that step.
	 */
	public abstract void preSimulationStep();
	
	/**
	 * This method is supposed to make necessary changes to the world that must occur after
	 * agent act in have acted in that step.
	 */
	public abstract void postSimulationStep();
	
	/**
	 * This method is supposed to let agents act.
	 */
	public abstract void runAgents();
	public abstract void simulationStep();
	public abstract void runInitAgents();
	public abstract void runFinalAgents();

	/**
	 * This method is supposed to make final actions before the simulation is shut down. Agents are still available in this
	 * method.
	 *
	 */
	public abstract String finalizeSimpleSimulation();

	public String endSimulation() {
		String winner = finalizeSimpleSimulation();
		runFinalAgents();
		removeAllAgents();
		return winner;
		
	}
}
