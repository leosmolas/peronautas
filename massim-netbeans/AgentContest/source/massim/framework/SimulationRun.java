package massim.framework;

/**
 * This class provides a very simple method to configure a simulation and to run it.
 *
 */

public class SimulationRun {

	private Simulation simulation;
	private Controller controller;
	private Observer observer;
	private AgentManager agentmanager;
	
	public String runSimulation() {
		simulation.setAgentManager(agentmanager);
		SimulationConfiguration config = controller.getSimulationConfiguration();
		simulation.configureSimulation(config);
		observer.notifySimulationConfiguration(config);
		observer.notifySimulationStart();
		simulation.startSimulation();
		
		while(true) {
			observer.notifySimulationState(simulation.getSimulationState());
			ControllerReturnValue r = controller.controlSimulation(simulation);
			
			if (!r.continueSimulation) {
				break;
			} else if (r.simulationChanged) {
				observer.notifySimulationState(simulation.getSimulationState());
			}
			
			if (simulation.isFinished()) break;
			simulation.stepSimulation();
		}
		observer.notifySimulationEnd();
	
		String winner = simulation.endSimulation();
		return winner;
	}

	public SimulationRun() {
		simulation=null;
		controller=null;
		observer=null;
		agentmanager=null;
	}

	/**
	 * @return Returns the agentmanager.
	 */
	public AgentManager getAgentmanager() {
		return agentmanager;
	}

	/**
	 * @param agentmanager The agentmanager to set.
	 */
	public void setAgentmanager(AgentManager agentmanager) {
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

	/**
	 * @return Returns the observer.
	 */
	public Observer getObserver() {
		return observer;
	}

	/**
	 * @param observer The observer to set.
	 */
	public void setObserver(Observer observer) {
		this.observer = observer;
	}

	/**
	 * @return Returns the simulation.
	 */
	public Simulation getSimulation() {
		return simulation;
	}

	/**
	 * @param simulation The simulation to set.
	 */
	public void setSimulation(Simulation simulation) {
		this.simulation = simulation;
	}
	
	
}
