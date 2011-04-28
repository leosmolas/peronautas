package massim.framework;

import java.util.concurrent.Future;

/**
 * This class is meant to regulate a simulation. It provides simulation configuration but in a later more complex
 * approach it might also administrate a running simulation.
 *
 */
public interface Controller extends Component {
/**
 * Retrieve simulation configuration.
 * @return simulation configuration
 */
	SimulationConfiguration getSimulationConfiguration();
/**
 * non-blocking version of getSimulationConfiguration, return a Future object.
 * @return future of simulation configuration
 */
	Future<SimulationConfiguration> concurrentGetSimulationConfiguration();
/**
 * This method will be called whenever the controller may influence the simulation in any way.
 * 
 * If you plan to change the simulation while it is running (for agent development purposes for example) then
 * you should put something non-trivial in this method.
 * @param simulation
 * @return true iff the controller doesn't want to abort the simulation
 */
	ControllerReturnValue controlSimulation(Simulation simulation);
}
