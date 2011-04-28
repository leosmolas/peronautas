package massim.server;

import massim.framework.DefaultController;
import massim.framework.Simulation;
import massim.framework.SimulationConfiguration;
/**
 * This is a very stupid implementation of Controller which will instantly deliver a previously defined configuration
 * to the simulation.
 *
 */
public class ConfigurationDelivererController extends DefaultController {
	private SimulationConfiguration configuration;
	
	public ConfigurationDelivererController(SimulationConfiguration configuration) {
		this.configuration=configuration;
	}
	
	public ConfigurationDelivererController() {
		configuration = null;
	}
	
	public SimulationConfiguration getSimulationConfiguration() {
		return configuration;
	}

	/**
	 * retrieve the predefined configuration
	 * @return Returns the configuration.
	 */
	public SimulationConfiguration getConfiguration() {
		return configuration;
	}

	/**
	 * set the predefined configuration
	 * @param configuration The configuration to set.
	 */
	public void setConfiguration(SimulationConfiguration configuration) {
		this.configuration = configuration;
	}

}
