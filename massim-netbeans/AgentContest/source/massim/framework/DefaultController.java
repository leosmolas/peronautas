package massim.framework;

import java.util.concurrent.Future;

import massim.framework.util.FutureObject;
import massim.framework.ControllerReturnValue;

/**
 * This class provides some reasonable defaults for concurrentGetSimulationConfiguration, by delegating
 * it to its blocking sibling using a new thread. 
 *
 */
public abstract class DefaultController implements Controller {
	public boolean onSimulationReady() {
		return true;
	}
	
	@SuppressWarnings("unchecked")
	public Future<SimulationConfiguration> concurrentGetSimulationConfiguration() {
		final FutureObject<SimulationConfiguration> f = new FutureObject<SimulationConfiguration>();
		new Thread(){public void run() {
			f.deliver(getSimulationConfiguration());
		}}.start();
		return f;
	}

	public ControllerReturnValue controlSimulation(Simulation simulation) {
		ControllerReturnValue r = new ControllerReturnValue();
		return r;
	}
	
	public void start() {}
	public void stop() {}
}
