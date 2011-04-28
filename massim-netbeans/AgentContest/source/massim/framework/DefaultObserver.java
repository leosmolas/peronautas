package massim.framework;
import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.log;
/**
 * This class serves as a good starting point for simulation observers. It is
 * strongly recommended to extend this class in order to write new observers, by
 * overriding some of the marked methods.
 * 
 * Observers in general are classes that are neither supposed to change a
 * simulation in any way. The only thing that they might change is the timing of
 * a simulation. They do only have read-only access.
 *
 */

public class DefaultObserver implements Observer {
	
	public void start() {
		log(LOGLEVEL_DEBUG);
	}

	public void stop() {
		log(LOGLEVEL_DEBUG);
	}

	public void notifySimulationStart() {
		log(LOGLEVEL_DEBUG);
	}
	public void notifySimulationEnd() {log(LOGLEVEL_DEBUG);}
	public void notifySimulationState(SimulationState state) {log(LOGLEVEL_DEBUG);}
	public void notifySimulationConfiguration(SimulationConfiguration simconf) {log(LOGLEVEL_DEBUG);}
}
