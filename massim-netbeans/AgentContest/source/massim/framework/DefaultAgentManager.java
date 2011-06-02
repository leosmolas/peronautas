package massim.framework;

import java.util.concurrent.Future;

import massim.framework.util.FutureObject;

/**
 * This class provides some reasonable defaults for concurrentCreateAgent and concurrentGetAction, by delegating
 * them to their blocking siblings using a new thread. 
 *
 */
public abstract class DefaultAgentManager implements AgentManager {

	//@SuppressWarnings("unchecked")
	public Future<SimulationAgent> concurrentCreateAgent(final AgentParameter params) {
		final FutureObject<SimulationAgent> f = new FutureObject<SimulationAgent>();
		new Thread(){public void run() {
			f.deliver(createAgent(params));
		}}.start();
		return f;
	}
	
	public void start() {}
	public void stop() {}
}
