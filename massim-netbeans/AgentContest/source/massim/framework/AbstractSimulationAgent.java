package massim.framework;

import java.util.concurrent.Future;

import massim.framework.util.FutureObject;

public abstract class AbstractSimulationAgent implements SimulationAgent {
	@SuppressWarnings("unchecked")
	public Future<Action> concurrentGetAction(final Perception perception) {
		final FutureObject<Action> f = new FutureObject<Action>();
		new Thread(){public void run() {
			f.deliver(getAction(perception));
		}}.start();
		return f;
	}
}
