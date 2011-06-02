package massim.framework;

import java.util.concurrent.Future;

/**
 * This interface must be implemented by every object that is used in simulation as an agent.
 *
 */
public interface SimulationAgent {
	/**
	 * Ask the agent to act, based a new perception p aswell as other previously received perceptions in that run.
	 * @param p new perception
	 * @return action the agent wants to perform
	 */
	Action getAction(Perception p);
	
	/**
	 * Tell an agent that it was removed from the simulation and will never act again. Calling remove will also invalidate
	 * the agents knowledge about the world.
	 */
	void remove();

	/**
	 * Concurrent version of getAction.
	 * @param perception
	 * @return
	 */
	Future<Action> concurrentGetAction(final Perception perception);
}
