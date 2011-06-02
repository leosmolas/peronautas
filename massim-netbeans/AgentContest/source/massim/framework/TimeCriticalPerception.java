package massim.framework;

/**
 * Classes that implement this interface represent perceptions that should take maximally a certain amount of time to
 * process and respond to it. This amount of time may or may not depend on a default time that the agent has to react. 
 *
 */

public interface TimeCriticalPerception extends Perception {
	/**
	 * Returns the time in milliseconds the agent is allowed to use to process and react to this perception.
	 * @param normaltimeout default timeout
	 * @return special timeout in milliseconds
	 */
	long getTimeout(long normaltimeout);
}
