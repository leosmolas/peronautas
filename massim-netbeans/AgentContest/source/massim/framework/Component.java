package massim.framework;

/**
 * This interface is implemented by all component-like classes. Components are classes that are supposed
 * to do something if and only if they are started via start. They will shut down their activity when they
 * are stopped.
 * 
 * The idea of this interface is to be able to launch a set of totally unknown components or to shut them down.
 *
 */
public interface Component {
	/**
	 * Tell the component to start activity.
	 */
	void start();
	/**
	 * Tell the component to cease activity.
	 */
	void stop();
}
