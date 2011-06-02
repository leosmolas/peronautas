package massim.framework;

/**
 * This interface should be implemented by any SimulationAgent that is somehow unique. It
 * provides a method to retrieve some object that identifies that agent. The deeper meaning is that
 * this object can be used to identify an agent via java.lang.Object.equals. This can be used to
 * grant an agent certain rights or to treat it in a special way. 
 *
 */
public interface UniqueSimulationAgent extends SimulationAgent {
	Object getIdentifier();
}
