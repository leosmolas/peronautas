package massim.framework;

/**
 * This interface must be implemented by all classes that serve as simulation state.
 * 
 * It's main purpose is to have type safety. Apart from that it is imperative that
 * any class that implements this interface is serializable.
 *
 */
public interface SimulationState extends java.io.Serializable {

}
