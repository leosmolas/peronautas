package massim.framework;

/**
 * This interface must be implemented by all classes that serve as a perception for an agent.
 * 
 * It's main purpose is to have type safety. Apart from that it is imperative that
 * any class that implements this interface is serializable.
 *
 */
public interface Perception extends java.io.Serializable {

}
