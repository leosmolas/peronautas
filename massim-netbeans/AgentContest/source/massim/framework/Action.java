package massim.framework;

import java.io.Serializable;
/**
 * This interface must be implemented by any class that represents an action performed by some agent. 
 * 
 * This interface does so far nothing (except implying that a class must be serializable) and it's primary
 * purpose is to have typesafety.
 *
 */
public interface Action extends Serializable {
}
