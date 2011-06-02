package massim.server;
import massim.framework.Observer;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;


public interface ObserverFactory {
	Observer createObserver(Element config, String simulationid) throws InvalidConfigurationException;
}
