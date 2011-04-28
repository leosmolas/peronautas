package massim.server;
import massim.framework.Controller;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;


public interface ControllerFactory {
	Controller createController(Element config, String simulationid) throws InvalidConfigurationException;
}
