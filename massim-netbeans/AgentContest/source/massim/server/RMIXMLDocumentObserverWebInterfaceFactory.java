package massim.server;
import static massim.test.ConfigurationUtilities.getNewInstanceFromConfig;
import massim.framework.Observer;
import massim.framework.rmi.XMLDocumentObserver;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;

public class RMIXMLDocumentObserverWebInterfaceFactory implements ObserverFactory {
	public Observer createObserver(Element config, String simulationid) throws InvalidConfigurationException {
		String className=config.getAttribute("rmixmlobserverweb");
		String servicePrefix = "xmlsimulation";
		// create RMI XML observer
		XMLDocumentObserver r = null;
		try {
			r = getNewInstanceFromConfig(className);
		} catch (InvalidConfigurationException e) {
			throw new RuntimeException(e);
			
		}
		r.setServiceName(servicePrefix+simulationid);
	//	r.setServiceName(servicePrefix);
		return r;
	}
}
