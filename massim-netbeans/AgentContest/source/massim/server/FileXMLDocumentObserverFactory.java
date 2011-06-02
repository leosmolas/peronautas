package massim.server;
import static massim.test.ConfigurationUtilities.getNewInstanceFromConfig;

import java.io.File;

import massim.framework.Observer;
import massim.framework.XMLFileWriter;
import massim.framework.XMLOutputObserver;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;


public class FileXMLDocumentObserverFactory implements ObserverFactory {

	public Observer createObserver(Element config, String simulationid) throws InvalidConfigurationException {
		String className = config.getAttribute("xmlobserver");
		String prefix = config.getAttribute("xmlobserverpath");
		XMLOutputObserver r = getNewInstanceFromConfig(className);
		XMLFileWriter obs = new XMLFileWriter();
		obs.setFile(new File(prefix+simulationid+".xml"));
		r.addObserver(obs);
		return r;
	}
}
