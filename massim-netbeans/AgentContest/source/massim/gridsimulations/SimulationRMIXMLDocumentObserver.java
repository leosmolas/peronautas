package massim.gridsimulations;

import massim.framework.rmi.XMLDocumentObserver;
import massim.server.ServerSimulationContext;
import massim.server.ServerSimulationContextReceiver;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
/**
 * This RMIXMLDocumentObserver provides the simulation statistics for the webserver and the servermonitor.
 * 
 */
public abstract class SimulationRMIXMLDocumentObserver extends XMLDocumentObserver implements ServerSimulationContextReceiver{
	protected ServerSimulationContext serverSimulationContext;
	public static String simulationName="";
	protected Element createItem(Document doc, String string, Integer k, Integer j) {
		
		Element el = doc.createElement(string);
		el.setAttribute("PositionX", k.toString());
		el.setAttribute("PositionY", j.toString());
		return el;
		
	}
	public void setSimulationContext(ServerSimulationContext context) {
		serverSimulationContext = context;
	}

}
