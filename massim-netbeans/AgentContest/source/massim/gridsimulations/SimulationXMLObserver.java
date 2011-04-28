package massim.gridsimulations;

import java.io.FileOutputStream;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import massim.framework.XMLOutputObserver;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This XMLObserver provides the simulation statistics and save it into a file.
 * 
 */
public abstract class SimulationXMLObserver extends XMLOutputObserver {
	
	protected Element el_root;
	protected Document doc;

	/**
	 * This constructor creates the document.
	 */
	public SimulationXMLObserver() {
		
		doc = getDocument();
		el_root = doc.createElement("statistics");
		doc.appendChild(el_root);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationStart()
	 */
	public void notifySimulationStart() {
		
		Element el_start = doc.createElement("simulationstart");
		el_root.appendChild(el_start);
		setChanged();
		notifyObservers();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationEnd()
	 */
	public void notifySimulationEnd() {
		
		Element el_start = doc.createElement("simulationend");
		el_root.appendChild(el_start);
		setChanged();
		notifyObservers();

	}

	protected void saveInfile(String filename){

	try {
		FileOutputStream out;
		out = new FileOutputStream(filename+".xml");
		Transformer transformer = TransformerFactory.newInstance()
				.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.transform(new DOMSource(doc), new StreamResult(out));
	} catch (Exception e) {
		// TODO: handle exception
	}
	}

	


	protected Element createItem(Document doc, String string, Integer k, Integer j) {
		
		Element el = doc.createElement(string);
		el.setAttribute("PositionX", k.toString());
		el.setAttribute("PositionY", j.toString());
		return el;
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Component#start()
	 */
	public void start() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Component#stop()
	 */
	public void stop() {
	}
}
