package massim.framework;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.log;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;

public abstract class XMLOutputObserver extends java.util.Observable implements Observer {
	private DocumentBuilder documentbuilder;
	private Document document;
	public XMLOutputObserver() {
		DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
		documentbuilder=null;
		try {
			documentbuilder=factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {throw new RuntimeException(e);}
		document =  documentbuilder.newDocument();
	}

	public void resetDocument() {
		if (documentbuilder==null) {
			DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
			try {
				documentbuilder=factory.newDocumentBuilder();
			} catch (ParserConfigurationException e) {throw new RuntimeException(e);}
		}
		document = documentbuilder.newDocument();
	}
	
	public synchronized Document getDocument() {
		return document;
	}
	public synchronized void setDocument(Document doc) {
		document=doc;
	}
	public void notifySimulationStart() {
		log(LOGLEVEL_DEBUG);
	}
	public void notifySimulationEnd() {log(LOGLEVEL_DEBUG);}
	public void notifySimulationState(SimulationState state) {log(LOGLEVEL_DEBUG);}
	public void notifySimulationConfiguration(SimulationConfiguration simconf) {log(LOGLEVEL_DEBUG);}
}
