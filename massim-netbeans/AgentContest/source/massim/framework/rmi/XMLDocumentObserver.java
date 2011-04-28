package massim.framework.rmi;

import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import massim.framework.DefaultObserver;
import massim.framework.SimulationState;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public abstract class XMLDocumentObserver extends DefaultObserver {
	private static class XMLDocumentServerImplementation extends UnicastRemoteObject implements XMLDocumentServer {
		private static final long serialVersionUID = -1254854915029577664L;
		private Document doc;
		//private Element root;

		public XMLDocumentServerImplementation() throws RemoteException {
			super();
			doc = createDocumentSkeleton();
			/*try {
				doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
				root = doc.createElement("simulationstate");
				root.setAttribute(STATUS_KEY,STATUS_SIMULATIONNOTYETRUNNING);
				doc.appendChild(root);
			} catch (ParserConfigurationException e) {
				log(LOGLEVEL_CRITICAL,"error while creating empty document");
			}*/
		}
		public Document getXMLDocument() throws RemoteException {
			synchronized(this) {
				return doc;
			}
		}
	}
	
	private XMLDocumentServerImplementation implementation;
	private String servicename;
	private static final String STATUS_KEY="status"; 
	private static final String STATUS_SIMULATIONNOTYETRUNNING="notyetrunning";
	private static final String STATUS_SIMULATIONENDED="ended";
	private static final String STATUS_SIMULATIONRUNNING="running";
	
	public XMLDocumentObserver() {
	}
	
	public String getServiceName() {
		return servicename;
	}
	
	public void setServiceName(String servicename) {
		this.servicename=servicename;
	}
	
	@Override
	public void notifySimulationEnd() {
		synchronized(implementation) {
			implementation.doc = createDocumentSkeleton();
			implementation.doc.getDocumentElement().setAttribute(STATUS_KEY,STATUS_SIMULATIONENDED);
		}
	}

	@Override
	public void notifySimulationStart() {
		synchronized(implementation) {
			implementation.doc = createDocumentSkeleton();
			implementation.doc.getDocumentElement().setAttribute(STATUS_KEY,STATUS_SIMULATIONRUNNING);
		}
	}

	@Override
	public void notifySimulationState(SimulationState state) {
		synchronized(implementation) {
			implementation.doc = createDocumentSkeleton();
			implementation.doc.getDocumentElement().setAttribute(STATUS_KEY,STATUS_SIMULATIONRUNNING);
			generateDocument(implementation.doc, state);
		}
	}

	public abstract void generateDocument(Document doc, SimulationState state);
	
	private static Document createDocumentSkeleton() {
		Document doc=null;
		Element root=null;
		try {
			doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
			root = doc.createElement("simulationstate");
			root.setAttribute(STATUS_KEY,STATUS_SIMULATIONNOTYETRUNNING);
			doc.appendChild(root);
		} catch (ParserConfigurationException e) {
			log(LOGLEVEL_CRITICAL,"error while creating empty document");
		}
		return doc;
	}
	
	@Override
	public void start()  {
			
		try {
			log(LOGLEVEL_DEBUG,"creating...");
			implementation = new XMLDocumentServerImplementation();
			log(LOGLEVEL_DEBUG,"binding...");
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.rebind(servicename,implementation);
			log(LOGLEVEL_DEBUG,"bound");
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"error while binding (RemoteException)");
		}
		
		
	}

	@Override
	public void stop() {
		try {
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.unbind(servicename);
			UnicastRemoteObject.unexportObject(implementation, true);
			implementation=null;
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"error while binding (RemoteException)");
		} catch (NotBoundException e) {
			log(LOGLEVEL_ERROR,"error while binding (NotBound)");
		}
	}

}
