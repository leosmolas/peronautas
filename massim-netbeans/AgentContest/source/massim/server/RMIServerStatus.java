package massim.server;


import java.net.Socket;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import massim.framework.connection.UsernamePasswordAccount;
import massim.framework.connection.XMLSocketSimulationAgent;
import massim.framework.rmi.XMLDocumentServer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class RMIServerStatus extends UnicastRemoteObject implements XMLDocumentServer {
	private ServerSimulationAgents serversimulationagents;
	
	public RMIServerStatus(ServerSimulationAgents serversimulationagents) throws RemoteException {
		super();
		this.serversimulationagents = serversimulationagents;
	}
	public Document getXMLDocument() throws RemoteException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setNamespaceAware(true);
		Document doc = null;
		try {
			doc = factory.newDocumentBuilder().newDocument();
		} catch (ParserConfigurationException e) {}
		Element root = doc.createElement("status");
		doc.appendChild(root);
		for (int i=0;i<serversimulationagents.agents.length;i++) {
			Element client = doc.createElement("client");
			root.appendChild(client);
			XMLSocketSimulationAgent agent = (XMLSocketSimulationAgent) serversimulationagents.agents[i];
			UsernamePasswordAccount account = (UsernamePasswordAccount) agent.getIdentifier();
			Socket s = agent.getCurrentSocket();
			if (s!=null) {
				if (!s.isClosed()) {
					client.setAttribute("ip",""+s.getInetAddress().getHostAddress());
					client.setAttribute("port",""+s.getPort());
				}
			}
			client.setAttribute("user",""+account.getUsername());
		}
		return doc;
	}
}
