package massim.server;


import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.io.IOException;

import massim.framework.connection.InetSocketListener;
import massim.framework.util.XMLUtilities;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class ServerInetSocketListener {
	public final int AGENT_PORT_DEFAULT=12300; 
	public final int AGENT_BACKLOG_DEFAULT=10; 
	
	public InetSocketListener object;
	
	public ServerInetSocketListener(Element xml) throws InvalidConfigurationException { 
		//network configuration
		int agent_port=AGENT_PORT_DEFAULT;
		int agent_backlog=AGENT_BACKLOG_DEFAULT;
		
		NodeList nl=XMLUtilities.getChildsByTagName(xml, "network-agent");
		if (nl.getLength()==1) {
			try {
				int v=Integer.parseInt(((Element)nl.item(0)).getAttribute("port"));agent_port=v;
			} catch (NumberFormatException e) {
				throw new InvalidConfigurationException("unable to parse agent port from configuration");
			}
			try {
				int v=Integer.parseInt(((Element)nl.item(0)).getAttribute("backlog"));agent_backlog=v;
			} catch (NumberFormatException e) {
				throw new InvalidConfigurationException("unable to parse agent backlog from configuration");
			}
		}
		
		//create socket listener
		InetSocketListener socketlistener = null;
		try {
			object = new InetSocketListener(agent_port,agent_backlog,null);
		} catch (IOException e) {
			log(LOGLEVEL_ERROR,"IO Error while creating InetSocketListener. Aborting...");
			e.printStackTrace();
			throw new RuntimeException(e);
		}
	}
	
	
}
