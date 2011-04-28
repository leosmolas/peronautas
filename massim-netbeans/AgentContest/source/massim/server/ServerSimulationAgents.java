package massim.server;


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import massim.framework.UniqueSimulationAgent;
import massim.framework.connection.Account;
import massim.framework.connection.SocketHandler;
import massim.framework.connection.UsernamePasswordAccount;
import massim.framework.connection.XMLSocketSimulationAgent;
import massim.framework.util.XMLCodec;
import massim.framework.util.XMLUtilities;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class ServerSimulationAgents {
	
	public UniqueSimulationAgent[] agents;
	public Account[] accounts;
	
	public String[] teams;
	
	public Map<Account, Object> accountTeamMap;
	public Map<Account, SocketHandler> accountSocketHandlerMap;
	
	public ServerSimulationAgents(Element xml) throws InvalidConfigurationException {
		if (xml==null) {
			throw new InvalidConfigurationException("invalid account element");
		}
		//read action class sets
		HashMap<String,HashMap<String,Class>> actionclasssets = new HashMap<String,HashMap<String,Class>>();
		NodeList nl_actionclassmaps = XMLUtilities.getChildsByTagName(xml,"actionclassmap");
		
		for (int i=0;i<nl_actionclassmaps.getLength();i++) {
			Element el_actionclassmap = (Element) nl_actionclassmaps.item(i);
			String mapname = el_actionclassmap.getAttribute("name");
			HashMap<String,Class> map = new HashMap<String,Class>();
			NodeList nl_actionclassmap = XMLUtilities.getChildsByTagName(el_actionclassmap,"actionclass");
			for (int j=0;j<nl_actionclassmap.getLength();j++) {
				Element el_actionclass = (Element) nl_actionclassmap.item(j);
				String id = el_actionclass.getAttribute("id");
				String clsname = el_actionclass.getAttribute("class");
				Class cls = null;
				try {
					cls = Class.forName(clsname);
				} catch (ClassNotFoundException e) {
					throw new InvalidConfigurationException("Class not found:"+clsname);
				}
				map.put(id,cls);
			}
			actionclasssets.put(mapname,map);
		}
		
		
		NodeList accountnodes = XMLUtilities.getChildsByTagName(xml,"account");
		int accountlistlen = accountnodes.getLength();
		
		//init fields
		agents = new UniqueSimulationAgent[accountlistlen];
		accounts = new Account[accountlistlen];
		accountTeamMap = new HashMap<Account, Object>();
		accountSocketHandlerMap = new HashMap<Account, SocketHandler>();
		Set<String> teamset = new HashSet<String>();
		
		for (int i=0;i<accountlistlen;i++) {
			Element el_account = (Element) accountnodes.item(i);
			String username=el_account.getAttribute("username");
			String password=el_account.getAttribute("password");
			String team=el_account.getAttribute("team");
			String actionclasssetname=el_account.getAttribute("actionclassmap");
			int timeout = 0;
			try {
				timeout = Integer.parseInt(el_account.getAttribute("timeout"));
			} catch (NumberFormatException e) {
				throw new InvalidConfigurationException(e);
			}
			int auxtimeout = 0;
			try {
				auxtimeout = Integer.parseInt(el_account.getAttribute("auxtimeout"));
			} catch (NumberFormatException e) {
				throw new InvalidConfigurationException(e);
			}
			int maxpacketlength = 0;
			try {
				maxpacketlength = Integer.parseInt(el_account.getAttribute("maxpacketlength"));
			} catch (NumberFormatException e) {
				throw new InvalidConfigurationException(e);
			}
			
			XMLSocketSimulationAgent agent = new XMLSocketSimulationAgent(); 
			UsernamePasswordAccount account = new UsernamePasswordAccount(username,password);

			agents[i] = agent; 
			accounts[i] = account;
			
			accountTeamMap.put(account,team);
			teamset.add(team);
			accountSocketHandlerMap.put(account,agent);
			
			agent.setIdentifier(account);
			agent.setMaximumPacketLength(maxpacketlength);
			agent.setTimeout(timeout);
			agent.setAuxiliaryTimeout(auxtimeout);

			XMLCodec.DefaultXMLToObjectConverter xmlToObjectConverter = new XMLCodec.DefaultXMLToObjectConverter();
			xmlToObjectConverter.setClassMap(actionclasssets.get(actionclasssetname));
			try {
				Class cls = Class.forName(el_account.getAttribute("defaultactionclass"));
				xmlToObjectConverter.setDefaultClass(cls);
			} catch (ClassNotFoundException e) {
				throw new InvalidConfigurationException("Class not found:"+el_account.getAttribute("defaultactionclass"));
			}
			agent.setXmlToObjectConverter(xmlToObjectConverter);
			agent.setActionClassMap(actionclasssets.get(actionclasssetname));
			
			
		}
		teams = teamset.toArray(new String[0]);
		
	}
}
