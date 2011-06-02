package massim.server;

import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.log;
import static massim.test.ConfigurationUtilities.getClassFromConfig;
import static massim.test.ConfigurationUtilities.getNewInstanceFromConfig;
import static massim.test.ConfigurationUtilities.getObjectFromConfig;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import massim.framework.BroadcastObserver;
import massim.framework.Observer;
import massim.framework.Simulation;
import massim.framework.SimulationConfiguration;
import massim.framework.TeamAgentParameter;
import massim.framework.XMLOutputObserver;
import massim.framework.simulation.DefaultSimpleSimulationConfiguration;
import massim.framework.simulation.SimpleSimulationConfiguration.AgentConfiguration;
import massim.framework.util.XMLUtilities;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;



	public class ServerSimulationRun2 extends massim.framework.SimulationRun {
		public XMLOutputObserver xmlstatisticsobserver=null;
		
		protected BroadcastObserver broadcastObserver;
		private ConfigurationDelivererController configurationDelivererController;
		private List<Observer> observer;
		
		
		public ServerSimulationRun2(Element xmlconfiguration, Map<String,String> teammapping, final String name, String tournamentname) 
		throws InvalidConfigurationException {
			observer = new LinkedList<Observer>();

			//initialize broadbast Observer
			broadcastObserver = new BroadcastObserver();
			
			//set broadcast Observer as primary observer for this simulation run
			setObserver(broadcastObserver);
			
//			Necessary for the monitor :-( FIXME
			//initialize raw dumping observer
			ObserverFactory oc1 = new ObjectDumperObserverFactory();
			Observer o1 = oc1.createObserver(xmlconfiguration, name);
			broadcastObserver.addObserver(o1);
			observer.add(o1);

			// create RMI XML observer for monitor 
			if(!xmlconfiguration.getAttribute("rmixmlobserver").equalsIgnoreCase(""))
			{
				ObserverFactory oc2 = new RMIXMLDocumentObserverFactory();
				Observer o2 = oc2.createObserver(xmlconfiguration, "monitor"+name);
				broadcastObserver.addObserver(o2);
				observer.add(o2);	
			}
			// create RMI XML observer for web server  
			
			
			if(!xmlconfiguration.getAttribute("rmixmlobserverweb").equalsIgnoreCase(""))
			{
				ObserverFactory oc2 = new RMIXMLDocumentObserverWebInterfaceFactory();
				Observer o2 = oc2.createObserver(xmlconfiguration, "");
				broadcastObserver.addObserver(o2);
				observer.add(o2);	
			}
			
			// create XML observer
			if(!xmlconfiguration.getAttribute("xmlobserver").equalsIgnoreCase(""))
			{
			ObserverFactory oc3 = new FileXMLDocumentObserverFactory();
			Observer o3 = oc3.createObserver(xmlconfiguration, name);
			broadcastObserver.addObserver(o3);
			observer.add(o3);
			}
		   	
			if(!xmlconfiguration.getAttribute("xmlstatisticsobserver").equalsIgnoreCase("")){
			xmlstatisticsobserver = getNewInstanceFromConfig(xmlconfiguration.getAttribute("xmlstatisticsobserver"));
			broadcastObserver.addObserver(xmlstatisticsobserver);
			observer.add(xmlstatisticsobserver);
			}
			// create visualisation observer
			String className = xmlconfiguration.getAttribute("visualisationobserver");
//			String prefix = xmlconfiguration.getAttribute("xmlobserverpath");
			if (!className.equalsIgnoreCase("")) {
//				System.err.println("Classname:"+className);
				Observer o4 = getNewInstanceFromConfig(className);
				broadcastObserver.addObserver(o4);
				observer.add(o4);
			}
			final String tn = tournamentname;
			final Map<String, String> tm = teammapping;
			ServerSimulationContext context = new ServerSimulationContext() {
				public String getSimulationName() {return name;}
				public String getGlobalName() {return tn;}
				public String[] getTeamNames() {
					String[] r = new String[tm.size()];
					for (int i=0;i<r.length;i++) {
						//r[i] = tm.get
					}
					return r;
				}
			};
			
			for (int i=0;i<observer.size();i++) {
				if (observer.get(i) instanceof ServerSimulationContextReceiver) {
					ServerSimulationContextReceiver rec = (ServerSimulationContextReceiver) observer.get(i);
					rec.setSimulationContext(context);
				}
			}
	
			
			
			
			//create controller
			configurationDelivererController = (ConfigurationDelivererController) new TrivialControllerFactory().createController(xmlconfiguration, name);
			setController(configurationDelivererController);
			
			log(LOGLEVEL_NORMAL,"######################### new simulation run ----");
			
			//create simulation
			Simulation simulation = getNewInstanceFromConfig(xmlconfiguration.getAttribute("simulationclass"));
			setSimulation(simulation);
			
			NodeList nl=XMLUtilities.getChildsByTagName(xmlconfiguration, "configuration");
			if (nl.getLength()!=1) {
				log(LOGLEVEL_CRITICAL,"invalid simulation configuration");
			}
			Element el_simconf = (Element) nl.item(0);
			
			// read classes for simulation configuration, agent and it's configuration
			DefaultSimpleSimulationConfiguration simconf = getObjectFromConfig(xmlconfiguration.getAttribute("configurationclass"), el_simconf);
			
			nl=XMLUtilities.getChildsByTagName(xmlconfiguration, "agents");
			if (nl.getLength()!=1) {
				log(LOGLEVEL_CRITICAL,"invalid simulation configuration");
			}
			Element el_agents = (Element) nl.item(0);
			
			NodeList agents=XMLUtilities.getChildsByTagName(el_agents, "agent");
			
			AgentConfiguration[] agentconfigs;
			agentconfigs=new AgentConfiguration[agents.getLength()];
			for (int i=0;i<agents.getLength();i++) {
				Element el_agent = (Element) agents.item(i);
				
				// create agent configuration
				agentconfigs[i]=new AgentConfiguration();

				//configure simulation agent class
				agentconfigs[i].agentClass = getClassFromConfig(el_agent.getAttribute("agentclass"));
				
				//retrieve agent parameter class
				NodeList nl2 = XMLUtilities.getChildsByTagName(el_agent, "configuration");
				if (nl2.getLength()!=1) {
					log(LOGLEVEL_CRITICAL,"Invalid number of agent parameter configurations.");
					System.exit(1);
				}
				Element el_agentparam = (Element) nl2.item(0);
				agentconfigs[i].agentParameter=getObjectFromConfig(el_agent.getAttribute("agentcreationclass"),el_agentparam);
				if (agentconfigs[i].agentParameter instanceof TeamAgentParameter) {
					((TeamAgentParameter) agentconfigs[i].agentParameter).setTeam(teammapping.get(el_agent.getAttribute("team")));
				}
			}

			if (simconf instanceof ServerSimulationConfiguration) {
				ServerSimulationConfiguration s = (ServerSimulationConfiguration) simconf;
				s.setSimulationName(name);
				for (int i=0;i<agents.getLength();i++) {
					Element el_agent = (Element) agents.item(i);
					s.setTeamName(i,teammapping.get(el_agent.getAttribute("team")));
				}
				s.setTournamentName(tournamentname);
			}
			
			simconf.setAgentConfigurations(agentconfigs);
			configurationDelivererController.setConfiguration((SimulationConfiguration)simconf);
		}

		@Override
		public String runSimulation() {
			
			broadcastObserver.start();
			for (int i=0;i<observer.size();i++) {
				observer.get(i).start();
			}
			
			getController().start();
			getSimulation().start();
			String winner = super.runSimulation();
			getSimulation().stop();
			getController().stop();

			for (int i=observer.size()-1;i>=0;i--) observer.get(i).stop();

			broadcastObserver.stop();
			return winner;
		}
	}

