package massim.server;
import massim.framework.Controller;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Element;


public class TrivialControllerFactory implements ControllerFactory {

	public Controller createController(Element config, String simulationid)
			throws InvalidConfigurationException {
		ConfigurationDelivererController r = new ConfigurationDelivererController();
		/*
		NodeList nl=XMLUtilities.getChildsByTagName(config, "agents");
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
			s.setSimulationName(simulationid);
			for (int i=0;i<agents.getLength();i++) {
				Element el_agent = (Element) agents.item(i);
				s.setTeamName(i,teammapping.get(el_agent.getAttribute("team")));
			}
			s.setTournamentName(tournamentname);
		}
		
		simconf.setAgentConfigurations(agentconfigs);*/
		return r;
	}
}
