package massim.framework.simulation;

import massim.framework.AgentParameter;

public interface SimpleSimulationConfiguration extends massim.framework.SimulationConfiguration {
	class AgentConfiguration {
		public AgentParameter agentParameter;
		public Class<SimulationAgent> agentClass;
	}
	
	AgentConfiguration[] getAgentConfigurations();
}
