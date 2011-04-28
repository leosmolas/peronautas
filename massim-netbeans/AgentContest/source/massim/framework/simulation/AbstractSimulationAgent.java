package massim.framework.simulation;

import massim.framework.AgentParameter;

public abstract class AbstractSimulationAgent implements SimulationAgent {
	private massim.framework.SimulationAgent agent;
	private AgentParameter agentparameter;
	
	public void setAgent(massim.framework.SimulationAgent agent) {
		this.agent=agent;
	}

	public massim.framework.SimulationAgent getAgent() {
		return agent;
	}
	
	public void setAgentParameter(AgentParameter agentpar) {
		agentparameter = agentpar;
	}
}

