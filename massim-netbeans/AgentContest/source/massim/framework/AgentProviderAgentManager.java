package massim.framework;

import java.util.concurrent.Future;

/**
 * This agent manager will "create" agents by retrieving suitable agents from an agent provider.
 * If there is no suitable agent it will deliver a dummy agent that will always return an invalid action.
 *
 */
public class AgentProviderAgentManager extends DefaultAgentManager {

	private class SimulationAgentWrapper implements UniqueSimulationAgent {
		private UniqueSimulationAgent agent;
		public SimulationAgentWrapper(UniqueSimulationAgent agent) {
			this.agent=agent;
		}
		
		public Action getAction(Perception p) {
			return agent.getAction(p);
		}

		public void remove() {
			agentProvider.freeAgent(agent);
			agent.remove();
		}

		public Future<Action> concurrentGetAction(Perception perception) {
			return agent.concurrentGetAction(perception);
		}

		public Object getIdentifier() {
			return agent.getIdentifier();
		}
	}

	
	private AgentProvider agentProvider;

	/**
	 * Creates a new AgentProviderAgentManager based upon an AgentProvider.
	 * @param agentProvider provider of agents to be returned.
	 */
	public AgentProviderAgentManager(AgentProvider agentProvider) {
		this.agentProvider = agentProvider;
	}
	public AgentProviderAgentManager() {
		this.agentProvider = null;
	}
	
	public AgentProvider getAgentProvider() {return agentProvider;}
	public void setAgentProvider(AgentProvider p) {agentProvider=p;}
	
	private class DummyAgent extends AbstractSimulationAgent {
		public Action getAction(Perception p) {
			return new InvalidAction();
		}
		public void remove() {}
	}
	
	public SimulationAgent createAgent(AgentParameter parameter) {
		UniqueSimulationAgent[] a = agentProvider.getAgents(parameter);
		synchronized(agentProvider) {
			if (a.length>0) {
				agentProvider.allocateAgent(a[0]);
				return new SimulationAgentWrapper(a[0]);
			} else
			return new DummyAgent();
		}
	}
}
