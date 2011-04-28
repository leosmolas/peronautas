package massim.framework;

import java.util.concurrent.Future;

/**
 * This interface offers access to some basic features for agent management.
 * This includes agent creation, removal and to send an agent a perception while
 * receiving an action in return.
 * 
 * Note that implementations of these methods may have to be synchronized. You can also initiate the
 * creation and acting of agents and retrieve their results later using
 * java.concurrent.Future.
 *
 */
public interface AgentManager extends Component {
	/**
	 * Create an agent that fulfills criteria mentioned in parameter.
	 * @param parameter agent creation criteria 
	 * @return agent id
	 */
	SimulationAgent createAgent(AgentParameter parameter);

	/**
	 * This is the non-blocking version of createAgent.
	 * @param m
	 * @return
	 */
	Future<SimulationAgent> concurrentCreateAgent(AgentParameter m);
	
}
