package massim.framework.simulation;

import massim.framework.Action;
import massim.framework.AgentParameter;
import massim.framework.FinalPerception;
import massim.framework.InitialStickyPerception;
import massim.framework.Perception;

public interface SimulationAgent {
	massim.framework.SimulationAgent getAgent();
	/**
	 * Assign a low level agent to this higher level agent, to interact with.
	 * @param agent low level agent to use
	 */
	void setAgent(massim.framework.SimulationAgent agent);
	
	/**
	 * Set agent creation parameter. Implementating this method is useful to know what exact properties
	 * this agent might have.
	 * @param agentpar agent parameters to use
	 */
	void setAgentParameter(AgentParameter agentpar);
	
	/**
	 * Retrieve agent state.
	 * @return corresponding agent state.
	 */
	AgentState getAgentState();
/*	
	void act(WorldState simstate, AgentState[] agentstates);
	void actInit(WorldState simstate, AgentState[] agentstates);
	void actFinal(WorldState simstate, AgentState[] agentstates);
*/
	
	Perception createPerception(WorldState simstate, AgentState[] agentstates);
	void processAction(Action a, WorldState simstate, AgentState[] agentstates);
	
	InitialStickyPerception createInitialPerception(WorldState simstate, AgentState[] agentstates);
	FinalPerception createFinalPerception(WorldState simstate, AgentState[] agentstates);
	//void updateWorldState(WorldState simstate, SimulationAgent[] agents);
}

