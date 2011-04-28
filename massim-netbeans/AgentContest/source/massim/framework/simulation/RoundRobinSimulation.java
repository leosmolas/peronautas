package massim.framework.simulation;

import massim.framework.Action;
import massim.framework.Perception;

/**
 * This provides a round-robin-style default to let agents act.
 */
public abstract class RoundRobinSimulation extends AbstractSimulation {

	public void runAgents() {
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		// Let agents act.
		for(int i=0;i<agents.length;i++) {
			Perception p = agents[i].createPerception(simstate,agentstates);
			Action a     = agents[i].getAgent().getAction(p);
			agents[i].processAction(a, simstate, agentstates);
			//agents[i].act(simstate,agentstates);
		}
	}
	public void runInitAgents() {
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		// Let agents act.
		for(int i=0;i<agents.length;i++) {
			Perception p = agents[i].createInitialPerception(simstate,agentstates);
			Action a = agents[i].getAgent().getAction(p);
			//agents[i].actInit(simstate,agentstates);
		}
	}
	public void runFinalAgents() {
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		// Let agents act.
		for(int i=0;i<agents.length;i++) {
			Perception p = agents[i].createFinalPerception(simstate,agentstates);
			Action a = agents[i].getAgent().getAction(p);
			//agents[i].actFinal(simstate,agentstates);
		}
	}
}
