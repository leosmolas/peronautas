package massim.framework.simulation;


import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.log;

import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import massim.framework.Action;
import massim.framework.Perception;


public abstract class ParallelizedRandomOrderSimulation extends AbstractSimulation {

	public void runAgents() {
		class MyPair {
			public Future<Action> action;
			public SimulationAgent agent;
		}
		
		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];
		
		// Assemble agent states to one array
		for (int i=0;i<agents.length;i++) agentstates[i]=agents[i].getAgentState();
		
		Vector<MyPair> actions = new Vector<MyPair>();
		
//		Vector<Future<Action>> actions = new Vector<Future<Action>>();
		Perception perceptions[] = new Perception[agents.length];
		
		// Create perceptions.
		for(int i=0;i<agents.length;i++) {
			perceptions[i] = agents[i].createPerception(simstate,agentstates);
		}
		// Send perceptions to agents and retrieve future object for actions.
		for (int i=0;i<agents.length;i++) {
			MyPair m = new MyPair();
			m.action = agents[i].getAgent().concurrentGetAction(perceptions[i]);
			m.agent = agents[i];
			actions.add(m);
			//actions.add(agents[i].getAgent().concurrentGetAction(perceptions[i]));
		}
		//shuffle future
		Collections.shuffle(actions);
		try {
			for (int i=0;i<agents.length;i++) {
				MyPair m = actions.get(i);
				m.agent.processAction(m.action.get(), simstate,agentstates);
				//agents[i].processAction(actions.get(i).get(), simstate, agentstates);
			}
			
			//we update the world state after all of agents have charged their positions
			for (int i=0;i<agents.length;i++) {
				
				MyPair m = actions.get(i);
				//GridSimulationAgent agent = (GridSimulationAgent)m.agent;
				//update the charges
				//m.agent.updateWorldState(simstate,agents);
			}
		} catch (ExecutionException e) {
			log(LOGLEVEL_CRITICAL,"Execution of getAction failed, shouldn't happen");
		} catch (InterruptedException e) {
			log(LOGLEVEL_CRITICAL,"Thread interrupted, shouldn't happen");
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
