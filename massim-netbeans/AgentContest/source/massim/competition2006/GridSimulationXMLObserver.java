package massim.competition2006;
import massim.framework.SimulationState;
import massim.framework.simulation.SimulationStateImpl;
import massim.gridsimulations.SimulationXMLObserver;

import org.w3c.dom.Element;

/**
 * This XMLObserver provides the simulation statistics and save it into a file.
 * 
 */
public class GridSimulationXMLObserver extends SimulationXMLObserver {

	/**
	 * This constructor creates the document.
	 */
	public GridSimulationXMLObserver() {
		super();
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationState(massim.SimulationState)
	 */
	public void notifySimulationState(SimulationState state) {
		SimulationStateImpl simplestate = (SimulationStateImpl) state;
		GridSimulationWorldState worldState = (GridSimulationWorldState) simplestate.simulationState;
		String[] teamName = new String[2];
		Integer[] teamScore = new Integer[2];
		teamName[0] = worldState.teamName[0];
		teamName[1] = worldState.teamName[1];
		teamScore[0] = worldState.teamScore[0];
		teamScore[1] = worldState.teamScore[1];
		Element el_state = doc.createElement("state");
		
		el_state.setAttribute("Simulation",worldState.simulationName);
		el_state.setAttribute(teamName[0] + "-Score", teamScore[0].toString());
		el_state.setAttribute(teamName[1] + "-Score", teamScore[1].toString());
		el_state.setAttribute("Step", Integer.toString(simplestate.steps));
		el_state.setAttribute("SizeX", worldState.sizex.toString());
		el_state.setAttribute("SizeY", worldState.sizey.toString());
		el_state.setAttribute("Number-of-gold-nuggets", worldState.numberOfGoldItems.toString());
		el_state.setAttribute("Number-of-obstacles", worldState.numberOfObstacles.toString());
		el_state.setAttribute("Number-of-agents", worldState.numberOfAgents.toString());
		el_state.setAttribute("DepotpositionX", worldState.depotx.toString());
		el_state.setAttribute("DepotpositionY", worldState.depoty.toString());
		el_state.setAttribute("FogProbability",	worldState.informationDistortionProbability.toString());
		writeAdditionalInformation(simplestate, worldState, el_state);
		el_root.appendChild(el_state);
	}
	
	private void writeAdditionalInformation(SimulationStateImpl simplestate, GridSimulationWorldState worldState, Element el_state) {
		el_state.setAttribute("SkillProbability",worldState.actionSuccessProbability.toString());
		// AgentStates
		for (Integer i = 0; i < simplestate.agentStates.length; i++) {
			Element el_agent = doc.createElement("agent");
			GridSimulationAgentState agentstate = (GridSimulationAgentState) simplestate.agentStates[i];
			el_agent.setAttribute("Number", i.toString());
			el_agent.setAttribute("PositionX", agentstate.posx
					.toString());
			el_agent.setAttribute("PositionY", agentstate.posy
					.toString());
			el_agent.setAttribute("Score", agentstate.score
					.toString());
			el_agent.setAttribute("Team", agentstate.team);
			el_agent.setAttribute("HoldsGold",
					agentstate.agentHoldsGold.toString());
			el_agent.setAttribute("AgentInDepot",
					agentstate.agentInDepot.toString());
			el_agent.setAttribute("Name", agentstate.name);
			el_state.appendChild(el_agent);
		}
	}
}
