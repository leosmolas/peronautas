package massim.competition2007;

import massim.framework.SimulationState;
import massim.framework.simulation.SimulationStateImpl;
import massim.gridsimulations.SimulationRMIXMLDocumentObserver;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
/**
 * This RMIXMLDocumentObserver provides the simulation statistics for the webserver and the servermonitor.
 * 
 */
public class GridSimulationRMIXMLDocumentObserver extends SimulationRMIXMLDocumentObserver{
	
	public void generateDocument(Document doc, SimulationState state) {
		
		Element el_root = doc.getDocumentElement();
		
		SimulationStateImpl simplestate = (SimulationStateImpl) state;
		GridSimulationWorldState worldState = (GridSimulationWorldState) simplestate.simulationState;
		String[] teamName = new String[2];
		Integer[] teamScore = new Integer[2];
		teamName[0] = worldState.teamName[0];
		teamName[1] = worldState.teamName[1];
		teamScore[0] = worldState.teamScore[0];
		teamScore[1] = worldState.teamScore[1];
		Element el_state = doc.createElement("state");
		
//		el_state.setAttribute(teamName[0] + "-Score", teamScore[0].toString());
//		el_state.setAttribute(teamName[1] + "-Score", teamScore[1].toString());
		el_state.setAttribute("Simulation",serverSimulationContext.getSimulationName());
		el_state.setAttribute("TeamName0", teamName[0]);
		el_state.setAttribute("TeamName1", teamName[1]);
		el_state.setAttribute("Team0-score", teamScore[0].toString());
		el_state.setAttribute("Team1-score", teamScore[1].toString());
		el_state.setAttribute("Step", Integer.toString(simplestate.steps));
		el_state.setAttribute("SizeX", worldState.sizex.toString());
		el_state.setAttribute("SizeY", worldState.sizey.toString());
		el_state.setAttribute("Number-of-gold-nuggets",
				worldState.numberOfGoldItems.toString());
		el_state.setAttribute("Number-of-obstacles",
				worldState.numberOfObstacles.toString());
		el_state.setAttribute("Number-of-agents", worldState.numberOfAgents
				.toString());
		el_state.setAttribute("DepotpositionX", worldState.depotx.toString());
		el_state.setAttribute("DepotpositionY", worldState.depoty.toString());
		el_state.setAttribute("FogProbability",
				worldState.informationDistortionProbability.toString());
		el_state.setAttribute("SkillProbability",
				worldState.actionFailureProbability.toString());
		
		// AgentStates
		for (Integer i = 0; i < simplestate.agentStates.length; i++) {
			Element el_agent = doc.createElement("agent");
			GridSimulationAgentState agentstate = (GridSimulationAgentState) simplestate.agentStates[i];
			el_agent.setAttribute("Number", i.toString());
			el_agent.setAttribute("PositionX", agentstate.posx.toString());
			el_agent.setAttribute("PositionY", agentstate.posy.toString());
			el_agent.setAttribute("Score", agentstate.score.toString());
			el_agent.setAttribute("Team", agentstate.team);
			el_agent.setAttribute("HoldsGold", agentstate.currentItems.toString());
			/*XQM -- We inform about the number of gold items an agent carries*/
			el_agent.setAttribute("AgentInDepot", agentstate.agentInDepot.toString());
			el_agent.setAttribute("Name", agentstate.name);
			el_state.appendChild(el_agent);
		}
		el_root.appendChild(el_state);
		
		for (int k = 0; k < worldState.sizex; k++)
		{
			for (int j = 0; j < worldState.sizey; j++)
			{
				Element el = null;
				
					
				if(worldState.board[k][j].depot){
						
					el = createItem(doc, "depot", k, j);
					el_state.appendChild(el);
				}
					
				if(worldState.board[k][j].gold){
					
					el = createItem(doc, "gold", k, j);
					
					el_state.appendChild(el);
				}
					
				if(worldState.board[k][j].obstacle){
						
					el = createItem(doc, "tree", k, j);
					el_state.appendChild(el);
				}
			
			}
		}
		
		el_root.appendChild(el_state);
	}
	
}
