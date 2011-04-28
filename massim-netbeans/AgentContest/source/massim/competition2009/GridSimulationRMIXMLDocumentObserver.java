package massim.competition2009;

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
	
	
	public void generateDocument(Document doc, SimulationState simstate) {
	
		Element el_root = doc.getDocumentElement();
		SimulationStateImpl simplestate = (SimulationStateImpl) simstate;
		GridSimulationWorldState worldState = (GridSimulationWorldState) simplestate.simulationState;
		String[] teamName = new String[2];
		Integer[] teamScore = new Integer[2];
		teamName[0] = worldState.teamName[0];
		teamName[1] = worldState.teamName[1];
		teamScore[0] = worldState.teamScore[0];
		teamScore[1] = worldState.teamScore[1];
		
		Element el_state = doc.createElement("state");
		el_state.setAttribute("simulation",""+worldState.simulationName);
		el_state.setAttribute("teamname0", teamName[0]);
		el_state.setAttribute("teamname1", teamName[1]);
		el_state.setAttribute("team0-score", teamScore[0].toString());
		el_state.setAttribute("team1-score", teamScore[1].toString());
		el_state.setAttribute("step", Integer.toString(simplestate.steps));
		el_state.setAttribute("sizex", Integer.toString(worldState.sizex));
		el_state.setAttribute("sizey", Integer.toString(worldState.sizey));
		el_state.setAttribute("number-of-cows",
				Integer.toString(worldState.numberOfCows));
		el_state.setAttribute("number-of-obstacles",
				Integer.toString(worldState.numberOfObstacles));
		el_state.setAttribute("number-of-agents", Integer.toString(worldState.numberOfAgents));
		el_state.setAttribute("number-of-fences", ""+worldState.numberOfFences);		
		el_state.setAttribute("number-of-steps",""+worldState.maxNumberOfSteps);	
		
		// AgentStates
		for (Integer i = 0; i < simplestate.agentStates.length; i++) {
			Element el_agent = doc.createElement("agent");
			GridSimulationAgentState agentstate = (GridSimulationAgentState) simplestate.agentStates[i];
			el_agent.setAttribute("number", i.toString());
			el_agent.setAttribute("posx", agentstate.posx.toString());
			el_agent.setAttribute("posy", agentstate.posy.toString());
			el_agent.setAttribute("score", agentstate.score.toString());
			el_agent.setAttribute("team", agentstate.team);
			el_agent.setAttribute("name", agentstate.name);
			el_state.appendChild(el_agent);
		
		}
		
		for(int i = 0 ; i<worldState.fenceDirection.length; i++){
			Element el = doc.createElement("switch");
			el.setAttribute("posx", ""+worldState.switchX[i]);
			el.setAttribute("posy", ""+worldState.switchY[i]);
			el.setAttribute("direction", ""+worldState.fenceDirection[i]);
			el_state.appendChild(el);	
		}
		
		for(int i = 0 ; i< worldState.sizex; i++){
			for(int j = 0; j < worldState.sizey;j++){
				
				 if(worldState.board[i][j].cow){
					
					Element el = doc.createElement("cow");
					el.setAttribute("posx", ""+i);
					el.setAttribute("posy", ""+j);
					el_state.appendChild(el);
				}
				 else if(worldState.board[i][j].obstacle){
					
					Element el = doc.createElement("tree");
					el.setAttribute("posx", ""+i);
					el.setAttribute("posy", ""+j);
					el_state.appendChild(el);
					
				}
				 else if(worldState.board[i][j].fence 
						&& !worldState.board[i][j].open){
					
					Element el = doc.createElement("fence");
					el.setAttribute("posx", ""+i);
					el.setAttribute("posy", ""+j);
					el_state.appendChild(el);
					
				}
				 else if(worldState.board[i][j].isStable1()){
					Element el = doc.createElement("stable1");
					el.setAttribute("posx", ""+i);
					el.setAttribute("posy", ""+j);
					el_state.appendChild(el);
				}
				 else if(worldState.board[i][j].isStable2()){
					Element el = doc.createElement("stable2");
					el.setAttribute("posx", ""+i);
					el.setAttribute("posy", ""+j);
					el_state.appendChild(el);
				 }
			}
		}
		//el_state.appendChild(newChild)<hung posx="15" posy="15"/>
		//Element el = doc.createElement("hung");
		//el.setAttribute("posx", ""+15);
		//el.setAttribute("posy", ""+15);
		//el_state.appendChild(el)
		el_root.appendChild(el_state);
	//	BackupWriter.write(doc, "monitorDoc", "monitor.xml");

	}
	
}
