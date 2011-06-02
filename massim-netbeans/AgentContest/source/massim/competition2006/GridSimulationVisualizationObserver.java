package massim.competition2006;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashMap;

import massim.framework.DefaultObserver;
import massim.framework.SimulationConfiguration;
import massim.framework.SimulationState;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.SimulationStateImpl;
import massim.server.ServerSimulationContext;
import massim.server.ServerSimulationContextReceiver;
import massim.visualization.GridPolicy;

/**
 * This VisualizationObserver takes care about the visualization (svg-files).
 * 
 */
public class GridSimulationVisualizationObserver extends DefaultObserver implements ServerSimulationContextReceiver {
	public String simulationName;
	public String tournamentName;
	private GridPolicy output;
	private String outputFolder;
	private int htaccess = 1;
	private int laststep = -1;
	public GridSimulationVisualizationObserver() {
		output = new GridPolicy();
	}
	
	public void notifySimulationStart() {
		laststep=-1;
	}

	public void notifySimulationConfiguration(SimulationConfiguration simconf) {
		GridSimulationConfiguration simconfig = (GridSimulationConfiguration) simconf;
		System.err.println("Chante mooremeomroermoemroemroemroemroeomr");
		// create svg-output-file.
		// create Subfolder
		Date dt = new Date();
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm");
		outputFolder = simulationName + "_" + df.format(dt);
		output.createFolder(outputFolder);
		//FIXME
		//drawSimulation(state);
		//FIXME value of htaccess is not read from the config file 
		if (htaccess == 1) {
			// create .htaccess
			try {
				output.createFile(".htaccess",
						"AuthType Basic\nAuthName \"Restricted Files\" \nAuthUserFile "
								+ new File(".").getAbsolutePath()
								+ "/.htusers \nRequire user "
								+ simconfig.teamName0 + " " + simconfig.teamName1);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public void notifySimulationEnd() {
		output.createPreviewSvg();
	}

	public void notifySimulationState(SimulationState state) {
		SimulationStateImpl tcstate = (SimulationStateImpl) state;
		
		if (laststep!=tcstate.steps) {
			drawSimulation(tcstate.steps, (GridSimulationWorldState) tcstate.simulationState, (AgentState[]) tcstate.agentStates);
		}
	}

	public void start() {
		// TODO Auto-generated method stub

	}

	public void stop() {
		// TODO Auto-generated method stub

	}
	
	private void drawSimulation(int step, GridSimulationWorldState state, AgentState[] agentstates) {
		//SimulationAgent agents[] = this.getAgents();
		// draw grid
		System.err.println("ouput:"+output);
		System.err.println("state:"+state);
		output.create();
		output.drawGrid(0, state.sizex, state.sizey);
		for (int x = 0; x < state.sizex; x++) {
			for (int y = 0; y < state.sizey; y++) {
				// draw depot
				if (state.board[x][y].depot) {
					output.drawGoldDepot(0, x, y);
				}
				// draw obstacles
				if (state.board[x][y].obstacle) {
					output.drawTrees(0, x, y);
				}
				// draw marks
				if (state.board[x][y].mark) {
					output.drawText(0, state.board[x][y].markText, x, y);
				}
				// draw items
				if (state.board[x][y].gold) {
					output.drawGold(0, x, y);
				}
			}
		}

		// draw agents and statistic-output
		// general statistics
		LinkedHashMap<String, String> statistic = new LinkedHashMap<String, String>();
		statistic.put("Step", state.currentStep.toString());
		statistic.put(state.teamName[0] + " Score", state.teamScore[0].toString());
		statistic.put(state.teamName[1] + " Score", state.teamScore[1].toString());
		statistic.put("Depot", "(" + state.depotx + "," + state.depoty + ")");
		statistic.put("GoldItems", state.numberOfGoldItems.toString());
		statistic.put("Size", "(" + state.sizex + "," + state.sizey + ")");
		statistic.put("Fog Prob", state.informationDistortionProbability.toString()+ "%");
		statistic.put("Skill Prob", state.actionSuccessProbability.toString()+ "%");

		output.setHeadInformationFirstLevel(tournamentName);
		output.setHeadInformationSecondLevel(simulationName);

		for (Integer i = 0; i < agentstates.length; i++) {
			// agents
			String color = "blue";
			GridSimulationAgentState agentstate = (GridSimulationAgentState)agentstates[i];
			if (agentstate.team == state.teamName[0]) {
				color = "red";
			}
			if (agentstate.agentHoldsGold) {
				output.drawGoldDiggerWithGold(i, color, agentstate.posx,
						agentstate.posy);
			} else {
				output.drawGoldDigger(i, color, agentstate.posx,
				agentstate.posy);
			}

			// statistics
			statistic.put(i + ":" + "Name", agentstate.name);
			if (agentstate.actionFailed) {
				statistic.put(i + ":" + "Action", agentstate.lastAction
						+ " (action failed)");
			} else {
				statistic.put(i + ":" + "Action", agentstate.lastAction);
			}
		}
		output.setGridStatistic(statistic);
		laststep=step;
		output.save();
	}

	public void setSimulationContext(ServerSimulationContext context) {
		simulationName = context.getSimulationName();
		tournamentName = context.getGlobalName();
	}

}
