package massim.competition2007;

import java.util.LinkedHashMap;

import massim.framework.simulation.AgentState;
import massim.gridsimulations.SimulationVisualizationObserver;
import massim.gridsimulations.SimulationWorldState;

/**
 * This VisualizationObserver takes care about the visualization (svg-files).
 * 
 */
public class GridSimulationVisualizationObserver extends
		SimulationVisualizationObserver {

	public GridSimulationVisualizationObserver() {
		super();

	}

	@Override
	protected synchronized void drawBackground(SimulationWorldState s_state) {
		GridSimulationWorldState state = (GridSimulationWorldState) s_state;
		output.create();
		output.drawGrid("grid", state.sizex, state.sizey);
		output.setHeadInformationFirstLevel(tournamentName);
		output.setHeadInformationSecondLevel(simulationName);
		
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
			}
		}
		output.save();
	}

	@Override
	protected synchronized void drawSimulation(int step,SimulationWorldState s_state,
			AgentState[] agentstates) {
		GridSimulationWorldState state = (GridSimulationWorldState) s_state;
		output.create();
		
		for (int x = 0; x < state.sizex; x++) {
			for (int y = 0; y < state.sizey; y++) {
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
		statistic.put(state.teamName[0] + " Color: ", " red");
		statistic.put(state.teamName[1] + " Color: ", "blue");
		statistic.put("Depot", "(" + state.depotx + "," + state.depoty + ")");
		statistic.put("GoldItems", state.numberOfGoldItems.toString());
		statistic.put("Size", "(" + state.sizex + "," + state.sizey + ")");
		statistic.put("Fog Prob", state.informationDistortionProbability.toString() + "%");
		statistic.put("Skill Prob", state.actionFailureProbability.toString() + "%");



		for (Integer i = 0; i < agentstates.length; i++) {

			// agents
			String color;
			GridSimulationAgentState agentstate = (GridSimulationAgentState) agentstates[i];

			if (agentstate.team == state.teamName[0]) {

				if (agentstate.wasPushed) {

					color = "darkred";
					agentstate.lastAction = agentstate.lastAction + " (p)";
				}

				else {

					color = "red";
				}

			}

			else {

				if (agentstate.wasPushed) {

					color = "darkblue";
					agentstate.lastAction = agentstate.lastAction + " (p)";
				}

				else {

					color = "blue";
				}
			}

			if (agentstate.agentHoldsGold) {

				output.drawGoldDiggerWithGold(i, color, agentstate.posx,
						agentstate.posy);

			}

			else {

				output.drawGoldDigger(i, color, agentstate.posx,
						agentstate.posy);
			}

			// statistics
			statistic.put(i + ":" + "Name", agentstate.name);

			// statistic.put(i.toString(), agentstate.actionFailureProbability +
			// "%,   " + agentstate.lastAction);
		}

		output.setGridStatistic(statistic);
		laststep = step;
		output.save();
	}
}
