package massim.gridsimulations;


import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import massim.framework.DefaultObserver;
import massim.framework.SimulationConfiguration;
import massim.framework.SimulationState;
import massim.framework.rmi.RMI_Infor;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.SimulationStateImpl;
import massim.server.ServerSimulationContext;
import massim.server.ServerSimulationContextReceiver;
import massim.visualization.GridPolicy;

/**
 * This VisualizationObserver takes care about the visualization (svg-files).
 * 
 */
public abstract class SimulationVisualizationObserver extends DefaultObserver implements ServerSimulationContextReceiver {
	
	public static String simulationName;
	public String tournamentName;
	protected GridPolicy output;
	protected String outputFolder;
	protected int htaccess = 1;
	protected int laststep = -1;
	
	public SimulationVisualizationObserver() {		
		output = new GridPolicy();
//		output.setConfigPath(Server.configurationFilenamePath + System.getProperty("file.separator") + "visualization" + System.getProperty("file.separator"));
	}
	
	public void notifySimulationStart() {
		
		laststep=-1;
	}

	public void notifySimulationConfiguration(SimulationConfiguration simconf) {
		
		SimulationConfigurationImpl simconfig = (SimulationConfigurationImpl) simconf;
		
		// create svg-output-file.
		// create Subfolder
		Date dt = new Date();
		//output.create();
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HH-mm");
		outputFolder = simulationName + "_" + df.format(dt);
		output.createFolder(outputFolder);
		
		if(!RMI_Infor.FLASH_SERVER_ACTIVATED) {
			SimulationXMLStatisticsObserver.simulationName=outputFolder;
			SimulationRMIXMLDocumentObserver.simulationName=outputFolder;
		}
		//FIXME
		//drawSimulation(state);
		//FIXME value of htaccess is not read from the config file 
		if (htaccess == 1) {
			// create .htaccess
			try {
				output.createFile(".htaccess",
						"AuthType Basic\nAuthName \"Restricted Files\" \nAuthUserFile "
								
								+ "/home/massim/htpasswd/.htusers \nRequire user "
								+ simconfig.teamName0 + " " + simconfig.teamName1 + " " + "massim");
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public void notifySimulationEnd() {
		
		output.createPreviewSvg();
	}

	public void notifySimulationState(SimulationState state) {
		
		SimulationStateImpl tcstate = (SimulationStateImpl) state;
		if(laststep==-1) {
			drawBackground((SimulationWorldState)tcstate.simulationState);
		}
		
		if (laststep!=tcstate.steps) {
			drawSimulation(tcstate.steps,  (SimulationWorldState)tcstate.simulationState, (AgentState[]) tcstate.agentStates);
		}
	}

	public void start() {
		// TODO Auto-generated method stub

	}

	public void stop() {
		// TODO Auto-generated method stub

	}
	
	public void setSimulationContext(ServerSimulationContext context) {
		
		simulationName = context.getSimulationName();
		tournamentName = context.getGlobalName();
	}
		
	protected abstract void drawSimulation(int step, SimulationWorldState state, AgentState[] agentstates);
	protected abstract void drawBackground(SimulationWorldState state);
		
}
