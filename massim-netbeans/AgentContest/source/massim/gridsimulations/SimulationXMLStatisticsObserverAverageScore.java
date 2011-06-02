package massim.gridsimulations;

import massim.competition2010.GridSimulationWorldState;
import massim.framework.SimulationState;
import massim.framework.XMLOutputObserver;
import massim.framework.backup.BackupReader;
import massim.framework.backup.BackupWriter;
import massim.framework.rmi.RMI_Infor;
import massim.framework.simulation.SimulationStateImpl;
import massim.server.Server;
import massim.visualization.MainPolicy;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * * This XMLObserver provides the simulation statistics for the results page of the web server.
 * 
 */
public class SimulationXMLStatisticsObserverAverageScore extends XMLOutputObserver{
	
	private Element el_root;
	private Document doc;
	
	// private int[] agentscores;
	private String[] teamName;
//	FIXME: outputFolder shouldn't be used at all!!!!
    public static String outputFolder="/home/massim/www/webapps/massim/output/";
//	private int[] teamScore;
    Integer[] cowsInCorral = new Integer[2];
    Double[] averageScore = new Double[2];
	public static String simulationName;
	public  String tournamentName ;
//	FIXME: visualconfig shouldn't be used at all!!!!!!
	private String configPath = Server.configurationFilenamePath + System.getProperty("file.separator") + "visualization" + System.getProperty("file.separator");
	private String configFile = configPath + "visualconfig.xml";

	/**
	 * This constructor appends the statistic-element to the document.
	 */
	public SimulationXMLStatisticsObserverAverageScore() {
		super();
		doc = getDocument();
		el_root = doc.createElement("statistics");
		Document doc = BackupReader.openFile(configFile);
		Element el = (Element) doc.getElementsByTagName("simulationOutput").item(0);
		outputFolder = el.getAttribute("path");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationStart()
	 */
	public void notifySimulationStart() {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationEnd()
	 */
	public void notifySimulationEnd() {
		
		synchronized (this) {
			resetDocument();
			doc = getDocument();
			el_root = doc.createElement("statistics");
			// the teamName attribute contains the result of the simulation that is transferred via RMI to the web server (results page).
			// If you change something here you have to change the Server.java AND the web server! So, DON'T CHANGE ANYTHING YOU KNOW WHAT YOU ARE DOING!
			el_root.setAttribute(teamName[0], Double.toString(averageScore[0]));
			el_root.setAttribute(teamName[1], Double.toString(averageScore[1]));
			el_root.setAttribute(teamName[0] + "cowsincorral", Integer.toString(cowsInCorral[0]));
			el_root.setAttribute(teamName[1] + "cowsincorral", Integer.toString(cowsInCorral[1]));
			el_root.setAttribute("simulation", simulationName);
		
			String outputFile;
			outputFile = MainPolicy.previewFile;
			el_root.setAttribute("output",outputFile);
			
			doc.appendChild(el_root);
		}

		setChanged();
		notifyObservers();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Observer#notifySimulationState(massim.SimulationState)
	 */
	public void notifySimulationState(SimulationState state) {
		
		SimulationStateImpl simplestate = (SimulationStateImpl) state;
		GridSimulationWorldState worldState = (GridSimulationWorldState) simplestate.simulationState;
		
		teamName = new String[2];
		teamName[0] = worldState.teamName[0];
		teamName[1] = worldState.teamName[1];
		averageScore[0] = worldState.averageScore[0];
		averageScore[1] = worldState.averageScore[1];
		cowsInCorral[0] = worldState.cowsInCorral[0];
		cowsInCorral[1] = worldState.cowsInCorral[1];
		
		simulationName  = worldState.simulationName;
		String outputFile;
		String backgroundFile = null;
		if(RMI_Infor.FLASH_SERVER_ACTIVATED){
			
			outputFile = outputFolder+simulationName+BackupWriter.file_sep+"masSim-"+simplestate.steps+".swf";
		}
		else{
			outputFile = outputFolder+simulationName+BackupWriter.file_sep+"masSim-"+simplestate.steps+".svg";
			backgroundFile= outputFolder+simulationName+BackupWriter.file_sep+"masSim-0.svg";
		}
		
		synchronized (this) {
			resetDocument();
			doc = getDocument();
			el_root = doc.createElement("statistics");
			el_root.setAttribute(teamName[0] + "averagescore", Double.toString(averageScore[0]));
			el_root.setAttribute(teamName[1] + "averagescore", Double.toString(averageScore[1]));
			el_root.setAttribute(teamName[0] + "cowsincorral", Double.toString(cowsInCorral[0]));
			el_root.setAttribute(teamName[1] + "cowsincorral", Double.toString(cowsInCorral[1]));
			el_root.setAttribute("simulation", simulationName);
			el_root.setAttribute("output",outputFile);
			el_root.setAttribute("output2",backgroundFile);
			doc.appendChild(el_root);
		}

		setChanged();
		notifyObservers();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Component#start()
	 */
	public void start() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.Component#stop()
	 */
	public void stop() {
	}
}
