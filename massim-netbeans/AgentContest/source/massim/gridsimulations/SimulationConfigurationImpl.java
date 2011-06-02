package massim.gridsimulations;
import massim.framework.SimulationConfiguration;
import massim.framework.simulation.DefaultSimpleSimulationConfiguration;
import massim.server.ServerSimulationConfiguration;

/**
 * This class describes the not hand crafted configuration.
 *
 */
public class SimulationConfigurationImpl extends DefaultSimpleSimulationConfiguration implements SimulationConfiguration, ServerSimulationConfiguration {
	
	public int sizex;
	public int sizey;
	public int numberOfAgents;
	public int maxNumberOfSteps;
	public int numberOfObstacles;
	public String teamName0 = "";
	public String teamName1 = "";
	public String tournamentName = "";
	public String simulationName = "";
	//perception information
	public int lineOfSight;
	
	
	/* (non-Javadoc)
	 * @see massim.server.ServerSimulationConfiguration#setTeamName(int, java.lang.String)
	 */
	public void setTeamName(int n, String name) {
		
		// Team mapping
		 if (teamName0 == "") {
			 teamName0 = name;
		 } 
		 
		 else if (teamName1 == "" && teamName0 != name) {
			 teamName1 = name;
		 }
	}

	/* (non-Javadoc)
	 * @see massim.server.ServerSimulationConfiguration#setSimulationName(java.lang.String)
	 */
	public void setSimulationName(String name) {
		simulationName = name;
	}

	/* (non-Javadoc)
	 * @see massim.server.ServerSimulationConfiguration#setTournamentName(java.lang.String)
	 */
	public void setTournamentName(String name) {
		tournamentName = name;
	}
}