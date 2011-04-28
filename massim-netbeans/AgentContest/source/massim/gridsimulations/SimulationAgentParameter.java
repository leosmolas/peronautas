package massim.gridsimulations;

/**
 * This class represents the AgentParameter.
 *
 */
public class SimulationAgentParameter implements massim.framework.TeamAgentParameter {
	
	public Object team;

	public SimulationAgentParameter() {}
	
	public Object getTeam() {
		
		return team;
	}

	public void setTeam(Object team) {
		
		this.team=team;
	}		
}