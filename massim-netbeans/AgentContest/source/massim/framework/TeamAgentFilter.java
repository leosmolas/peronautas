package massim.framework;

import java.util.Map;

import massim.framework.connection.Account;

/**
 * This class allows filtering agents that do not belong to a certain team, based upon a map which agent belongs
 * to which team.
 *
 */

public class TeamAgentFilter extends AbstractAgentFilter {
	private Map<Account,Object> teammap;
	
	/**
	 * Construct a new TeamAgentFilter. 
	 * @param teammap association map to use
	 * @param agentprovider agent provider to receive agents from
	 */
	public TeamAgentFilter(Map<Account,Object> teammap, AgentProvider agentprovider) {
		super(agentprovider);
		this.teammap=teammap;
	}

	@Override
	public boolean isSuitable(UniqueSimulationAgent a, AgentParameter p) {
		TeamAgentParameter tp;
		try {
			tp=(TeamAgentParameter)p;
		}catch(ClassCastException e){
			return true;
			}//so we don't care about teams, right?
		
		return tp.getTeam().equals(teammap.get(a.getIdentifier()));
	}
}
