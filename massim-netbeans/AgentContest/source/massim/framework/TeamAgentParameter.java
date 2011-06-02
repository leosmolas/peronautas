package massim.framework;

/**
 * This interface should be implemented by all AgentParamter objects that intend to create an agent of one certain team.
 * It offers a mechanism to set and retrieve an identifier for the team the agent should belong to. 
 *
 */

public interface TeamAgentParameter extends AgentParameter {
	/**
	 * Returns a team identifier. This identifier will be used to create an appropriate agent. Note that Object.equals
	 * might be used while doing that to compare it to some other identifier. Returning null means that the team doesn't
	 * matter.
	 * @return team identifier.
	 */
	Object getTeam();
	/**
	 * Sets the team identifier. 
	 * @param team
	 */
	void setTeam(Object team);
}
