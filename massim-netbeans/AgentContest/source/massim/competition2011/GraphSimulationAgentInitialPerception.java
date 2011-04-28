package massim.competition2011;

import java.util.Collection;

import org.w3c.dom.Element;

import massim.framework.InitialStickyPerception;

/**
 * This class holds the information that is sent to the agent as initial perception when simulation begins.
 */
public class GraphSimulationAgentInitialPerception implements
		InitialStickyPerception, massim.framework.util.XMLCodec.XMLEncodable {

	private static final long serialVersionUID = 8293224111557780965L;
	
	/**
	 * The state of the agent to which this perception belongs.
	 */
	public GraphSimulationAgentState self;
	
	/**
	 * The number of steps that this simulation will run.
	 */
	public int steps;
	
	/**
	 * The number of vertices that the map graph has.
	 */
	public int vertices;
	
	/**
	 * The number of edges that the map graph has.
	 */
	public int edges;
	
	/**
	 * A collection of agent states, holding the states of all the team Members in the
	 * same team of the owner of this perception.
	 */
	public Collection<GraphSimulationAgentState> teamMembers;

	/**
	 * Encodes the contents of this perception object in the right XML format,
	 * in concordance to the protocol description,
	 */
	@Override
	public void encodeToXML(Element target) {

		target.setAttribute("steps", String.valueOf(steps));
		target.setAttribute("vertices", String.valueOf(vertices));
		target.setAttribute("edges", String.valueOf(edges));
		
		// TODO maybe add team names (self and rivals). (depending on whether we use internal or provided team names)
		
//		Element elSelf = target.getOwnerDocument()
//				.createElement("self");
//		elSelf.setAttribute("name", self.name);
//		elSelf.setAttribute("team", self.team);
//		elSelf.setAttribute("strength", String.valueOf(self.strength));
//		elSelf.setAttribute("maxEnergy", String.valueOf(self.maxEnergy));
//		elSelf.setAttribute("visRange", String.valueOf(self.visRange));	
//		target.appendChild(elSelf);
//		
//		Element elTeamMembers = target.getOwnerDocument()
//				.createElement("teamMembers");		
//		for (GraphSimulationAgentState agent : teamMembers) {
//			Element elAgent = target.getOwnerDocument()
//					.createElement("teamMember");
//			elAgent.setAttribute("name", agent.name);
//			elAgent.setAttribute("team", agent.team);
//			elAgent.setAttribute("strength", String.valueOf(agent.strength));
//			elAgent.setAttribute("maxEnergy", String.valueOf(agent.maxEnergy));
//			elAgent.setAttribute("visRange", String.valueOf(agent.visRange));				
//			
//			elTeamMembers.appendChild(elAgent);
//		}
//		target.appendChild(elTeamMembers);		
	}

}
