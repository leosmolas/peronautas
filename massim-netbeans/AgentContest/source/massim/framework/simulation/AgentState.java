package massim.framework.simulation;

import java.io.Serializable;

/**
 * Classes that implement this interface represent the state of an agent with respect to the simulation.
 * The agent state can include agent specific information that belongs to SimulationState. This might include
 * things like agent position in a world, agent score, agent speed or something similar.
 *
 */
public interface AgentState extends Serializable {
}
