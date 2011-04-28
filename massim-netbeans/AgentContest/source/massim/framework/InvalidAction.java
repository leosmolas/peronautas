package massim.framework;

/**
 * This class represents an action that is by no means valid. It can be returned by getAction if some obscure error
 * in an agent occured that is to be hidden from Simulation.
 *
 */
public class InvalidAction implements Action {
	private static final long serialVersionUID = 1850318242566049793L;
}
