package massim.framework.connection;

/**
 * This class represents a username/password-style authentication.
 *
 */

public class UsernamePasswordAuthentication implements Authentication {
	/** username component for this authentication */
	public String username;
	
	/** password component for this authentication */
	public String password;

	/**
	 * Constructs a new UsernamePasswordAuthentication object.
	 */
	public UsernamePasswordAuthentication() {
		username="";
		password="";
	}
}
