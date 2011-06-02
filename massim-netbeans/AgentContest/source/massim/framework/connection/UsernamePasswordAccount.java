package massim.framework.connection;

/**
 * This class represents an account that requires you to send username and password to login.
 * They are checked by comparing them to a given username and password.
 *
 */

public class UsernamePasswordAccount implements Account {
	private String username;
	private String password;
	
	public boolean authenticate(Authentication a) {
		UsernamePasswordAuthentication auth=(UsernamePasswordAuthentication) a;
		return username.equals(auth.username) && password.equals(auth.password); 
	}

	/**
	 * Construct a new UsernamePasswordAccount, using a given username and password.
	 * @param username
	 * @param password
	 */
	public UsernamePasswordAccount(String username, String password) {
		this.password = password;
		this.username = username;
	}
	
	/**
	 * Retrieve username for this account.
	 * @return
	 */
	public String getUsername() {
		return username;
	}
}
