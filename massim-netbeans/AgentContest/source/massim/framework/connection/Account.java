package massim.framework.connection;

/**
 * This interface is supposed to be implemented by any class that will be used as account by AbstractLoginManager.
 *
 */
public interface Account {
	/**
	 * The contract of this method is that it is supposed to check if the Authentication object auth is sufficient to
	 * log into the account represented by the implementing class. 
	 * @param auth authentification object to be checked.
	 * @return true if and only if auth is ok to log into this account
	 */
	boolean authenticate(Authentication auth);
}
