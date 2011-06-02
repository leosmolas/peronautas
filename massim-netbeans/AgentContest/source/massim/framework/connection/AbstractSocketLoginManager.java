package massim.framework.connection;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.log;

import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public abstract class AbstractSocketLoginManager implements SocketHandler {
	private Account[] accounts;
	private Map<Account, SocketHandler> socketHandlerMap;
	
	public AbstractSocketLoginManager(Account[] accounts, Map<Account, SocketHandler> socketHandlerMap) {
		this.accounts = accounts;
		this.socketHandlerMap = socketHandlerMap;
	}
	public AbstractSocketLoginManager() {
		setAccountSocketHandlerMap(new HashMap<Account,SocketHandler>());
	}
	public Map<Account, SocketHandler> getAccountSocketHandlerMap() {
		return socketHandlerMap;
	}
	
	public void setAccountSocketHandlerMap(Map<Account, SocketHandler> v) {
		socketHandlerMap = v;
		Set<Account> s = v.keySet();
		accounts = s.toArray(new Account[s.size()]);
	}
	
	public void handleSocket(Socket s) {
		log(LOGLEVEL_DEBUG);
		Authentication auth = getAuthentication(s);
		int i;
		do
			for (i=0; i<accounts.length; i++) {
				Account iacc = accounts[i];
				if (iacc.authenticate(auth)) {
					SocketHandler handler = socketHandlerMap.get(iacc);
					handleValidAuthentication(s);
					handler.handleSocket(s);
					return;
				}
			}
		while (handleInvalidAuthentication(s));
	}

	/**
	 * This method is called to receive a new Authentification implementing object for a connection.
	 * @param c the connection
	 * @return Authentication associated to connection.
	 */
	abstract protected Authentication getAuthentication(Socket s);
	
	/**
	 * This method is called when an agent did not authenticate.
	 * @param s socket
	 */
	abstract protected boolean handleInvalidAuthentication(Socket s);
	abstract protected void handleValidAuthentication(Socket s);
}
