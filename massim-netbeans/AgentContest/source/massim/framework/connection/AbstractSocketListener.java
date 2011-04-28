package massim.framework.connection;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.log;

import java.net.Socket;

import massim.framework.Component;

public abstract class AbstractSocketListener implements Component {
	protected static class StopListeningException extends Exception {
		private static final long serialVersionUID = -4787588718053398042L;}

	abstract void stopListening();
	abstract Socket waitForIncomingSocket() throws StopListeningException;
	
	private Thread listenthread;
	private SocketHandler socketHandler;
	
	public AbstractSocketListener() {
		listenthread = new Thread(){
			public void run() {
				try {
					while (true) {
						final Socket s = waitForIncomingSocket();
						Thread t = new Thread() {
							public void run() {
								if (socketHandler!=null) {
									socketHandler.handleSocket(s);
								}
							}
						};
						t.start();
					}
				} catch (StopListeningException e) {
					log(LOGLEVEL_DEBUG, "caught StopListeningException");
				}
			}
		};
	}
	
	
	
	public void start() {
		listenthread.start();
	}

	public void stop() {
		stopListening();
	}
	
	public SocketHandler getSocketHandler() {
		return socketHandler;
	}
	public void setSocketHandler(SocketHandler socketHandler) {
		this.socketHandler = socketHandler;
	}
}
