package massim.framework.connection;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.log;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class InetSocketListener extends AbstractSocketListener {
	private ServerSocket serverSocket;
	public InetSocketListener(int port, int backlog, InetAddress inetaddr) throws IOException {
		log(LOGLEVEL_NORMAL, "InetSocketListener created. Set to port "+port+" with backlog "+backlog);
		serverSocket=new ServerSocket(port, backlog, inetaddr);
	}
	
	@Override
	void stopListening() {
		try{serverSocket.close();}
		catch(IOException e) {log(LOGLEVEL_ERROR, e.toString());}
	}

	@Override
	Socket waitForIncomingSocket() throws StopListeningException {
		log(LOGLEVEL_DEBUG);
		while (true) {
			try {
				log(LOGLEVEL_DEBUG,"waiting for connection...");
				Socket s=serverSocket.accept();
				log(LOGLEVEL_DEBUG,"got a connection");
				return s;
			} catch (IOException e) {
				if (! serverSocket.isClosed()) log(LOGLEVEL_ERROR, e.toString());
				log(LOGLEVEL_DEBUG,"stop listening");
				throw new StopListeningException();
			}
		}
	}

}
