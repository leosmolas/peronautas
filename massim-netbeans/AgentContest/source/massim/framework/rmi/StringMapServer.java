package massim.framework.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

public interface StringMapServer extends Remote, java.io.Serializable {
	Map<String, String> get() throws RemoteException;
}
