package massim.framework.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import massim.framework.SimulationState;

public interface SimulationStateServer extends Remote, java.io.Serializable {
	/**
	 * Retrieve the last simulation state (if there is any simulation running), else null.
	 * @return
	 * @throws RemoteException
	 */
	SimulationState getSimulationState() throws RemoteException;
}
