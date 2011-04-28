package massim.framework.rmi;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import massim.framework.DefaultObserver;
import massim.framework.SimulationState;

/**
 * This class will act as an Observer, which means it receives information about a simulation. It will as long as 
 * the simulation is running provide the last transmitted simulation state through RMI.
 *
 */
public class SimulationStateServerObserver extends DefaultObserver {
	private static class SimulationStateServerImplementation extends UnicastRemoteObject implements SimulationStateServer {
		private static final long serialVersionUID = -1254854915029577664L;
		public SimulationState laststate;
		public SimulationState getSimulationState() throws RemoteException {
			synchronized(this) {
				return laststate;
			}
		}
		public SimulationStateServerImplementation() throws RemoteException {
			super();
		}
	}
	
	private SimulationStateServerImplementation implementation;
	private String servicename;
	
	public SimulationStateServerObserver(String servicename) {
		this.servicename=servicename;
		try {
		implementation = new SimulationStateServerImplementation();
		} catch (RemoteException r) {}
		implementation.laststate=null;
	}
	
	@Override
	public void notifySimulationEnd() {
//		implementation.laststate=null;
	}

	@Override
	public void notifySimulationStart() {
		implementation.laststate=null;
	}

	@Override
	public void notifySimulationState(SimulationState state) {
		System.out.println("set simstate to"+implementation.laststate);
		synchronized(implementation) {
			implementation.laststate = state;
		}
	}
	
	@Override
	public void start() {
		
		try {
			log(LOGLEVEL_DEBUG,"binding...");
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.rebind(servicename,implementation);
			log(LOGLEVEL_DEBUG,"bound");
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"error while binding (RemoteException)");
		}
	
	}

	@Override
	public void stop() {
		try {
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.unbind(servicename);
			UnicastRemoteObject.unexportObject(implementation, true);
			implementation = null;
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"error while binding (RemoteException)");
		} catch (NotBoundException e) {
			log(LOGLEVEL_ERROR,"error while binding (NotBound)");
		}
	}
}
