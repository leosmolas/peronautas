package massim.framework;

import java.util.Vector;

/**
 * This class provides a simple way to have multiple Observers appearing as
 * one Observer. It will just pass all notifications to all target observers.
 *
 */
public class BroadcastObserver extends DefaultObserver {
	private Vector<Observer> observerlist;
	public BroadcastObserver() {
		observerlist = new Vector<Observer>();
	}

	public void addObserver(Observer o) {
		observerlist.add(o);
	}
	
	public void removeObserver(Observer o) {
		observerlist.remove(o);
	}
	
	@Override
	public void notifySimulationStart() {
		for (int i=0;i<observerlist.size();i++) observerlist.get(i).notifySimulationStart();
	}

	@Override
	public void notifySimulationEnd() {
		for (int i=0;i<observerlist.size();i++) observerlist.get(i).notifySimulationEnd();
	}

	@Override
	public void notifySimulationState(SimulationState state) {
		for (int i=0;i<observerlist.size();i++) observerlist.get(i).notifySimulationState(state);
	}

	@Override
	public void notifySimulationConfiguration(SimulationConfiguration simconf) {
		for (int i=0;i<observerlist.size();i++) observerlist.get(i).notifySimulationConfiguration(simconf);
	}
}
