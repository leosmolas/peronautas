package massim.framework;

public interface Observer extends Component {
	void notifySimulationStart();
	void notifySimulationEnd();
	void notifySimulationState(SimulationState state);
	void notifySimulationConfiguration(SimulationConfiguration simconf);
/*	
	void notifySimulationAgentCreated(long agendidentifier, );
	void notifySimulationAgentRemoved();
	void notifySimulationAgentAct(Object perception, Object action);
	 */
}
