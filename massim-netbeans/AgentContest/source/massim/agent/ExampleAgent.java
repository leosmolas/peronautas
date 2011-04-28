package massim.agent;

import org.w3c.dom.Element;

public class ExampleAgent extends AbstractAgent {

	// main: should create new instance and configure it.
	public static void main(String[] args) {
		ExampleAgent agent = new ExampleAgent();
		// configure network
		agent.setPort(12300);
		agent.setHost("localhost");

		// configure credentials
		agent.setUsername(args[0]);
		agent.setPassword(args[1]);

		// launch agent
		agent.start();
	}

	// some notes on usage:
	public void processLogIn() {
		// called as soon as agent logged in successfully
		// TODO: insert code here
	}

	public void processSimulationEnd(Element perception, long currenttime) {
		// react on simulation end
		// TODO: insert code here
	}

	public void processSimulationStart(Element perception, long currenttime) {
		// react on simulation start
		// TODO: insert code here
	}

	public void processRequestAction(Element perception, Element target,
			long currenttime, long deadline) {
		// react to normal perception, modify target so that it becomes a valid
		// action
		// TODO: insert code here
	}

}
