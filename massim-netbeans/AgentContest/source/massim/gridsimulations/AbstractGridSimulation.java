package massim.gridsimulations;

import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.log;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import massim.framework.Action;
import massim.framework.Perception;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.SimulationAgent;
import massim.framework.simulation.WorldState;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * 
 * This class is the mainclass of the GridSimulation. It provides methods to
 * handle the initialization, configuration, presimulationstep and
 * postsimulationstep.
 * 
 */

public abstract class AbstractGridSimulation extends
		massim.framework.simulation.ParallelizedRandomOrderSimulation {

	@Override
	public void preSimulationStep() {

	}

	@Override
	public void runAgents() {
		class MyPair {
			public Future<Action> action;
			public SimulationAgent agent;
		}

		SimulationAgent[] agents = getAgents();
		WorldState simstate = (WorldState) getSimpleSimulationState();
		AgentState[] agentstates = new AgentState[agents.length];

		// Assemble agent states to one array
		for (int i = 0; i < agents.length; i++)
			agentstates[i] = agents[i].getAgentState();

		Vector<MyPair> actions = new Vector<MyPair>();

		// Vector<Future<Action>> actions = new Vector<Future<Action>>();
		Perception perceptions[] = new Perception[agents.length];

		// Create perceptions.
		for (int i = 0; i < agents.length; i++) {
			perceptions[i] = agents[i].createPerception(simstate, agentstates);
		}
		// Send perceptions to agents and retrieve future object for actions.
		for (int i = 0; i < agents.length; i++) {
			MyPair m = new MyPair();
			m.action = agents[i].getAgent().concurrentGetAction(perceptions[i]);
			m.agent = agents[i];
			actions.add(m);
			// actions.add(agents[i].getAgent().concurrentGetAction(perceptions[i]));
		}
		// shuffle future
		Collections.shuffle(actions);
		try {
			for (int i = 0; i < agents.length; i++) {
				MyPair m = actions.get(i);
				m.agent.processAction(m.action.get(), simstate, agentstates);
				// agents[i].processAction(actions.get(i).get(), simstate,
				// agentstates);
			}

			// we update the world state after all of agents have charged their
			// positions
			for (int i = 0; i < agents.length; i++) {

				MyPair m = actions.get(i);
				// GridSimulationAgent agent = (GridSimulationAgent)m.agent;
				// update the charges
				SimulationAgentExtend agent = (SimulationAgentExtend) m.agent;
				agent.updateWorldState(simstate, agents);
			}
		} catch (ExecutionException e) {
			log(LOGLEVEL_CRITICAL,
					"Execution of getAction failed, shouldn't happen");
		} catch (InterruptedException e) {
			log(LOGLEVEL_CRITICAL, "Thread interrupted, shouldn't happen");
		}

	}

	@Override
	public void simulationStep() {
	}

	@Override
	public String finalizeSimpleSimulation() {

		SimulationWorldState state = (SimulationWorldState) this
				.getSimpleSimulationState();
		String winner = "";

		if (state.teamScore[0] < state.teamScore[1]) {
			winner = state.teamName[1];
		}

		else if (state.teamScore[0] > state.teamScore[1]) {
			winner = state.teamName[0];
		}

		else {
			winner = "draw";
		}

		return winner;

	}

	protected Document updateAgentPosition(Document doc) {
		Element root = doc.getDocumentElement();
		root.setAttribute("recoverstep", "" + this.getSteps());
		NodeList nl = doc.getElementsByTagName("array");
		SimulationAgent[] simagents = this.getAgents();

		Element agentx = null;
		Element agenty = null;

		for (int i = 0; i < nl.getLength(); i++) {

			Element e = (Element) nl.item(i);

			String meta_name = e.getAttribute("meta:name");

			if (meta_name.equalsIgnoreCase("agentPositionX")) {
				agentx = e;
			} else if (meta_name.equalsIgnoreCase("agentPositionY")) {
				agenty = e;
			}
		}
		NodeList accounts = doc.getElementsByTagName("account");
		Map<String, String> user_pass = new HashMap<String, String>();

		for (int i = 0; i < accounts.getLength(); i++) {
			Element account = (Element) accounts.item(i);
			String user = account.getAttribute("username");
			String pass = account.getAttribute("password");
			user_pass.put(user, pass);
		}

		for (int i = 0; i < simagents.length; i++) {
			SimulationAgent agent = (SimulationAgent) simagents[i];
			SimulationAgentState a_state = (SimulationAgentState) agent
					.getAgentState();
			Element account = (Element) accounts.item(i);

			agentx.setAttribute("item" + i, "" + a_state.posx);
			agenty.setAttribute("item" + i, "" + a_state.posy);

			String pass = user_pass.get(a_state.name);
			account.setAttribute("username", a_state.name);
			account.setAttribute("password", pass);
			account.setAttribute("team", a_state.team);

		}
		return doc;
	}

}
