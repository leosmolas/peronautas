package massim.competition2006;import java.util.Random;import massim.framework.Action;import massim.framework.AgentParameter;import massim.framework.FinalPerception;import massim.framework.InitialStickyPerception;import massim.framework.InvalidAction;import massim.framework.Perception;import massim.framework.UniqueSimulationAgent;import massim.framework.connection.UsernamePasswordAccount;import massim.framework.simulation.AgentState;import massim.framework.simulation.SimulationAgent;import massim.framework.simulation.WorldState;import massim.goldsimulations.GridSimulationAgentInitialPerception;import massim.goldsimulations.GridSimulationCell;import massim.goldsimulations.GridSimulationPerceptionCell;import massim.gridsimulations.GridSimulationAgentFinalPerception;import massim.gridsimulations.SimulationAgentExtend;
/**
 * This class deals with the agent state, his actions and perceptions.
 *
 */
public class GridSimulationAgent extends SimulationAgentExtend {
	private GridSimulationAgentState agentstate;

	/**
	 * The constructor instantiates the agentstate.
	 */
	public GridSimulationAgent() {
		agentstate = new GridSimulationAgentState();
	}

	
	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#getAgentState()
	 */
	public AgentState getAgentState() {
		return agentstate;
	}

	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#setAgentParameter(massim.AgentParameter)
	 */
	public void setAgentParameter(AgentParameter agentpar) {
		super.setAgentParameter(agentpar);
		// get Team
		GridSimulationAgentParameter gridSimAgentParameter = (GridSimulationAgentParameter) agentpar;
		agentstate.team = gridSimAgentParameter.getTeam().toString();
		// get Username
		if (this.getAgent() instanceof UniqueSimulationAgent) {
			UniqueSimulationAgent agent = (UniqueSimulationAgent) this
					.getAgent();
			if (agent.getIdentifier() instanceof UsernamePasswordAccount) {
				UsernamePasswordAccount upa = (UsernamePasswordAccount) agent
						.getIdentifier();
				agentstate.name = upa.getUsername();
			}
		}
	}

	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#createPerception(massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public Perception createPerception(WorldState simstate,
			AgentState[] agentstates) {
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		GridSimulationAgentPerception p = new GridSimulationAgentPerception();
		p.posx = agentstate.posx;
		p.posy = agentstate.posy;
		p.step = simulationstate.currentStep;
		Random r1 = new Random();
		// information distortion and local view creation
		p.cur = convertCell(simulationstate.board[p.posx][p.posy]);
		if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
			p.cur.unknown = true;
		}
		if (p.posy > 0) {
			p.n = convertCell(simulationstate.board[p.posx][p.posy - 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.n.unknown = true;
			}
		}
		if (p.posy < simulationstate.sizey - 1) {
			p.s = convertCell(simulationstate.board[p.posx][p.posy + 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.s.unknown = true;
			}
		}
		if (p.posx > 0) {
			p.w = convertCell(simulationstate.board[p.posx - 1][p.posy]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.w.unknown = true;
			}
		}
		if (p.posx < simulationstate.sizex - 1) {
			p.e = convertCell(simulationstate.board[p.posx + 1][p.posy]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.e.unknown = true;
			}
		}
		if (p.posx > 0 && p.posy > 0) {
			p.nw = convertCell(simulationstate.board[p.posx - 1][p.posy - 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.nw.unknown = true;
			}
		}
		if (p.posx > 0 && p.posy < simulationstate.sizey - 1) {
			p.sw = convertCell(simulationstate.board[p.posx - 1][p.posy + 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.sw.unknown = true;
			}
		}
		if (p.posx < simulationstate.sizex - 1 && p.posy > 0) {
			p.ne = convertCell(simulationstate.board[p.posx + 1][p.posy - 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.ne.unknown = true;
			}
		}
		if (p.posx < simulationstate.sizex - 1
				&& p.posy < simulationstate.sizey - 1) {
			p.se = convertCell(simulationstate.board[p.posx + 1][p.posy + 1]);
			if ((Math.abs(r1.nextInt()) % 100) < simulationstate.informationDistortionProbability) {
				p.se.unknown = true;
			}
		}
		return p;
	}

	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#processAction(massim.Action, massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public void processAction(Action a, WorldState simstate,
			AgentState[] agentstates) {
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		agentstate.oldPosx = agentstate.posx;
		agentstate.oldPosy = agentstate.posy;
		agentstate.actionFailed = false;
		// information distortion
		Random r1 = new Random();
		GridSimulationAgentAction received = new GridSimulationAgentAction();
		if (a instanceof InvalidAction) {
		// set lastAction
			agentstate.lastAction = "invalid";
		} else if (a instanceof GridSimulationAgentAction) {
			received = (GridSimulationAgentAction) a;
		// set lastAction
			agentstate.lastAction = received.type;
		}
		// actionSuccessProbability
		if (simulationstate.actionSuccessProbability < (Math.abs(r1.nextInt()) % 101)) {
			agentstate.actionFailed = true;
			received.type = "skip";
		}
		
		// Perform action
		if (received.type.equalsIgnoreCase("drop")
				&& agentstate.agentHoldsGold
				&& !simulationstate.board[agentstate.posx][agentstate.posy].gold) {
			agentstate.agentHoldsGold = false;
			if (agentstate.agentInDepot) {
				agentstate.score += 1;
				if (agentstate.team == simulationstate.teamName[0]) {
					simulationstate.teamScore[0] += 1;
				} else {
					simulationstate.teamScore[1] += 1;
				}
			} else {
				simulationstate.numberOfGoldItems += 1;
				simulationstate.board[agentstate.posx][agentstate.posy].gold = true;
			}
		} else if (received.type.equalsIgnoreCase("pick")
				&& !agentstate.agentInDepot && !agentstate.agentHoldsGold
				&& simulationstate.board[agentstate.posx][agentstate.posy].gold) {
			agentstate.agentHoldsGold = true;
			simulationstate.board[agentstate.posx][agentstate.posy].gold = false;
			simulationstate.numberOfGoldItems -= 1;
		} else if (received.type.equalsIgnoreCase("up")
				&& collisionCheck(agentstate.posx, agentstate.posy - 1,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board)) {
			agentstate.posy -= 1;
		} else if (received.type.equalsIgnoreCase("down")
				&& collisionCheck(agentstate.posx, agentstate.posy + 1,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board)) {
			agentstate.posy += 1;
		} else if (received.type.equalsIgnoreCase("left")
				&& collisionCheck(agentstate.posx - 1, agentstate.posy,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board)) {
			agentstate.posx -= 1;
		} else if (received.type.equalsIgnoreCase("right")
				&& collisionCheck(agentstate.posx + 1, agentstate.posy,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board)) {
			agentstate.posx += 1;
		} else if (received.type.equalsIgnoreCase("unmark")
				&& !agentstate.agentInDepot) {
			simulationstate.board[agentstate.posx][agentstate.posy].mark = false;
		} else if (received.type.equalsIgnoreCase("mark")
				&& !agentstate.agentInDepot) {
			simulationstate.board[agentstate.posx][agentstate.posy].markText = received.param.substring(0, received.param.length() % 4);
			simulationstate.board[agentstate.posx][agentstate.posy].mark = true;
		}
		// If the agent does not leave the depot in the first possible
		// opportunity, or won't drop the gold --> teleporting
		else if (agentstate.agentInDepot) {
			boolean teleported = false;
			Random r = new Random();
			while (!teleported) {
				int x = Math.abs(r.nextInt()) % (simulationstate.sizex - 1);
				int y = Math.abs(r.nextInt()) % (simulationstate.sizey - 1);
				if ((!simulationstate.board[x][y].agent
						&& !simulationstate.board[x][y].depot && !simulationstate.board[x][y].obstacle)) {
					agentstate.posx = x;
					agentstate.posy = y;
					teleported = true;
					agentstate.agentInDepot = false;
				}
			}
		} else if (received.type == "skip" && !agentstate.agentInDepot) {
			// do nothing, just added because of clearness
			// System.out.println("DO nothing");
		}
		
	}

	/**
	 * This method checks wether the agent collides with an other object.
	 * @param x GridPosition x
	 * @param y GridPosition y
	 * @param sizex GridSize x
	 * @param sizey GridSize y
	 * @param board Grid
	 * @return
	 */
	private boolean collisionCheck(int x, int y, int sizex, int sizey,
			GridSimulationCell[][] board) {
		if (x < 0 || x >= sizex || y < 0 || y >= sizey) {
			return false;
		} else if (board[x][y].obstacle) {
			return false;
		} else if (board[x][y].agent) {
			return false;
		} else if (board[x][y].depot && !agentstate.agentHoldsGold) {
			return false;
		} else if (board[x][y].depot && agentstate.agentHoldsGold) {
			agentstate.agentInDepot = true;
			return true;
		}
		agentstate.agentInDepot = false;
		return true;
	}

	/**
	 * Converts a GridCell to a PerceptionCell
	 * @param oldCell The GridCell
	 * @return The new PerceptionCell
	 */
	private GridSimulationPerceptionCell convertCell(GridSimulationCell oldCell) {
		GridSimulationPerceptionCell newCell = new GridSimulationPerceptionCell();		if (oldCell.agent) {
			if (agentstate.team.equalsIgnoreCase(oldCell.agentTeam)) {
				newCell.agentType = "ally";			} else {				newCell.agentType = "enemy";			}					}
		newCell.agent = oldCell.agent;
		newCell.depot = oldCell.depot;
		newCell.gold = oldCell.gold;
		newCell.mark = oldCell.mark;
		if (oldCell.mark) {
			newCell.markText = oldCell.markText;
		}
		newCell.obstacle = oldCell.obstacle;
		return newCell;
	}

	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#createInitialPerception(massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public InitialStickyPerception createInitialPerception(WorldState simstate,
			AgentState[] agentstates) {
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		GridSimulationAgentInitialPerception p = new GridSimulationAgentInitialPerception();
		p.steps = simulationstate.numberOfSteps;
		p.gsizex = simulationstate.sizex;
		p.gsizey = simulationstate.sizey;
		p.depotx = simulationstate.depotx;
		p.depoty = simulationstate.depoty;
		if (agentstate.team.equalsIgnoreCase(simulationstate.teamName[0])) {
			p.opponent = simulationstate.teamName[1];
		} else {
			p.opponent = simulationstate.teamName[0];
		}
		return p;
	}

	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#createFinalPerception(massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public FinalPerception createFinalPerception(WorldState simstate,
			AgentState[] agentstates) {
		GridSimulationAgentFinalPerception p = new GridSimulationAgentFinalPerception();
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		int k = 0;
		if (!agentstate.team.equalsIgnoreCase(simulationstate.teamName[k])) {
			k = 1;
		}
		p.score = simulationstate.teamScore[k];
		if (simulationstate.teamScore[0] == simulationstate.teamScore[1]) {
			p.result = "draw";
		} else if (simulationstate.teamScore[k] < simulationstate.teamScore[(k + 1) % 2]) {
			p.result = "loose";
		} else if (simulationstate.teamScore[k] > simulationstate.teamScore[(k + 1) % 2]) {
			p.result = "win";
		}
		return p;
	}	@Override	public void updateWorldState(WorldState simstate, SimulationAgent[] agents) {		// TODO Auto-generated method stub		// set NewWorldState		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;		simulationstate.board[agentstate.oldPosx][agentstate.oldPosy].agent = false;		simulationstate.board[agentstate.oldPosx][agentstate.oldPosy].agentTeam = null;		simulationstate.board[agentstate.posx][agentstate.posy].agent = true;		simulationstate.board[agentstate.posx][agentstate.posy].agentTeam = agentstate.team;			}
}
