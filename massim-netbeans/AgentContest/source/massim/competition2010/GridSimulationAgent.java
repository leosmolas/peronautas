package massim.competition2010;



import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.log;

import java.util.Random;

import massim.cowsimulations.GridSimulationAgentInitialPerception;
import massim.cowsimulations.GridSimulationPerceptionCell;
import massim.framework.Action;
import massim.framework.AgentParameter;
import massim.framework.FinalPerception;
import massim.framework.InitialStickyPerception;
import massim.framework.Perception;
import massim.framework.UniqueSimulationAgent;
import massim.framework.connection.UsernamePasswordAccount;
import massim.framework.simulation.AgentState;
import massim.framework.simulation.SimulationAgent;
import massim.framework.simulation.WorldState;
import massim.gridsimulations.SimulationAgentExtend;
import massim.cowsimulations.GridSimulationCell;

/**
 * This class deals with the agent state, his actions and perceptions.
 *
 */
public class GridSimulationAgent extends SimulationAgentExtend {
	
	private GridSimulationAgentState agentstate = null;
	private GridSimulationAgentAction action = null;
	private GridSimulationWorldState simulationstate = null;
	
	private String originalAction = "none";
	
	private String up = "up"; private String north="north";
	private String down= "down"; private String south="south";
	private String left ="left"; private String west = "west";
	private String right= "right";private String east="east";
	private String upleft = "upleft";	private String northwest ="northwest";
	private String downright="downright";private String southeast = "southeast";
	private String downleft  ="downleft";private String southwest ="southwest";
	
	private String upright ="upright"; private String northeast="northeast";

	
	/**
	 * The constructor instantiates the agentstate and the action
	 */
	public GridSimulationAgent() {
		
		agentstate = new GridSimulationAgentState();
		action = new GridSimulationAgentAction();
	}
	
	
	/**
	 * Set the GridSimulationWorldState
	 * @param simstate 
	 */
	public void setGrid(WorldState simstate){
		
		simulationstate = (GridSimulationWorldState) simstate;
	}
	
	
	/**
	 * Set the new action for this agent
	 * @param action Action
	 */
	public void setAction(Action newAction){
		
		if (newAction instanceof massim.framework.InvalidAction) {
			
			//set invalid action
			this.action.type = "invalid";
			this.originalAction = action.type;
			this.agentstate.currentAction = action.type;
			//this.agentstate.nextAction = action.type;
		} 
		
		else if (newAction instanceof GridSimulationAgentAction) {
			
			//set action
			this.action = (GridSimulationAgentAction) newAction;
			this.originalAction = action.type;
			this.agentstate.currentAction = action.type;
		}
	}
	
	
	/**
	 * Set the new action for this agent
	 * @param action Action as string
	 */
	public void setAction(String action){
		this.action.type= action;
		this.agentstate.currentAction = action;
	}
	
	
	/**
	 * Get the current action of this agent
	 * @return action GridSimulationAgentAction
	 */
	public GridSimulationAgentAction getAction(){
		
		return action;
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
			AgentState[] as) {
		int i,j;
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		GridSimulationAgentPerceptionAverage p = new GridSimulationAgentPerceptionAverage();
		p.posx = agentstate.posx;
		p.posy = agentstate.posy;
		p.step = simulationstate.currentStep;
		
		Random r1 = new Random();
		
		int k = 0;
		
		if (!agentstate.team.equalsIgnoreCase(simulationstate.teamName[k])) {
			k = 1;
		}
		p.cowsInCorral = simulationstate.cowsInCorral[k];
			
		int new_sight= simulationstate.lineOfSight*2 +1;
		p.perception = new GridSimulationPerceptionCell[new_sight][new_sight];
		// information distortion and local view creation
		
		
		k = new_sight/2;
		//TODO: i= -lineofsight, to fit into the rest of the sim (like cow behavior)
		for (i = 0; i < new_sight ; i++)
		{
			for (j = 0; j< new_sight ; j++)
			{
				if (p.posx+i-k >= 0 && p.posx+i-k < simulationstate.sizex && p.posy+j-k >= 0 && p.posy+j-k < simulationstate.sizey) 
				{
					p.perception[i][j] = convertCell(simulationstate.board[p.posx+i-k][p.posy+j-k], simulationstate);
					if (Math.abs(r1.nextInt()) < simulationstate.fogprobability)
					{
						p.perception[i][j].unknown = true;
					}
					else p.perception[i][j].unknown = false;
				}

			}
		}
		return  p;
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.simulation.simplesimulation.SimulationAgent#processAction(massim.Action,
	 *      massim.simulation.simplesimulation.WorldState,
	 *      massim.simulation.simplesimulation.AgentState[])
	 */
	//int oldPosx, oldPosy;
	public void processAction(Action a, WorldState simstate,
			AgentState[] agentstates) {
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		agentstate.oldPosx = agentstate.posx;
		agentstate.oldPosy = agentstate.posy;
		agentstate.actionFailed = false;
		// information distortion
		
		GridSimulationAgentAction received = new GridSimulationAgentAction();
		if (a instanceof massim.framework.InvalidAction) {
			// set lastAction
			agentstate.lastAction = "invalid";
			log(LOGLEVEL_NORMAL, "InvalidAction:" +agentstate.name);
		} else if (a instanceof GridSimulationAgentAction) {
			received = (GridSimulationAgentAction) a;
			// set lastAction
			agentstate.lastAction = received.type;
		}
	
		//calculate the success probability of action
		this.filterFatique(received, simstate);

		if(this.isForbiddenStep(agentstate.posx, agentstate.posy,received.type,simulationstate)){
			received.type = "skip";
		}
		
		if ((received.type.equalsIgnoreCase(up) || received.type.equalsIgnoreCase(north))
				&& !collisionCheck(agentstate.posx, agentstate.posy - 1,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board,simulationstate)) {
			agentstate.posy -= 1;
		} 
		else if ((received.type.equalsIgnoreCase(down) || received.type.equalsIgnoreCase(south))
				&& !collisionCheck(agentstate.posx, agentstate.posy + 1,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board, simulationstate)) {
			agentstate.posy += 1;
		}
		else if ((received.type.equalsIgnoreCase(left) || received.type.equalsIgnoreCase(west))
				&& !collisionCheck(agentstate.posx - 1, agentstate.posy,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board,simulationstate)) {
			agentstate.posx -= 1;
		}
		else if ((received.type.equalsIgnoreCase(right) || received.type.equalsIgnoreCase(east))
				&& !collisionCheck(agentstate.posx + 1, agentstate.posy,
						simulationstate.sizex, simulationstate.sizey,
						simulationstate.board,simulationstate)) {
			agentstate.posx += 1;
		}
		else if((received.type.equalsIgnoreCase(upright) || received.type.equalsIgnoreCase(northeast))
				&& !collisionCheck(agentstate.posx+1, agentstate.posy-1, simulationstate.sizex, simulationstate.sizey, simulationstate.board,simulationstate))
		{
			agentstate.posx +=1;
			agentstate.posy -=1;
		}
		else if((received.type.equalsIgnoreCase(upleft) || received.type.equalsIgnoreCase(northwest))
				&& !collisionCheck(agentstate.posx-1, agentstate.posy-1, 
						simulationstate.sizex, simulationstate.sizey, simulationstate.board,simulationstate))
		{
			agentstate.posx -=1;
			agentstate.posy -=1;
		}
		else if((received.type.equalsIgnoreCase(downright) || received.type.equalsIgnoreCase(southeast))
				&& !collisionCheck(agentstate.posx+1, agentstate.posy+1,
						simulationstate.sizex, simulationstate.sizey, simulationstate.board,simulationstate))
		{
			agentstate.posx +=1;
			agentstate.posy +=1;
		}
		else if((received.type.equalsIgnoreCase(downleft) || received.type.equalsIgnoreCase(southwest))
				&& !collisionCheck(agentstate.posx-1, agentstate.posy+1, 
						simulationstate.sizex, simulationstate.sizey, simulationstate.board,simulationstate))
		{
			agentstate.posx -=1;
			agentstate.posy +=1;
		}
		
		
		else if (received.type == "skip" ){ 
			// do nothing, just added because of clearness
			// System.out.println("DO nothing");
		}
		//We don not update the WorldState now because the later agents can see the updated world state.
		//so that we update the world state after all agents have charged their positions
		
	}
	private boolean isForbiddenStep(Integer posx, Integer posy, String step_direction,GridSimulationWorldState simulationstate) {
		//if there is a switch on the left of agent
		if((posx-1)>=0 && simulationstate.board[posx-1][posy].switcher){
			//if direction of fence is up
			String fenceDir = this.fenceDirection(posx-1,posy,simulationstate);
			if(fenceDir.equalsIgnoreCase(up)
					
			&& (step_direction.equalsIgnoreCase(upleft) || step_direction.equalsIgnoreCase(northwest)))
			{
				return true;
			}
			if(fenceDir.equalsIgnoreCase(down)
					&& (step_direction.equalsIgnoreCase(downleft) || step_direction.equalsIgnoreCase(southwest)))
					{
						return true;
					}
		}
		
		//if there is a switch on the right of agent
		if((posx+1) < simulationstate.sizex && simulationstate.board[posx+1][posy].switcher){
			//if direction of fence is up and agent want to move upright
			String fenceDir = this.fenceDirection(posx+1,posy,simulationstate);
			if(fenceDir.equalsIgnoreCase(up)
					
			&& (step_direction.equalsIgnoreCase(upright) || step_direction.equalsIgnoreCase(northeast)))
			{
				return true;
			}
			//if direction of fence is down and agent want to move downright
			if(fenceDir.equalsIgnoreCase(down)
					&& (step_direction.equalsIgnoreCase(downright) || step_direction.equalsIgnoreCase(southeast)))
					{
						return true;
					}
		}
		
		//if there is a switch on the top of agent
		if((posy-1)>=0 && simulationstate.board[posx][posy-1].switcher){
			//if direction of fence is left and agent want to move upleft
			String fenceDir = this.fenceDirection(posx,posy-1,simulationstate);
			if(fenceDir.equalsIgnoreCase(left)
					
			&& (step_direction.equalsIgnoreCase(upleft) || step_direction.equalsIgnoreCase(northwest)))
			{
				return true;
			}
			//if direction of fence is right and agent want to move upright
			if(fenceDir.equalsIgnoreCase(right)
					&& (step_direction.equalsIgnoreCase(upright) || step_direction.equalsIgnoreCase(northeast)))
					{
						return true;
					}
		}
		//if there is a switch on the bottom of agent
		if((posy+1) < simulationstate.sizey && simulationstate.board[posx][posy+1].switcher){
			//if direction of fence is left and agent want to move upleft
			String fenceDir = this.fenceDirection(posx,posy+1,simulationstate);
			if(fenceDir.equalsIgnoreCase(left)
					
			&& (step_direction.equalsIgnoreCase(downleft) || step_direction.equalsIgnoreCase(southwest)))
			{
				return true;
			}
			//if direction of fence is right and agent want to move downright
			if(fenceDir.equalsIgnoreCase(right)
					&& (step_direction.equalsIgnoreCase(downright) || step_direction.equalsIgnoreCase(southeast)))
					{
						return true;
					}
		}
		
		return false;
	}


	private String fenceDirection(Integer posx, Integer posy,
			GridSimulationWorldState simulationstate2) {
		String dir = "";
		for(int i = 0 ; i< simulationstate2.numberOfFences; i++){
			if(simulationstate2.switchX[i]== posx && simulationstate2.switchY[i]== posy){
				dir = simulationstate2.fenceDirection[i];
			}
		}
		return dir;
	}

	/**
	 * take charge of agent's positions in world state.
	 * in the case, two or more agents want to move in the same cell, only one of them can move in
	 * the others will be set back to their old positions.
	 * @param simstate is the WorldState
	 */
	public void updateWorldState(WorldState simstate, SimulationAgent[] agents){
		// update worldstate
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		//in the case, two or more agents want to move in the same cell, only one of them can move in
		//the other will be set back to their old positions.
		if(simulationstate.board[agentstate.posx][agentstate.posy].freeCell()){

			simulationstate.board[agentstate.oldPosx][agentstate.oldPosy].agent = false;
			simulationstate.board[agentstate.oldPosx][agentstate.oldPosy].agentTeam = null;
			simulationstate.board[agentstate.posx][agentstate.posy].agent = true;
			simulationstate.board[agentstate.posx][agentstate.posy].agentTeam = agentstate.team;
			
			String direction =  getAgentDirection(agentstate.posx, agentstate.posy, agentstate.oldPosx,agentstate.oldPosy);
			if(!direction.equalsIgnoreCase("")){
				agentstate.direction = direction;
			}
		}
		//set agent back to his old position because there is already agent moved in the cell
		else {
			agentstate.posx = agentstate.oldPosx;
			agentstate.posy = agentstate.oldPosy;
		}
	}

/**
 * return one of the following directions: left, leftup, leftdown, right, 
 * rightup, rightdown and "";
 *  
 * @param posx actual x position of agent 
 * @param posy actual y position of agent
 * @param oldPosx old x position of agent
 * @param oldPosy old y position of agent
 * @return
 */
private String getAgentDirection(Integer posx, Integer posy,
			Integer oldPosx, Integer oldPosy) {
		String direction = "";
		
		if(posy < oldPosy){
			direction = direction+up;
		}
		else if(posy > oldPosy){
			direction = direction+down;
		}
		
		if(posx < oldPosx){
			direction = direction+left;	
		}
		else if(posx > oldPosx){
			direction = direction+right;
			
		}
		return direction;
		
	}


//the probability that agent's action fails is 10%
//FIXME see GridSimulationWorldState and fog probability. misused variables and hard-wired values.
	  private void filterFatique(GridSimulationAgentAction received,WorldState simstate){
		
		  Random r = new Random();
			int random = Math.abs(r.nextInt()) % 100;
			
			//skip because of probability
			if(random < 5){
				
				received.type = "skip";
				agentstate.actionFailed = true;
			}
		}
	  
	/**
	 * This method checks whether the agent collides with an other object.
	 * 
	 * @param x
	 *            GridPosition x
	 * @param y
	 *            GridPosition y
	 * @param sizex
	 *            GridSize x
	 * @param sizey
	 *            GridSize y
	 * @param board
	 *            Grid
	 * @return
	 */
	private boolean collisionCheck(int x, int y, int sizex, int sizey,
			GridSimulationCell[][] board, GridSimulationWorldState state) {
	
		if(x >= 0 && x < sizex && y >=0 
				&& y < sizey && board[x][y].freeCell())
			
			return false;
		else 
			return true;
	}
	
	/**
	 * Converts a GridCell to a PerceptionCell
	 * @param OldCell The GridCell
	 * @return The new PerceptionCell
	 */
	private GridSimulationPerceptionCell convertCell(GridSimulationCell oldCell, GridSimulationWorldState state) {
		
		GridSimulationPerceptionCell newCell = new GridSimulationPerceptionCell();

		if (oldCell.agent) {
			
			if (agentstate.team.equalsIgnoreCase(oldCell.agentTeam)) {
				newCell.agentType = "ally";
			} 
			
			else {
				newCell.agentType = "enemy";
			}			
		}
		
		newCell.agent = oldCell.agent;
		newCell.cow = oldCell.cow;
		newCell.obstacle = oldCell.obstacle;
		newCell.cowID = oldCell.cowID;
		newCell.corral = oldCell.isStable1() || oldCell.isStable2();
		newCell.switcher = oldCell.switcher;
		newCell.fence = oldCell.fence;
		newCell.open = oldCell.open;
		
		if (agentstate.team.equalsIgnoreCase(state.teamName[0]) && oldCell.isStable1() || agentstate.team.equalsIgnoreCase(state.teamName[1]) && oldCell.isStable2())
		{
			newCell.stableType = "ally";
		}
		if (agentstate.team.equalsIgnoreCase(state.teamName[1]) && oldCell.isStable1() || agentstate.team.equalsIgnoreCase(state.teamName[0]) && oldCell.isStable2())	
		{
			newCell.stableType = "enemy";
		}
		return newCell;
	}
	
	
	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#createInitialPerception(massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public InitialStickyPerception createInitialPerception(WorldState simstate,
			AgentState[] as) {
		
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		GridSimulationAgentInitialPerception p = new GridSimulationAgentInitialPerception();
		
		p.steps = simulationstate.maxNumberOfSteps;
		p.gsizex = simulationstate.sizex;
		p.gsizey = simulationstate.sizey;
		
		
		if (!agentstate.team.equalsIgnoreCase(simulationstate.teamName[0])) {
			p.corralx0 = simulationstate.stable2X[0];
			p.corraly0 = simulationstate.stable2Y[0];
			p.corralx1 = simulationstate.stable2X[1];
			p.corraly1 = simulationstate.stable2Y[1];
		}
		else
		{
			p.corralx0 = simulationstate.stable1X[0];
			p.corraly0 = simulationstate.stable1Y[0];
			p.corralx1 = simulationstate.stable1X[1];
			p.corraly1 = simulationstate.stable1Y[1];
		}
		p.lineOfSight = simulationstate.lineOfSight;
		
		
		
		
		if (agentstate.team.equalsIgnoreCase(simulationstate.teamName[0])) {
			p.opponent = simulationstate.teamName[1];
			//p.team = 0;
		} else {
			p.opponent = simulationstate.teamName[0];
			//p.team = 1;
		}
		return p;
	}

	
	/* (non-Javadoc)
	 * @see massim.simulation.simplesimulation.SimulationAgent#createFinalPerception(massim.simulation.simplesimulation.WorldState, massim.simulation.simplesimulation.AgentState[])
	 */
	public FinalPerception createFinalPerception(WorldState simstate,
			AgentState[] as) {
		
		GridSimulationAgentFinalPerceptionAverageScore p = new GridSimulationAgentFinalPerceptionAverageScore();
		GridSimulationWorldState simulationstate = (GridSimulationWorldState) simstate;
		
		int k = 0;
		
		if (!agentstate.team.equalsIgnoreCase(simulationstate.teamName[k])) {
			k = 1;
		}
		p.averageScore = simulationstate.averageScore[k];
		
		if (simulationstate.averageScore[0] == simulationstate.averageScore[1]) {
			p.result = "draw";
		} 
		
		else if (simulationstate.averageScore[k] < simulationstate.averageScore[(k + 1) % 2]) {
			p.result = "loose";
		} 
		
		else if (simulationstate.averageScore[k] > simulationstate.averageScore[(k + 1) % 2]) {
			p.result = "win";
		}
		
		return p;
	}
	
	
	/**
	 * This method computes for each agent his probabilty of action failure 
	 * @param fap failureActionProbability
	 * @param mfap maxFailureActionProbability
	 * @param mng maxNumberOfGoldItems
	 * @param ng numberOfGoldItems (current)
	 * @return value of probability
	 */
	public int computeProbability(int fap, int mfap, int mng, int ng){
		
		return (fap + ((mfap - fap) / mng) * ng);
	}
}

