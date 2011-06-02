package massim.competition2011.scenario;

import java.io.Serializable;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import massim.competition2011.GraphSimulationAgentState;


/**
 * This class holds information about the current state of a Team, including current score, zones built,
 * achievement points, achievements, etc.
 * 
 */
public class TeamState implements Serializable{

	private static final long serialVersionUID = -1800159344742627560L;
	
	public static final int ACHIEVEMENT_PONITS_SCALE = 1;
	
	public String name;
	public int teamIdx;
	public int ranking;
	public long summedScore;
	public long currAchievementPoints;
	public long usedAchievementPoints;
	public List<DominatedArea> areas;
	private int successfulAttacks;
	private int successfulParrys;
	private Set<GraphEdge> surveyedEdges;
	private Set<String> probedNodes;
	private Set<String> inspectedAgents;
	public Vector<Achievement> achievements;
	public Vector<Achievement> newAchievements;
	
	public TeamState(String name, int teamIdx) {
		super();
		this.name = name;
		this.teamIdx = teamIdx;
		this.ranking = -1;
		this.summedScore = 0;
		this.currAchievementPoints = 0;
		this.usedAchievementPoints = 0;
		this.successfulAttacks = 0;
		this.successfulParrys = 0;
		this.areas = new LinkedList<DominatedArea>();
		
		this.surveyedEdges = new HashSet<GraphEdge>();
		this.probedNodes = new HashSet<String>();
		this.inspectedAgents = new HashSet<String>();
		this.achievements = new Vector<Achievement>();
		this.newAchievements = new Vector<Achievement>();
	}
	
	/**
	 * Sums the step score to the total score. This method should be called
	 * once at the end of every simulation step.
	 */
	public void sumCurrent() { 
		summedScore += getCurrent();
	}
	
	/**
	 * Returns the current step-score (that is, the score that depends only on the current status of the world,
	 * and that should be added to the total team score in every step).
	 * @return
	 */
	public long getCurrent() { 
		return currAchievementPoints + getAreasValue();
	}
	
	/**
	 * Returns the summed score of all the dominated areas.
	 * @return
	 */
	public long getAreasValue() { 
		long score = 0;
		for (DominatedArea area: areas){
			score += getAreaValue(area);
		}
		return score;
	}
	
	/**
	 * Returns the value of the area given as parameter.
	 * @param area
	 * @return
	 */
	public long getAreaValue(DominatedArea area){
		// return area.totalScore;
		return area.calculateProbedScore(probedNodes);
	}
	
	
	/**
	 * Returns the value of the dominated area of which the agent given as parameter is part.
	 * (returns 0 if the agent is not dominating area)
	 * @param agent
	 * @return
	 */
	public long getAreaValue(GraphSimulationAgentState agent){
		for (DominatedArea area: areas){
			if (area.agents.contains(agent)){
				return getAreaValue(area);
			}
		}		
		return 0;
	}
	
	
	
	public boolean useAchievementPoints(long points){
		if (points >= currAchievementPoints) return false;
		currAchievementPoints -= points;
		usedAchievementPoints += points;
		return true;
	}
	
	
	/**
	 * Checks if the teams has reached new achievements, marks them as reached,
	 * and increases the current achievementPoints accordingly.
	 */
	public void calculateNewAchievements(){
		this.newAchievements.clear();
		for (Achievement ach : this.achievements) {
			if (!ach.achieved){
				if ("probedVertices".equals(ach.achievementClass)){
					if (probedNodes.size() >= ach.quantity){
						ach.achieved = true;
						this.currAchievementPoints += ach.points;
						newAchievements.add(ach);
					}
				}
				else if ("surveyedEdges".equals(ach.achievementClass)){
					if (surveyedEdges.size() >= ach.quantity){
						ach.achieved = true;
						this.currAchievementPoints += ach.points;
						newAchievements.add(ach);
					}
				}
				else if ("inspectedAgents".equals(ach.achievementClass)){
					if (inspectedAgents.size() >= ach.quantity){
						ach.achieved = true;
						this.currAchievementPoints += ach.points;
						newAchievements.add(ach);
					}
				}
				else if ("successfulAttacks".equals(ach.achievementClass)){
					if (successfulAttacks >= ach.quantity){
						ach.achieved = true;
						this.currAchievementPoints += ach.points;
						newAchievements.add(ach);
					}
				}
				else if ("successfulParries".equals(ach.achievementClass)){
					if (successfulParrys >= ach.quantity){
						ach.achieved = true;
						this.currAchievementPoints += ach.points;
						newAchievements.add(ach);
					}
				}
				else if ("areaValue".equals(ach.achievementClass)){
					for (DominatedArea area : areas) {
						if (area.calculateProbedScore(probedNodes)>= ach.quantity){
							ach.achieved = true;
							this.currAchievementPoints += ach.points;
							newAchievements.add(ach);
							break;
						}
					}
				}				
			}
		}	
	}
	
	/**
	 * Returns a vector with the name of the achievements that the team obtained
	 * during the <b>whole simulation</b>.
	 * @return a <code>Vector&lt;String&gt;</code> containing the names.
	 */
	public Vector<String> getAchieved(){
		Vector<String> v = new Vector<String>();
		for (Achievement ach : this.achievements) {
			if (ach.achieved){
				v.add(ach.name);
			}
		}
		return v;
	}
	
	/**
	 * Returns a vector with the name of the achievements that the team obtained
	 * during the <b>last simulation step</b>.
	 * @return a <code>Vector&lt;String&gt;</code> containing the names.
	 */
	public Vector<String> getNewlyAchieved(){
		Vector<String> v = new Vector<String>();
		for (Achievement ach : this.newAchievements) {
			v.add(ach.name);
		}
		return v;
	}
	
	public void succsefullAttack(){
		successfulAttacks++;
	}
	
	public void succsefullParry(){
		successfulParrys++;
	}
	
	public boolean addSurveyedEdge(GraphEdge e){
		return surveyedEdges.add(e);
	}
	
	public boolean addProbedNodes(String nodeName){
		return probedNodes.add(nodeName);
	}
	
	public boolean addInspectedAgent(String agentName){
		return inspectedAgents.add(agentName);
	}
	
	public boolean addProbedNodes(GraphNode node){
		return probedNodes.add(node.name);
	}
	
	public Set<String> getProbedNodes(){
		return probedNodes;
	}
	
	public boolean addInspectedAgent(GraphSimulationAgentState agent){
		return inspectedAgents.add(agent.name);
	}
	
	public void initAchievements(Vector<Achievement> configAchievements){
		
		for (Achievement achievement : configAchievements) {
			Achievement copy = new Achievement(achievement);
			this.achievements.add(copy);
		}
		
	}
}
