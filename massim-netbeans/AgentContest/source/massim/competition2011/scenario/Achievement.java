package massim.competition2011.scenario;

import java.io.Serializable;

/**
 * This class holds the configuration of an achievement, as well as its achieved status.
 */
public class Achievement implements Serializable {

	private static final long serialVersionUID = -2761429573471745751L;
	
	public String name;
	public String achievementClass;
	public int points;
	public int quantity;
	public boolean achieved; 
	
	public Achievement() {
		this.name = null;
		this.achievementClass = null;
		this.points = 0;
		this.quantity = 0;
		this.achieved = false;
	}
	
	public Achievement(Achievement achievement) {
		this.name = achievement.name;
		this.achievementClass = achievement.achievementClass;
		this.points = achievement.points;
		this.quantity = achievement.quantity;
		this.achieved = achievement.achieved;
	}
	
}
