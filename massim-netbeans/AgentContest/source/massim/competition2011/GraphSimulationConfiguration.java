package massim.competition2011;


import java.io.Serializable;
import java.util.HashMap;
import java.util.Vector;

import massim.competition2011.scenario.Achievement;
import massim.competition2011.scenario.ActionConfiguration;
import massim.competition2011.scenario.RoleConfiguration;
import massim.framework.simulation.DefaultSimpleSimulationConfiguration;
import massim.framework.util.XMLCodec;
import massim.server.ServerSimulationConfiguration;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * This class holds the simulation configuration specified in the XML config file.
 */
public class GraphSimulationConfiguration extends DefaultSimpleSimulationConfiguration implements Serializable, ServerSimulationConfiguration, XMLCodec.XMLDecodable{
	
	private static final long serialVersionUID = 5802657031982257279L;	
	
	/**
	 * Tournament's name.
	 */
	public String tournamentName = "";
	
	/**
	 * Simulation's name.
	 */
	public String simulationName = "";
	
	private Vector<String> teamNames;
	
	/**
	 * The max number of steps that this simulation should run if not finalized or interrupted before.
	 */
	public int maxNumberOfSteps;
	
	/**
	 * The number of agents taking part in the simulation.
	 */
	public int numberOfAgents;
	
	/**
	 * The number of teams taking part in the simulation.
	 */
	public int numberOfTeams;
	
	/**
	 * The number of agents taking part in each team.
	 */
	public int agentsPerTeam;
	
	// Graph generation
	/**
	 * The number of nodes for the graph to generate.
	 */
	public int numberOfNodes;
	
	/**
	 * The width of the abstract grid in which the graph nodes will be randomly placed when generating the graph.
	 */
	public int gridWidth;
	
	/**
	 * The height of the abstract grid in which the graph nodes will be randomly placed when generating the graph.
	 */
	public int gridHeight;
	
	/**
	 * The width of each grid cell in the abstract grid in which the graph nodes will be randomly placed
	 * when generating the graph. Used for visualization.
	 */
	public int cellWidth;
	
	/**
	 * Minimum possible value for the random assignment of weights to nodes during map generation.
	 */
	public int minNodeWeight;
	
	/**
	 * Maximum possible value for the random assignment of weights to nodes during map generation.
	 */
	public int maxNodeWeight;
	
	/**
	 * Minimum possible value for the random assignment of costs to edges during map generation.
	 */
	public int minEdgeCost;
	
	/**
	 * Maximum possible value for the random assignment of costs to edges during map generation.
	 */
	public int maxEdgeCost;
	
	/**
	 * A map from action names to their configurations.
	 */
	public HashMap<String, ActionConfiguration> actionsConfMap;
	
	/**
	 * A map from role names to their configurations.
	 */
	public HashMap<String, RoleConfiguration> rolesConfMap;  
	
	/**
	 * A vector holding the configurations of every possible achievement.
	 */
	public Vector<Achievement> achievements;
	
	

	/* (non-Javadoc)
	 * @see massim.server.ServerSimulationConfiguration#setSimulationName(java.lang.String)
	 */
	public void setSimulationName(String name) {
		simulationName = name;
	}

	/* (non-Javadoc)
	 * @see massim.server.ServerSimulationConfiguration#setTournamentName(java.lang.String)
	 */
	public void setTournamentName(String name) {
		tournamentName = name;
	}

	@Override
	public void setTeamName(int n, String name) {
		if (teamNames == null){
			teamNames = new Vector<String>();
		}
		if (n>= teamNames.size()){
			teamNames.setSize(n+1);
		}
		teamNames.set(n,name);
	}
	
	public Vector<String> getTeamNames() {
		return teamNames;
	}
	
	/**
	 * Returns the configuration object of the action whose name is given as a parameter.
	 * @param name The name of the action.
	 * @return an <code>ActionConfiguration</code> object.
	 */
	public ActionConfiguration getActionConf(String name){
		return actionsConfMap.get(name);
	}
	
	/**
	 * Returns the configuration object of the role whose name is given as a parameter.
	 * @param name The name of the role.
	 * @return a <code>RoleConfiguration</code> object.
	 */
	public RoleConfiguration getRoleConf(String name){
		return rolesConfMap.get(name);
	}

	/**
	 * Populates this object from the contents of an XML subtree with its root in <code>source</code> (taken
	 * from the configuration file).
	 * @param source
	 */
	@Override
 	public void decodeFromXML(Element source) {
		maxNumberOfSteps = Integer.decode(source.getAttribute("maxNumberOfSteps"));		
		numberOfAgents  =  Integer.decode(source.getAttribute("numberOfAgents")); 
		numberOfTeams  =   Integer.decode(source.getAttribute("numberOfTeams")); 
		agentsPerTeam  =   Integer.decode(source.getAttribute("agentsPerTeam"));
		numberOfNodes  =   Integer.decode(source.getAttribute("numberOfNodes"));
		gridWidth  =       Integer.decode(source.getAttribute("gridWidth"));
		gridHeight  =      Integer.decode(source.getAttribute("gridHeight"));
		cellWidth  =       Integer.decode(source.getAttribute("cellWidth"));
		minNodeWeight  =   Integer.decode(source.getAttribute("minNodeWeight"));
		maxNodeWeight  =   Integer.decode(source.getAttribute("maxNodeWeight"));
		minEdgeCost  =     Integer.decode(source.getAttribute("minEdgeCost"));
		maxEdgeCost  =     Integer.decode(source.getAttribute("maxEdgeCost"));
		
		actionsConfMap = new HashMap<String, ActionConfiguration>();
		rolesConfMap = new HashMap<String, RoleConfiguration>();
		
//		probedVerticesAchievements = new Vector<Achievement>();
//		surveyedEdgesAchievements = new Vector<Achievement>();
//		inspectedAgentsAchievements = new Vector<Achievement>();
//		successfulAttacksAchievements = new Vector<Achievement>();
//		successfulParriesAchievements = new Vector<Achievement>();
//		areaValueAchievements = new Vector<Achievement>();
		achievements = new Vector<Achievement>();
		
		
		NodeList nl = source.getChildNodes();
		for (int i=0; i < nl.getLength(); i++) {
			Node n = nl.item(i);
			if ("actions".equals(n.getNodeName())){
				NodeList actionNodes = n.getChildNodes();
				
				for (int j=0; j < actionNodes.getLength(); j++) {
					Node actionNode = actionNodes.item(j);
					if ("action".equals(actionNode.getNodeName())){
						Element actionElement = (Element)actionNode;
						ActionConfiguration ac = new ActionConfiguration();
						String name = actionElement.getAttribute("name");
						ac.name = name;
						ac.energyCost =               Integer.decode(actionElement.getAttribute("energyCost"));
						ac.energyCostFailed =         Integer.decode(actionElement.getAttribute("energyCostFailed"));
						ac.energyCostDisabled =       Integer.decode(actionElement.getAttribute("energyCostDisabled"));
						ac.energyCostFailedDisabled = Integer.decode(actionElement.getAttribute("energyCostFailedDisabled"));
						ac.healthCost =               Integer.decode(actionElement.getAttribute("healthCostFailed"));
						ac.healthCostFailed =         Integer.decode(actionElement.getAttribute("healthCostFailed"));
						ac.healthCostDisabled =       Integer.decode(actionElement.getAttribute("healthCostDisabled"));
						ac.healthCostFailedDisabled = Integer.decode(actionElement.getAttribute("healthCostFailedDisabled"));
						ac.pointsCost =               Integer.decode(actionElement.getAttribute("pointsCost"));
						ac.pointsCostFailed =         Integer.decode(actionElement.getAttribute("pointsCostFailed"));
						ac.pointsCostDisabled =       Integer.decode(actionElement.getAttribute("pointsCostDisabled"));
						ac.healthCostFailedDisabled = Integer.decode(actionElement.getAttribute("healthCostFailedDisabled"));
						
						// TODO add action-specific configs?
//						if ("buy".equals(name)){
//							// ac.items = Integer.decode(actionElement.getAttribute("items"));
//						} else if ("goto".equals(name)){
//							//
//						}
						actionsConfMap.put(ac.name, ac);						
					}
				}
			} else if ("roles".equals(n.getNodeName())){
				NodeList roleNodes = n.getChildNodes();
				
				for (int j=0; j < roleNodes.getLength(); j++) {
					Node roleNode = roleNodes.item(j);
					if ("role".equals(roleNode.getNodeName())){
						Element roleElement = (Element)roleNode;
						RoleConfiguration rc = new RoleConfiguration();
						rc.name =                  roleElement.getAttribute("name");
						rc.maxEnergy =             Integer.decode(roleElement.getAttribute("maxEnergy"));
						rc.maxBuyEnergy =          Integer.decode(roleElement.getAttribute("maxBuyEnergy"));
						rc.rateBuyEnergy =         Integer.decode(roleElement.getAttribute("rateBuyEnergy"));
						rc.maxEnergyDisabled =     Integer.decode(roleElement.getAttribute("maxEnergyDisabled"));
						rc.rateBuyEnergyDisabled = Integer.decode(roleElement.getAttribute("rateBuyEnergyDisabled"));
						rc.maxHealth =             Integer.decode(roleElement.getAttribute("maxHealth"));
						rc.maxBuyHealth =          Integer.decode(roleElement.getAttribute("maxBuyHealth"));
						rc.rateBuyHealth =         Integer.decode(roleElement.getAttribute("rateBuyHealth"));
						rc.strength =              Integer.decode(roleElement.getAttribute("strength"));
						rc.maxBuyStrength =        Integer.decode(roleElement.getAttribute("maxBuyStrength"));
						rc.rateBuyStrength =       Integer.decode(roleElement.getAttribute("rateBuyStrength"));
						rc.visRange =              Integer.decode(roleElement.getAttribute("visRange"));
						rc.maxBuyVisRange =        Integer.decode(roleElement.getAttribute("maxBuyVisRange"));
						rc.rateBuyVisRange =       Integer.decode(roleElement.getAttribute("rateBuyVisRange"));
						
						NodeList nl2 = roleElement.getChildNodes();
						for (int k=0; k < nl2.getLength(); k++) {
							Node classNode = nl2.item(k);
							if ("actions".equals(classNode.getNodeName())){
								NodeList actionNodes = classNode.getChildNodes();								
								for (int l=0; l < actionNodes.getLength(); l++) {
									Node actionNode = actionNodes.item(l);
									if ("action".equals(actionNode.getNodeName())){
										Element actionElement = (Element)actionNode;
										rc.actions.add(actionElement.getAttribute("name"));
									}
								}
							} else if ("actionsDisable".equals(classNode.getNodeName())){
								NodeList actionNodes = classNode.getChildNodes();								
								for (int l=0; l < actionNodes.getLength(); l++) {
									Node actionNode = actionNodes.item(l);
									if ("action".equals(actionNode.getNodeName())){
										Element actionElement = (Element)actionNode;
										rc.actionsDisable.add(actionElement.getAttribute("name"));
									}
								}
							}						
						}
						
						rolesConfMap.put(rc.name, rc);						
					}
				}
			} else if ("achievements".equals(n.getNodeName())){
				NodeList achievementNodes = n.getChildNodes();
				
				for (int j=0; j < achievementNodes.getLength(); j++) {
					Node achievementNode = achievementNodes.item(j);
					if ("achievement".equals(achievementNode.getNodeName())){
						Element achievementElement = (Element)achievementNode;
						Achievement ac = new Achievement();
						ac.name = achievementElement.getAttribute("name");
						ac.achievementClass = achievementElement.getAttribute("class");
						ac.points =   Integer.decode(achievementElement.getAttribute("points"));
						ac.quantity = Integer.decode(achievementElement.getAttribute("quantity"));
						achievements.add(ac);
					}
				}
			}
		}
		
	}
	
	
}
