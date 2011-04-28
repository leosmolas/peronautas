package massim.agent;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.SimpleFormatter;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Description of an agent for the simulation
 * 
 */
public class DemoGridAgent extends AbstractAgent {

	private int gsizeX = 0, gsizeY = 0;
	private int posX = 0, posY = 0;
	private Vector<String> vectorC = null;
	private Vector<String> vectorD = null;

	private int homex1;
	private int homex2;
	private int homey1;
	private int homey2;

	private int lineOfSight = 0;


	/**
	 * Main method of this class
	 * 
	 * @param args
	 *            username, Password and optional Host
	 */
	public static void main(String[] args) {
	
		
		int port =12300;
		String host = "localhost";
		String username = "";
		String pass = "";

		for(int i = 0; i<args.length;i++){
			String arg = args[i];
			if(arg.equalsIgnoreCase("-port")){
				port = Integer.parseInt(args[i+1]);
			}
			
			else if(arg.equalsIgnoreCase("-username")){
				username = args[i+1];
			}
			else if(arg.equalsIgnoreCase("-host")){
				host = args[i+1];
			}
			else if(arg.equalsIgnoreCase("-password"))
				pass = args[i+1];
			else if(arg.equalsIgnoreCase("-help")){
				System.out.println("Usage: DemoGridAgent -username [name] -password [password] [-host] [host] [-port] [port] [-logpath] [path]");
				System.exit(0);				
			}
			else if(arg.equalsIgnoreCase("-logpath")){
					String path = args[i+1];
					try {
						if(!new File(path).exists()){
							new File(path).mkdirs();
						}
						FileHandler fh = new FileHandler(path+System.getProperty("file.separator")+"Agent_"+username+".txt");
						DemoGridAgent.logger.addHandler(fh);
						DemoGridAgent.logger.setLevel(Level.ALL);
						fh.setFormatter(new SimpleFormatter());
					} catch (SecurityException e) {
						
						e.printStackTrace();
					} catch (IOException e) {
						
						e.printStackTrace();
					}
				
			}
		}

		if(username.equalsIgnoreCase("") || pass.equalsIgnoreCase(""))
		{
		System.out.println("Usage: DemoGridAgent -username [name] -password [password] [-host] [host] [-port] [port] [-logpath] [path]");
		System.exit(0);				
		}
		
		DemoGridAgent d = new DemoGridAgent(host,username,pass,port);
		d.start();		
	}
	public DemoGridAgent(){}
public DemoGridAgent(String host,String username, String pass,int port){
	this.setUsername(username);
	this.setPassword(pass);
	this.setHost(host);
	this.setPort(port);
}


	public DemoGridAgent(String host, String username, String pass) {
		this.setUsername(username);
		this.setPassword(pass);
		this.setHost(host);
}
	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.AbstractAgent#processRequestAction(org.w3c.dom.Element,
	 *      org.w3c.dom.Element, long, long)
	 */
	@SuppressWarnings("static-access")
	public void processRequestAction(Element perception, Element target,
			long currenttime, long deadline) {

		// react to normal perception, modify target so that it becomes a valid
		// action
		String action = "";
		
		action = processPerception(perception);

		posX = Integer.parseInt(perception.getAttribute("posx"));
		posY = Integer.parseInt(perception.getAttribute("posy"));
		target.setAttribute("type", action);
	}

	public String processPerception(Element a) {
		int i;
		

		NodeList nl = a.getElementsByTagName("cell");
		Map<String, Integer> dir_point=new HashMap<String, Integer>();
		dir_point.put("west", 0);
		dir_point.put("east", 0);
		dir_point.put("south", 0);
		dir_point.put("north", 0);
		dir_point.put("northeast", 0);
		dir_point.put("southeast", 0);
		dir_point.put("northwest", 0);
		dir_point.put("southwest", 0);
		dir_point.put("skip", 0);
		for (i = 0; i < nl.getLength(); i++) {
			Node n = nl.item(i);
			Element ne = (Element) n;
			NodeList nnl = n.getChildNodes();

			String content = nnl.item(1).getNodeName();
			int directionX = Integer.parseInt(ne.getAttribute("x"));
			int directionY = Integer.parseInt(ne.getAttribute("y"));
			if(content.equalsIgnoreCase("cow")){
				String dir = convertDir(directionX, directionY);
				int p= dir_point.get(dir);
				p= p+1;
				dir_point.put(dir, p);
			}

		}
		String movedir = findMaxPointDir(dir_point);

		return movedir;
	}

	private String findMaxPointDir(Map<String, Integer> dir_point) {
		String d = "skip";
		
		Vector<String> dir = new Vector<String>();
		dir.add("west");dir.add("east"); dir.add("north"); dir.add("south");
		dir.add("northwest"); dir.add("northeast"); dir.add("southwest"); dir.add("southeast");
		
		Collections.shuffle(dir);
		d = dir.get(0);
		
		int point = 0;
		
		if(dir_point.get("west") > point){
			point = dir_point.get("west");
			d = "west";
		}
		if(dir_point.get("east") > point){
			point = dir_point.get("east");
			d = "east";
		}
		if(dir_point.get("north") > point){
			point = dir_point.get("north");
			d = "north";
		}
		if(dir_point.get("south") > point){
			point = dir_point.get("south");
			d = "south";
		}
		if(dir_point.get("northeast") > point){
			point = dir_point.get("northeast");
			d = "northeast";
		}
		if(dir_point.get("southeast") > point){
			point = dir_point.get("southeast");
			d = "southeast";
		}
		if(dir_point.get("northwest") > point){
			point = dir_point.get("northwest");
			d = "northwest";
		}
		if(dir_point.get("southwest") > point){
			point = dir_point.get("southwest");
			d = "southwest";
		}
		
		return d;
	}
	private String convertDir(int directionX, int directionY) {
		String dir="";
		if(directionY > 0)
			dir +="south";
		else if (directionY < 0)
			dir +="north";
		if(directionX > 0)
			dir +="west";
		else if(directionX <0)
			dir +="east";
		return dir;
	}
	/**
	 * Determination of random action
	 * 
	 * @return new action Random action
	 */
	public String randomAction() {

		Random r1 = new Random();
		String action = "skip";

		// action generating by random number
		switch (r1.nextInt(2)) {
		case 0:
			action = "east";
			break;
		case 1:
			action = "west";
			break;
		}

		// return action
		return action;
	}

	public String surround() {
		return "south";
	}

	public String push() {
		return "west";
	}

	/**
	 * Computation of the better direction (shortest way) to the depot
	 * 
	 * @param north
	 *            Distance to the depot (from the adjacent north cell)
	 * @param west
	 *            Distance to the depot (from the adjacent west cell)
	 * @param south
	 *            Distance to the depot (from the adjacent south cell)
	 * @param east
	 *            Distance to the depot (from the adjacent east cell)
	 * @return The better direction
	 */
	public String determineDirection(int north, int west, int south, int east) {

		String direction = "skip";

		// "north"
		if (north <= west && north <= south && north <= east) {

			direction = "north";
		}

		// "west"
		else if (west <= north && west <= south && west <= east) {

			direction = "west";
		}

		// "south"
		else if (south <= north && south <= west && south <= east) {

			direction = "south";
		}

		// "east"
		else if (east <= north && east <= west && east <= south) {

			direction = "east";
		}

		// return action
		return direction;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.AbstractAgent#processLogIn()
	 */
	public void processLogIn() {
		// called as soon as agent logged in successfully
		// TODO: insert code here
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.AbstractAgent#processSimulationEnd(org.w3c.dom.Element, long,
	 *      long)
	 */
	public void processSimulationEnd(Element perception, long currenttime) {
		// react on simulation end
		// TODO: insert code here
	}

	/**
	 * Initialisation of main variables and vectors
	 */
	public void processSimulationStart(Element perception, long currenttime) {

		// react on simulation Start
		gsizeX = Integer.parseInt(perception.getAttribute("gsizex"));
		gsizeY = Integer.parseInt(perception.getAttribute("gsizey"));

		lineOfSight = 8;

		homex1 = Integer.parseInt(perception.getAttribute("corralx0"));
		homex2 = Integer.parseInt(perception.getAttribute("corralx1"));
		homey1 = Integer.parseInt(perception.getAttribute("corraly0"));
		homey2 = Integer.parseInt(perception.getAttribute("corraly1"));


		vectorC = new Vector<String>();
		vectorD = new Vector<String>();
	}
}