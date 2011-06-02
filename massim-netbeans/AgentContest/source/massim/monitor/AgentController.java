package massim.monitor;

import java.awt.Graphics2D;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferStrategy;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import massim.agent.DemoGridAgent;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


public class AgentController extends CowMonitor implements KeyListener {

	
	int numberControlledAgent = 1;
	Vector<DemoControlledAgent> agent_manager;
	int controlledAgent;
	boolean enterAction = false;

	DemoControlledAgent agent;
	String command ="";
	private int index = 0;
	private String username; 
	 String[][] account;
	
	public AgentController(String configPath){

		this.getInfor(configPath);
		this.addKeyListener(this);
		
		 agent_manager = new Vector<DemoControlledAgent>();
		 
		 try {
			 System.out.println("There are "+this.numberOfAgents +" Agents");
			 System.out.println("INPUT for number of controlled agents: ");
			numberControlledAgent = Integer.parseInt(new BufferedReader(new InputStreamReader(System.in)).readLine());
			
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		 
		 //create controlled agents
		 if(numberControlledAgent<=this.numberOfAgents){
			
			 for(int i = 0; i< numberControlledAgent; i++){
				 String c= account[i][0];
				 DemoControlledAgent agents = new DemoControlledAgent("localhost",this.account[i][0],this.account[i][1]);
				 agents.start();
				 agent_manager.add(agents);
				 
				 try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				 
			 }
		 }
		 //create automatics agents
		
		 for(int i = numberControlledAgent ; i< this.numberOfAgents; i++){
			 
			 new DemoGridAgent("localhost", this.account[i][0], this.account[i][1],12300).start();
			 try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		 }
		 
			this.searchService();
			this.updateGraphics();
			
			this.runAgents();
	
	}
	
boolean conf =false;
private void updateGraphics() {
		
		Document xmlDoc = 	getRMIObject(this.rmihost, this.rmiport,
				this.service);
		
		if(conf==false) {this.configure(xmlDoc); conf = true;}
		
		if (xmlDoc != null) {
			this.updateOutput(xmlDoc);
			// double buffering
			BufferStrategy strategy = null;
			Graphics2D g2d = null;
			strategy = this.getBufferStrategy();
			g2d = (Graphics2D) strategy.getDrawGraphics();

			// paint
			paint(g2d);

			// done
			g2d.dispose();
			
			// display
			strategy.show();
		}
	
	}


private void getInfor(String configPath) {
	File file = new File(configPath);
	DocumentBuilder documentbuilder = null;
	DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
	dbfactory.setNamespaceAware(true);
	try {
		documentbuilder = dbfactory.newDocumentBuilder();
	} catch (ParserConfigurationException e) {
		System.err.println(e);
	}
	Document doc = null;
	try {
		doc = documentbuilder.parse(file);
	} catch (Exception e) {
		System.err.println(e);
	}
	Element root = doc.getDocumentElement();

	if (doc == null) {
		System.err
				.println("Error parsing configuration: Missing root element");
	}
	
	Element e = (Element) root.getElementsByTagName("simulation").item(0);
	rmihost = e.getAttribute("rmixmlobsserverhost");
	rmiport = Integer.parseInt(e.getAttribute("rmixmlobsserverport"));
	
	NodeList nl = root.getElementsByTagName("configuration");
	
	e = (Element) nl.item(0);

	int x = Integer.parseInt(e.getAttribute("sizex"));
	int y = Integer.parseInt(e.getAttribute("sizey"));
	this.setOutputSize(x, y);
	this.numberOfAgents = root.getElementsByTagName("account").getLength();
	account = new String[this.numberOfAgents][2];
	NodeList node = root.getElementsByTagName("account");
	for (int i = 0; i < node.getLength(); i++) {
		Element el = (Element) node.item(i);
		account[i][0] = el.getAttribute("username");
		account[i][1] = el.getAttribute("password");
	}
	}

Thread blink ;
public void runAgents(){

	if(agent_manager.size()!=0){
		agent= agent_manager.get(index);
		username = agent.getUsername();
		paintControlledAgent(username);
		System.out.println("Enter your Action for Agent: "+ username);
	}
}
	public static void main(String[] args) {
		if(args.length==0){
			System.out.println("AgentMonitor [path zu config file]");
			System.exit(0);
		}
		new AgentController(args[0]);
		//new CowMonitor("config/serverconfig1.xml");
	}


	public void keyPressed(KeyEvent e) {
		String action = "skip";
		//10 = Enter 32 = leertaste

		command = command+e.getKeyChar();
		if(agent!= null){
			if(command.equalsIgnoreCase("w")){
	        	 action = "north";
	         }
	         if(command.equalsIgnoreCase("x")){
	        	 action = "south";
	         }
	         
	         if(command.equalsIgnoreCase("a")){
	        	 action = "west";
	         }
	         if(command.equalsIgnoreCase("d")){
	        	 action = "east";
	         }

	         if(command.equalsIgnoreCase("q") ){
	        	 action = "northwest";
	         }
	         if(command.equalsIgnoreCase("e") ){
	        	 action = "northeast";
	         }
	         if(command.equalsIgnoreCase("c") ){
	        	 action = "southeast";
	         }
	         if(command.equalsIgnoreCase("y")){
	        	 action = "southwest";
	         } 
	  
	         agent.sendAction(action);
	         agent.actionRequested = false;
	         agent = null;
	         enterAction=true;
	         command = "";
	         index +=1;
			
			if(index == agent_manager.size()) {
				index =0;
				try {
					Thread.sleep(1000);
					this.updateGraphics();
					
				} catch (InterruptedException e1) {
					e1.printStackTrace();
			}
		
			
}
			
			
			this.runAgents();
		}
		else
			command = "";
	}
	public void keyReleased(KeyEvent e) {
		// TODO Auto-generated method stub
		
	}


	public void keyTyped(KeyEvent e) {
			
	}

}
