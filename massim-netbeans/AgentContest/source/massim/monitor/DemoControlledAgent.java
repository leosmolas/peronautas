package massim.monitor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import massim.agent.AbstractAgent;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Description of an agent for the simulation
 * 
 */
public class DemoControlledAgent extends AbstractAgent {

	private int gsizeX = 0, gsizeY = 0;
	private int posX = 0, posY = 0;
	private Vector<String> vectorC = null;
	private Vector<String> vectorD = null;

	private int homex1;
	private int homex2;
	private int homey1;
	private int homey2;

	private int lineOfSight = 0;

	private int state = 1;


	/**
	 * Main method of this class
	 * 
	 * @param args
	 *            Username, Passwort and optional Host
	 */
	public boolean actionRequested = false;
	public String replyID = "0";
	public long deadline=0;
	public DemoControlledAgent(){}
	public DemoControlledAgent(String host,String username, String pass){
	this.setUsername(username);
	this.setPassword(pass);
	this.setHost(host);
	this.setPort(12300);
}
	@Override
	public boolean processMessage(Element el_message) {

		printXML(el_message);
		String type = el_message.getAttribute("type");
		if (type.equals("request-action") || type.equals("sim-start")
				|| type.equals("sim-end")) {
			// get perception
			Element el_perception = null;
			NodeList nl = el_message.getChildNodes();
			String infoelementname = "perception";

			if (type.equals("request-action")) {
				infoelementname = "perception";
			} else if (type.equals("sim-start")) {
				infoelementname = "simulation";
			} else if (type.equals("sim-end")) {
				infoelementname = "sim-result";
			}

			for (int i = 0; i < nl.getLength(); i++) {
				Node n = nl.item(i);
				if (n.getNodeType() == Element.ELEMENT_NODE
						&& n.getNodeName().equalsIgnoreCase(infoelementname)) {
					if (el_perception == null)
						el_perception = (Element) n;
					else {
						System.err
								.println("perception message doesn't contain right number of perception elements");
						return true;
					}
				}
			}

			Document doc = null;
			try {
				doc = documentbuilderfactory.newDocumentBuilder().newDocument();
			} catch (ParserConfigurationException e) {
				System.err.println("parser config error");
				e.printStackTrace();
				System.exit(1);
			}
			Element el_response = doc.createElement("message");

			doc.appendChild(el_response);
			Element el_action = doc.createElement("action");
			el_response.setAttribute("type", "action");
			el_response.appendChild(el_action);

			long currenttime = 0;
			try {
				currenttime = Long.parseLong(el_message
						.getAttribute("timestamp"));
			} catch (NumberFormatException e) {
				System.err.println("number format invalid");
				e.printStackTrace();
				return true;
			}

			
			replyID = el_perception.getAttribute("id");
			if (type.equals("request-action")) {

				try {
					deadline = Long.parseLong(el_perception
						.getAttribute("deadline"));
				} catch (NumberFormatException e) {
					System.err.println("number format invalid");
					e.printStackTrace();
					return true;
				}
				this.actionRequested = true;
//				processRequestAction(el_perception, el_action, currenttime,
//						deadline);
				
			} else if (type.equals("sim-start")) {
				processSimulationStart(el_perception, currenttime);
			} else if (type.equals("sim-end")) {
				processSimulationEnd(el_perception, currenttime);
			}
			return true;
		}
			return false;
	}
	
	
	
	
	
	
private void printXML(Element el_message) {
	System.out.println("Server --> Agent");
    TransformerFactory tfactory = TransformerFactory.newInstance();
    Transformer serializer;
    try {
        serializer = tfactory.newTransformer();
        //Setup indenting to "pretty print"
        serializer.setOutputProperty(OutputKeys.INDENT, "yes");
        serializer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
        
        serializer.transform(new DOMSource(el_message.getOwnerDocument()), new StreamResult(System.out));
    } catch (TransformerException e) {
        // this is fatal, just dump the stack and throw a runtime exception
        e.printStackTrace();
        
        throw new RuntimeException(e);
    }
		
	}
public void sendAction(String action){
	Document doc = null;
	try {
		doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
	} catch (ParserConfigurationException e) {
		System.err.println("parser config error");
		e.printStackTrace();
		System.exit(1);
	}
	Element el_response = doc.createElement("message");

	doc.appendChild(el_response);
	Element el_action = doc.createElement("action");
	el_action.setAttribute("id", this.replyID);
	el_action.setAttribute("type", action);
	//el_action.setAttribute("direction", action);
	el_response.setAttribute("type", "action");
	
	el_response.appendChild(el_action);
	
	try {
		this.sendDocument(doc);
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
}
	@SuppressWarnings("static-access")
	public void processRequestAction(Element perception, Element target,
			long currenttime, long deadline) {

		System.out.print("Enter Your Action: ");

	      //  open up standard input
	      BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
	      String action = "skip";
	      try {
	         String comand = br.readLine();
	         if(comand.equalsIgnoreCase("w")){
	        	 action = "up";
	         }
	         if(comand.equalsIgnoreCase("s")){
	        	 action = "down";
	         }
	         
	         if(comand.equalsIgnoreCase("a")){
	        	 action = "left";
	         }
	         if(comand.equalsIgnoreCase("d")){
	        	 action = "right";
	         }
	      } catch (IOException ioe) {
	         System.out.println("IO error ");
	         System.exit(1);
	      }
		// set the action
		target.setAttribute("type", action);
		vectorC.clear();
		vectorD.clear();
	}

	


	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.AbstractAgent#processLogIn()
	 */
	public void processLogIn() {
		// called as soon as agent logged in successfully
		// TODO: insert code here
		// Note: This is a good place to do a ping
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.AbstractAgent#processPong(java.lang.String)
	 */
	public void processPong(String pong) {
		// react on incoming pong
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

		lineOfSight = 17;

		homex1 = Integer.parseInt(perception.getAttribute("corralx0"));
		homex2 = Integer.parseInt(perception.getAttribute("corralx1"));
		homey1 = Integer.parseInt(perception.getAttribute("corraly0"));
		homey2 = Integer.parseInt(perception.getAttribute("corraly1"));

		// lineOfSight = 5;
		// //Integer.parseInt(perception.getAttribute("lineOfSight"));

		vectorC = new Vector<String>();
		vectorD = new Vector<String>();
	}
}