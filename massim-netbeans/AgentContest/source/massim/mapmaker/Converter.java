package massim.mapmaker;

import java.awt.Color;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Vector;
import java.util.Map.Entry;

import javax.imageio.ImageIO;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class Converter {
	
	private static Color OBSTACLECOLOR = new Color(0,255,0);
	private static Color REDAGENTCOLOR = new Color(255,0,0);
	private static Color REDCORRALCOLOR = new Color(255,100,100);
	private static Color BLUEAGENTCOLOR = new Color(0,0,255);
	private static Color BLUECORRALCOLOR = new Color(100,100,255);
	private static Color COWCOLOR = new Color(255,255,255);
	private static Color FENCECOLOR = new Color(255,255,0);
	private static Color SWITCHCOLOR = new Color(255,100,0);
	
	private Point extractDimensions(BufferedImage img) {
		
		int width = img.getWidth(null);
		int height = img.getHeight(null);
		
		return new Point(width,height);
		
	}
	
	private Collection<Point> extractPoints(BufferedImage img, Color color) {
		
		LinkedList<Point> ret = new LinkedList<Point>();
		
		for( int x = 0 ; x < img.getWidth() ; x++ ) {
			for( int y = 0 ; y < img.getHeight() ; y++ ) {
				
				Color c = new Color(img.getRGB(x, y));
				if( c.equals(color) ) {
					ret.add( new Point(x,y) );
				}
				
			}
		}
				
		return ret;
		
	}

	private Rectangle extractRectangle(BufferedImage img, Color color) {
		
		int minX = Integer.MAX_VALUE;
		int minY = Integer.MAX_VALUE;
		int maxX = Integer.MIN_VALUE;
		int maxY = Integer.MIN_VALUE;
		
		for( int x = 0 ; x < img.getWidth() ; x++ ) {
			for( int y = 0 ; y < img.getHeight() ; y++ ) {
				
				Color c = new Color(img.getRGB(x, y));
				if( c.equals(color) ) {
					if( minX > x)
						minX = x;
					if( minY > y)
						minY = y;
					if( maxX < x)
						maxX = x;
					if( maxY < y)
						maxY = y;
				}
				
			}
		}
				
		return new Rectangle(minX,minY,maxX-minX,maxY-minY);
		
	}
	
	private Map<Point,Point> extractFences(BufferedImage img) {
		
		Collection<Point> switches = new LinkedList<Point>();
		
		// 1. find switches
		for( int x = 0 ; x < img.getWidth() ; x++ ) {
			for( int y = 0 ; y < img.getHeight() ; y++ ) {
				
				Color c = new Color(img.getRGB(x, y));
				if( c.equals(SWITCHCOLOR) ) {
					switches.add( new Point(x,y) );
				}
				
			}
		}

		Map<Point,Point> ret = new HashMap<Point,Point>();
		
		// 2. find directions and length
		for( Point s : switches ) {
			
			// where does the fence start, what is its direction?
			Color cDown = new Color(img.getRGB(s.x, s.y+1));
			Color cLeft = new Color(img.getRGB(s.x-1, s.y));
			Color cUp = new Color(img.getRGB(s.x, s.y-1));
			Color cRight = new Color(img.getRGB(s.x+1, s.y));
			
			// is the fence correct?
			int dir=0;
			int count = 0;
			if( cDown.equals(FENCECOLOR) ) { count ++; dir = 0; }
			if( cLeft.equals(FENCECOLOR) ) { count ++; dir = 1; }
			if( cUp.equals(FENCECOLOR) ) { count ++; dir = 2; }
			if( cRight.equals(FENCECOLOR) ) { count ++; dir = 3; }
			if( count == 0) {
				System.out.println("Switch at " + s + " has no fence.");
				System.exit(0);
			}
			else if( count > 1) {
				System.out.println("Switch at " + s + " has ambiguous fence.");
				System.exit(0);
			}
			
			// now we have the direction. find the length
			int len = 0;
			// down
			if( dir == 0 ) {
				int x = s.x;
				int y = s.y;
				Color c = null;
				do {
					y ++;
					len ++;
					c = new Color(img.getRGB(x, y));
				}
				while( c.equals(FENCECOLOR)) ;
			}
			//left
			else if ( dir == 1) {
				int x = s.x;
				int y = s.y;
				Color c = null;
				do {
					x --;
					len ++;
					c = new Color(img.getRGB(x, y));
				}
				while( c.equals(FENCECOLOR)) ;
				
			}
			// up
			else if ( dir == 2) {
				int x = s.x;
				int y = s.y;
				Color c = null;
				do {
					y --;
					len ++;
					c = new Color(img.getRGB(x, y));
				}
				while( c.equals(FENCECOLOR)) ;
		
			}
			// right
			else if ( dir == 3) {
				int x = s.x;
				int y = s.y;
				Color c = null;
				do {
					x ++;
					len ++;
					c = new Color(img.getRGB(x, y));
				}
				while( c.equals(FENCECOLOR)) ;
		
			}
			
			ret.put(s, new Point(dir,len));
			
		}
		
		return ret;
		
	}

	private Collection<Element> elementsFromCollection(Collection<Point> obstacles, String metaName, Document doc) {
		
		Element arrayX = doc.createElement("array");
		arrayX.setAttribute("meta:length", new Integer(obstacles.size()).toString());
		arrayX.setAttribute("meta:name", metaName + "X");

		Element arrayY = doc.createElement("array");
		arrayY.setAttribute("meta:length", new Integer(obstacles.size()).toString());
		arrayY.setAttribute("meta:name", metaName + "Y");

		int a = 0;
		for( Point o : obstacles ) {
			arrayX.setAttribute("item" + a, new Integer(o.x).toString());
			arrayY.setAttribute("item" + a, new Integer(o.y).toString());
			a ++;
		}
		
		Vector<Element> ret = new Vector<Element>();
		ret.add(arrayX);
		ret.add(arrayY);
		return ret;
		
	}
	
	private Collection<Element> elementsFromFences(Map<Point,Point> fences, Document doc) {
		
		int size = fences.size();
		
		Element arraySwitchX = doc.createElement("array");
		arraySwitchX.setAttribute("meta:length", new Integer(size).toString());
		arraySwitchX.setAttribute("meta:name","switchX");

		Element arraySwitchY = doc.createElement("array");
		arraySwitchY.setAttribute("meta:length", new Integer(size).toString());
		arraySwitchY.setAttribute("meta:name","switchY");

		Element arrayFenceLength = doc.createElement("array");
		arrayFenceLength.setAttribute("meta:length", new Integer(size).toString());
		arrayFenceLength.setAttribute("meta:name","fenceLength");

		Element arrayFenceDirections = doc.createElement("array");
		arrayFenceDirections.setAttribute("meta:length", new Integer(size).toString());
		arrayFenceDirections.setAttribute("meta:name","fenceDirections");

		int a = 0;
		for( Entry<Point, Point> e : fences.entrySet() ) {
			arraySwitchX.setAttribute("item" + a, new Integer(e.getKey().x).toString());
			arraySwitchY.setAttribute("item" + a, new Integer(e.getKey().y).toString());
			arrayFenceLength.setAttribute("item" + a, new Integer(e.getValue().y).toString());
			int dir = e.getValue().x;
			String sDir = "";
			if( dir == 0 ) sDir = "down";
			else if( dir == 1 ) sDir = "left";
			else if( dir == 2 ) sDir = "up";
			else if( dir == 3 ) sDir = "right";
			arrayFenceDirections.setAttribute("item" + a, sDir);
		
			a++;
		}
		
		Vector<Element> ret = new Vector<Element>();
		ret.add(arraySwitchX);
		ret.add(arraySwitchY);
		ret.add(arrayFenceLength);
		ret.add(arrayFenceDirections);
		return ret;
		
	}

	private Collection<Element> elementsFromCorrals(Rectangle corral1, Rectangle corral2, Document doc) {
		
		//<array item0="1" item1="15" meta:length="2" meta:name="stable1Y"/>
		//<array item0="25" item1="44" meta:length="2" meta:name="stable1X"/>
		//<array item0="54" item1="68" meta:length="2" meta:name="stable2Y"/>
		//<array item0="25" item1="44" meta:length="2" meta:name="stable2X"/>
	
		Element array1X = doc.createElement("array");
		array1X.setAttribute("meta:length", "2");
		array1X.setAttribute("meta:name", "stable1X");
		array1X.setAttribute("item0", new Integer(corral1.x).toString());
		array1X.setAttribute("item1", new Integer(corral1.x+corral1.width).toString());

		Element array1Y = doc.createElement("array");
		array1Y.setAttribute("meta:length", "2");
		array1Y.setAttribute("meta:name", "stable1Y");
		array1Y.setAttribute("item0", new Integer(corral1.y).toString());
		array1Y.setAttribute("item1", new Integer(corral1.y+corral1.height).toString());

		Element array2X = doc.createElement("array");
		array2X.setAttribute("meta:length", "2");
		array2X.setAttribute("meta:name", "stable2X");
		array2X.setAttribute("item0", new Integer(corral2.x).toString());
		array2X.setAttribute("item1", new Integer(corral2.x+corral2.width).toString());

		Element array2Y = doc.createElement("array");
		array2Y.setAttribute("meta:length", "2");
		array2Y.setAttribute("meta:name", "stable2Y");
		array2Y.setAttribute("item0", new Integer(corral2.y).toString());
		array2Y.setAttribute("item1", new Integer(corral2.y+corral2.height).toString());

		Vector<Element> ret = new Vector<Element>();
		ret.add(array1X);
		ret.add(array1Y);
		ret.add(array2X);
		ret.add(array2Y);
		return ret;

	}
	
	private Collection<Element> elementsFromAgents(int redAgents, int blueAgents, Document doc) {
		
		Vector<Element> ret = new Vector<Element>();

		
		//<agent agentclass="massim.competition2010.GridSimulationAgent" 
		//  agentcreationclass="massim.competition2010.GridSimulationAgentParameter" team="blue">
		//<configuration/>
		
		for( int a = 0 ; a < redAgents ; a++ ) {

			Element agent = doc.createElement("agent");
			agent.setAttribute("agentclass", "massim.competition2010.GridSimulationAgent");
			agent.setAttribute("agentcreationclass", "massim.competition2010.GridSimulationAgentParameter");
			agent.setAttribute("team", "red");
			
			Element config = doc.createElement("configuration");
			agent.appendChild(config);
	
			ret.add(agent);
			
		}
		
		for( int a = 0 ; a < blueAgents ; a++ ) {

			Element agent = doc.createElement("agent");
			agent.setAttribute("agentclass", "massim.competition2010.GridSimulationAgent");
			agent.setAttribute("agentcreationclass", "massim.competition2010.GridSimulationAgentParameter");
			agent.setAttribute("team", "blue");
			
			Element config = doc.createElement("configuration");
			agent.appendChild(config);
	
			ret.add(agent);
			
		}

		return ret;
		
	}

	public Converter(File pngFile, File xmlFile, File destFile) {
		
		// 1. load the png as an image
		BufferedImage image = null;

		try {
			image = ImageIO.read(pngFile);
		} catch (IOException e) {
			e.printStackTrace();
		}

		
		// 2. get the topology from the image
		// 2.1 dimensions
		Point dim = extractDimensions(image);
		//System.out.println("Dimensions: " + dim);
		
		// 2.2 obstacles
		Collection<Point> obstacles = extractPoints(image,OBSTACLECOLOR);
		//System.out.println("Obstacles: " + obstacles);
		
		// 2.3 red and blue agents 
		Collection<Point> redAgents = extractPoints(image,REDAGENTCOLOR);
		//System.out.println("Red agents: " + redAgents);
		Collection<Point> blueAgents = extractPoints(image,BLUEAGENTCOLOR);
		//System.out.println("Blue agents: " + blueAgents);

		// 2.4 red and blue corrals
		Rectangle redCorral = extractRectangle(image,REDCORRALCOLOR);
		//System.out.println("Red corral: " + redCorral);
		Rectangle blueCorral = extractRectangle(image,BLUECORRALCOLOR);
		//System.out.println("Blue corral: " + blueCorral);
		
		// 2.5 cows
		Collection<Point> cows = extractPoints(image,COWCOLOR);
		//System.out.println("Cows: " + cows);

		// 2.6 switches and fences
		Map<Point,Point> fences = extractFences(image);
		
		// TODO 3. write to the XML-file (replace)
		// TODO 3.1 parse XML-file
		Document doc = null;
		try {
			DocumentBuilderFactory documentbuilderfactory = DocumentBuilderFactory.newInstance();
			//documentbuilderfactory.setExpandEntityReferences(false); // do not resolve entities
			doc = documentbuilderfactory.newDocumentBuilder().parse(xmlFile);
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}
		
		// get the root
		Element root = doc.getDocumentElement();

		// find configuration-tag
		NodeList sims = doc.getElementsByTagName("simulation");
		//System.out.println(sims.getLength());
		Element sim = (Element) sims.item(0);
		Element config = null;
		for( int a = 0 ; a < sim.getChildNodes().getLength() ; a++ ) {
			Node node =  sim.getChildNodes().item(a);
			if( node.getNodeName().equals("configuration")) {
				config = (Element)node;
				break;
			}
		}
		assert sim != null;
		assert config != null;
		
		// clean
		removeAll(config,Node.ELEMENT_NODE,"array");
		//removeAll(config,Node.TEXT_NODE,"#text");
		
		// replace id
		String id = pngFile.getName();
		id = id.substring(0, id.indexOf("."));
		sim.removeAttribute("id");
		sim.setAttribute("id", id);
		
		
		// TODO 3.2 dimensions
		config.removeAttribute("sizex");
		config.setAttribute("sizex", new Integer(dim.x).toString());
		config.removeAttribute("sizey");
		config.setAttribute("sizey", new Integer(dim.y).toString());

		Collection<Element> newArrays = new LinkedList<Element>();
		
		// 3.3 obstacles
		config.removeAttribute("numberOfObstacles");
		config.setAttribute("numberOfObstacles", new Integer(obstacles.size()).toString());
		newArrays.addAll(elementsFromCollection(obstacles,"obstaclePosition",doc));
		
		// 3.4 agents
		Collection<Point> agents = new LinkedList<Point>();
		agents.addAll(redAgents);
		agents.addAll(blueAgents); // ORDER?
		config.removeAttribute("numberOfAgents");
		config.setAttribute("numberOfAgents", new Integer(agents.size()).toString());
		newArrays.addAll(elementsFromCollection(agents,"agentPosition",doc));

		// 3.5 corrals
		newArrays.addAll(elementsFromCorrals(redCorral,blueCorral,doc));
		
		// 3.6 cows
		config.removeAttribute("numberOfCows");
		config.setAttribute("numberOfCows", new Integer(cows.size()).toString());
		newArrays.addAll(elementsFromCollection(cows,"cowPosition",doc));
		
		// 3.7 switches and fences
		config.removeAttribute("numberOfFences");
		config.setAttribute("numberOfFences", new Integer(fences.size()).toString());
		newArrays.addAll(this.elementsFromFences(fences,doc));
		
		
		// 3.8 agent tags
		removeAll(sim,Node.ELEMENT_NODE,"agent");
		Collection<Element> newAgents = elementsFromAgents(redAgents.size(),blueAgents.size(),doc);
		
		// 3.9 replace/insert tags and write file
		for( Element element : newArrays )
			config.appendChild(element);

		Element agentsElement = doc.createElement("agents");

		for( Element element : newAgents )
			agentsElement.appendChild(element);

		sim.appendChild(agentsElement);
		
		try {

			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			DOMSource source = new DOMSource( doc );
			FileOutputStream os     = new FileOutputStream( destFile );
			StreamResult result = new StreamResult( os );
			transformer.transform( source, result );
			
			System.out.println("Result written to " + destFile);
	
		} catch (TransformerConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	
	public static void removeAll(Node node, short nodeType, String name) { 
		if (node.getNodeType() == nodeType &&  (name == null || node.getNodeName().equals(name))) { 
			node.getParentNode().removeChild(node); 
		} else { 
			// Visit the children 
			NodeList list = node.getChildNodes(); 
			for (int i=0; i<list.getLength(); i++) { 
				removeAll(list.item(i), nodeType, name); 
			} 
		} 
	} 

}
