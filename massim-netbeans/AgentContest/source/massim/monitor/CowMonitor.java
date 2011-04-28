package massim.monitor;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferStrategy;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.HashMap;

import javax.swing.ImageIcon;

import massim.framework.rmi.XMLDocumentServer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

@SuppressWarnings("serial")
public class CowMonitor extends AbstractSimulationOutput {


	public int numberOfAgents = 0;
	private Image bufImg = null;
	private Graphics bufGph = null;
//	private Document xmlDoc = null;
	private boolean simulationRunning = false;
	public String teamName1 = null;
	
	private int[][] simulationField = null;
	private HashMap<String, Point> agent_map=new HashMap<String, Point>();;
	private HashMap<String,String> team_name=new HashMap<String, String>();;
	
	int l = 16;
	public boolean monitorStarted = false;
	public String rmihost="localhost";
	public int rmiport = 1099;
	public String service;
	private boolean configured = false;


	public CowMonitor(String[] args){
		
			System.out.println("CowMonitor [-rmihost] [host] [-rmiport] [port]");
		
		for(int i = 0; i<args.length;i++){
			if(args[i].equalsIgnoreCase("-rmihost"))
				this.rmihost = args[i+1];
			if(args[i].equalsIgnoreCase("-rmiport"))
				this.rmiport = Integer.parseInt(args[i+1]);
			
		}
		
	}
	
	public CowMonitor() {
		
	}
	private void showScreen() {
		
		simulationField = new int[this.getOutputSizeX()][this.getOutputSizeY()];

		for (int i = 0; i < this.getOutputSizeX(); i++) {

			for (int j = 0; j < this.getOutputSizeY(); j++) {
				
				simulationField[i][j] = 0;
			}
		}
		
		Toolkit toolkit = Toolkit.getDefaultToolkit();

		// Get the current screen size
		Dimension scrnsize = toolkit.getScreenSize();
		scrnsize.width-=50;
		scrnsize.height-=50;
		
		l = (scrnsize.width/this.getOutputSizeX());

		if (l*this.getOutputSizeY()>scrnsize.height) l = scrnsize.height/this.getOutputSizeY();

		this.setSize(l * this.getOutputSizeX() + 50, l * this.getOutputSizeY() + 50);
		this.setTitle("Cows simulation");
		this.setResizable(true);
		this.setVisible(true);
		this.createBufferStrategy(2);
		
		this.addWindowListener(new WindowAdapter() {
			
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		});
	}

	@Override
	public void configure() {
		String filesep = "/";

		java.net.URL url0 =    CowMonitor.class.getResource("img"+filesep +"grass.png");
		java.net.URL url1 =    CowMonitor.class.getResource("img"+filesep +"tree.png");
		java.net.URL url2 =    CowMonitor.class.getResource("img"+filesep +"cow.png");
		java.net.URL url3 =    CowMonitor.class.getResource("img"+filesep +"fence.png");
		java.net.URL url4 =    CowMonitor.class.getResource("img"+filesep +"agent_red.png");
		java.net.URL url5 =    CowMonitor.class.getResource("img"+filesep +"agent_blue.png");
		java.net.URL url6 =    CowMonitor.class.getResource("img"+filesep +"fence.png");
		java.net.URL url7 =    CowMonitor.class.getResource("img"+filesep +"switch.png");
		java.net.URL url8 =    CowMonitor.class.getResource("img"+filesep +"agent_controlled.png");
		java.net.URL url9 =    CowMonitor.class.getResource("img"+filesep +"Stable1.png");
		java.net.URL url10 =    CowMonitor.class.getResource("img"+filesep +"Stable2.png");
		
		Image[] images = new Image[11];
		images[0] = new ImageIcon(url0).getImage();
		images[1] = new ImageIcon(url1).getImage();
		images[2] = new ImageIcon(url2).getImage();
		images[3] = new ImageIcon(url3).getImage();
		images[4] = new ImageIcon(url4).getImage();
		images[5] = new ImageIcon(url5).getImage();
		images[6] = new ImageIcon(url6).getImage();
		images[7] = new ImageIcon(url7).getImage();
		images[8] = new ImageIcon(url8).getImage();
		images[9] = new ImageIcon(url9).getImage();
		images[10] = new ImageIcon(url10).getImage();

		MediaTracker mt = new MediaTracker(this);

		mt.addImage(images[0], 0);
		mt.addImage(images[1], 1);
		mt.addImage(images[2], 2);
		mt.addImage(images[3], 3);
		mt.addImage(images[4], 4);
		mt.addImage(images[5], 5);
		mt.addImage(images[6], 6);
		mt.addImage(images[7], 7);
		mt.addImage(images[8], 8);

		try {
			mt.waitForAll();
		} catch (InterruptedException e1) {

			e1.printStackTrace();
		}
		this.setImages(images);
		this.showScreen();
		configured  = true;

	}

	/**
	 * 
	 */
	
	private void runMonitor() {
	this.searchService();
		while (true) {

		Document xmlDoc = 	getRMIObject(this.rmihost, this.rmiport,
					this.service);
		if(!configured){
			this.configure(xmlDoc);
		}
			if (xmlDoc != null && configured) {
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
				monitorStarted = true;
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {

					e.printStackTrace();
				}
			} else{
				configured=false;
				this.runMonitor();
			}
				
		}
	}

	protected void configure(Document xmlDoc) {
		Element root = xmlDoc.getDocumentElement();
		NodeList nl = root.getElementsByTagName("state");
		Element state = (Element) nl.item(0);
		int sizex = Integer.parseInt(state.getAttribute("sizex"));
		int sizey = Integer.parseInt(state.getAttribute("sizey"));
		this.setOutputSize(sizex, sizey);
		
		this.configure();
		configured  = true;
	}
	public void searchService() {
		boolean serviceRunning = false;
		while(!serviceRunning){
			try {
				Registry r = LocateRegistry.getRegistry(rmihost,rmiport);
				String[] s = r.list();
				for(int i = 0 ; i< s.length; i++){
					System.out.println("RMI Service found: "+s[i]);
					if(s[i].contains("xmlsimulationmonitor")){
						serviceRunning = true;
						service = s[i];
						System.out.println("take RMI Service: "+s[i]);
						break;
					}	
				
				}
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
				
					//e.printStackTrace();
				}
				
			} catch (RemoteException e1) {
				System.out.println("There is no running server on: "+this.rmihost+":"+this.rmiport);
				configured  = false;
				try {
					Thread.sleep(3000);
				} catch (InterruptedException e) {
				
					//e.printStackTrace();
				}
				//e1.printStackTrace();			
			}
			
		}
		
	}

	protected void paintControlledAgent(String username) {
		Point a = agent_map.get(username);
		simulationField[a.x][a.y] = 8;
		repaint();

	}


	public void paint(Graphics g) {

		Image[] images = this.getImages();

		for (int i = 3; i < this.getOutputSizeX() + 3; i++) {

			for (int j = 3; j < this.getOutputSizeY() + 3; j++) {

				int a = i - 3;
				int b = j - 3;
				// paint grass
				if (simulationField[a][b] == 0) {

					g.drawImage(images[0], i * l, j * l, l, l, null, null);

				}
				// paint tree

				else if (simulationField[a][b] == 1) {

					g.drawImage(images[1], i * l, j * l, l, l, null, null);
				}

				// paint switch
				else if (simulationField[a][b] == 7) {

					g.drawImage(images[7], i * l, j * l, l, l, null, null);

				}
				// paint fence
				else if (simulationField[a][b] == 6) {

					g.drawImage(images[6], i * l, j * l, l, l, null, null);

				}

				// paint cow

				else if (simulationField[a][b] == 2) {

					g.drawImage(images[2], i * l, j * l, l, l, null, null);
				}

				// paint agent blue

				else if (simulationField[a][b] == 4) {

					g.drawImage(images[4], i * l, j * l, l, l, null, null);

				}
				// paint agent red

				else if (simulationField[a][b] == 5) {

					g.drawImage(images[5], i * l, j * l, l, l, null, null);

				}

				// paint controlled agent
				else if (simulationField[a][b] == 8) {

					g.drawImage(images[8], i * l, j * l, l, l, null, null);

				}
				// paint fence

				else if (simulationField[a][b] == 3) {
					g.drawImage(images[3], i * l, j * l, l, l, null, null);
				}

				// paint stable 1
				else if (simulationField[a][b] == 9) {

					g.drawImage(images[9], i * l, j * l, l, l, null, null);

				}
				// paint stable 2
				else if (simulationField[a][b] == 10) {

					g.drawImage(images[10], i * l, j * l, l, l, null, null);

				}

			}
		}
	}

	/**
	 * 
	 */
	public void update(Graphics g) {

		int w = this.getSize().width;
		int h = this.getSize().height;

		if (bufImg == null) {
			bufImg = this.createImage(w, h);
			bufGph = bufImg.getGraphics();
		}
		bufGph.setColor(this.getBackground());
		bufGph.fillRect(0, 0, w, h);
		bufGph.setColor(this.getForeground());

		paint(bufGph);

		g.drawImage(bufImg, 0, 0, this);

	}

	String team1 = null;

	@Override
	public void updateOutput(Document doc) {
		
	//	BackupWriter.write(doc, "angentcontroller", "agentcontroller.xml");
		for (int i = 0; i < this.getOutputSizeX(); i++) {

			for (int j = 0; j < this.getOutputSizeY(); j++) {

				this.simulationField[i][j] = 0;
			}
		}

		Element root = doc.getDocumentElement();
		NodeList nl;
		
		nl = doc.getElementsByTagName("stable1");
		for (int i = 0; i < nl.getLength(); i++) {
			Element e1 = (Element) nl.item(i);

			int posX = Integer.parseInt(e1.getAttribute("posx"));
			int posY = Integer.parseInt(e1.getAttribute("posy"));

			this.simulationField[posX][posY] = 9;

		}

		nl = doc.getElementsByTagName("stable2");
		for (int i = 0; i < nl.getLength(); i++) {
			Element e1 = (Element) nl.item(i);

			int posX = Integer.parseInt(e1.getAttribute("posx"));
			int posY = Integer.parseInt(e1.getAttribute("posy"));

			this.simulationField[posX][posY] = 10;

		}
		nl = root.getElementsByTagName("tree");

		for (int i = 0; i < nl.getLength(); i++) {

			Element e1 = (Element) nl.item(i);

			int treeX = Integer.parseInt(e1.getAttribute("posx"));
			int treeY = Integer.parseInt(e1.getAttribute("posy"));

			this.simulationField[treeX][treeY] = 1;

		}

		nl = doc.getElementsByTagName("cow");

		for (int i = 0; i < nl.getLength(); i++) {

			Element el = (Element) nl.item(i);
			int posX = Integer.parseInt(el.getAttribute("posx"));
			int posY = Integer.parseInt(el.getAttribute("posy"));
			this.simulationField[posX][posY] = 2;

		}

		nl = doc.getElementsByTagName("fence");

		for (int i = 0; i < nl.getLength(); i++) {

			Element e1 = (Element) nl.item(i);

			int posX = Integer.parseInt(e1.getAttribute("posx"));
			int posY = Integer.parseInt(e1.getAttribute("posy"));

			this.simulationField[posX][posY] = 6;

		}

		nl = doc.getElementsByTagName("switch");

		for (int i = 0; i < nl.getLength(); i++) {

			Element e1 = (Element) nl.item(i);

			int posX = Integer.parseInt(e1.getAttribute("posx"));
			int posY = Integer.parseInt(e1.getAttribute("posy"));

			this.simulationField[posX][posY] = 7;

		}


		nl = doc.getElementsByTagName("agent");

		if (team1 == null) {
			Element a = (Element) nl.item(0);
			team1 = a.getAttribute("team");
		}

		for (int i = 0; i < nl.getLength(); i++) {

			Element e1 = (Element) nl.item(i);

			int posX = Integer.parseInt(e1.getAttribute("posx"));
			int posY = Integer.parseInt(e1.getAttribute("posy"));

			String name = e1.getAttribute("name");
			agent_map.put(name, new Point(posX, posY));

			if (e1.getAttribute("team").equalsIgnoreCase(team1)) {

				this.simulationField[posX][posY] = 4;
			} else
				this.simulationField[posX][posY] = 5;
		}
	}
	/**
	 * 
	 * @param host
	 * @param port
	 * @param service
	 */
	protected Document getRMIObject(String host, int port, String service) {
		Document	xmlDoc = null;
		try {
			
			simulationRunning = true;
			Registry registry = LocateRegistry.getRegistry(host,port); 
			XMLDocumentServer server_state = (XMLDocumentServer) registry.lookup(service);
			try {
			xmlDoc= server_state.getXMLDocument();
			} catch (NullPointerException e) {
				simulationRunning = false;
				throw new RemoteException(
						"NullPointerException while trying to get XMLDocument",
						e);
				
			}
		} catch (RemoteException e) {
			System.err.println("Currently no simulation running on " + e + " " +host
					+ " " + port + "...\n");
			
		} catch (NotBoundException e) {
			System.err.println("Currently no simulation running on " + e + host
					+ " " + port + "...\n");
			
		}
		return xmlDoc;
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String args[]) {
//args[0] = competition2008/config/serverconfig.xml
		CowMonitor cow = new CowMonitor(args);
		cow.runMonitor();

	}

}
