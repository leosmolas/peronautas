package massim.competition2011.monitor;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import massim.framework.rmi.XMLDocumentServer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * This is the RMI XML monitor application for 2011 Mars Scenario. Start with these options:<br/>
 * <br/>
 * <code>GraphMonitor [-rmihost &lt;host&gt;] [-rmiport &lt;port&gt;] [-savexmls [&lt;folder&gt;]]</code><br/>
 * <br/>
 * You can specify the host and the port of the RMI server 
 * (by default <code>localhost</code> and <code>1099</code>).<br/>
 * <br/>
 * By activating the <code>-savexmls</code> flag, the monitor stores all the well-formed XMLs received in a
 * properly named sub-folder of the specified <code>folder</code>, for later visualization using the
 * file-based viewer. If the <code>-savexmls</code> flag is active and no folder is specified, the current
 * working folder is used.
 */
public class GraphMonitor extends JFrame implements ActionListener {

	private static final long serialVersionUID = 4099856653707665324L;
	
	// Config Info.
	private boolean saveXMLs = false;
	private String xmlsFolder = null;
	private String simId = null;
	private String fileBaseName = null;
	
	// 
	/*
	Color [] teamDomColors =  {new Color(255,0,0),new Color(0,255,0),new Color(0,0,255),
			   new Color(255,255,0),new Color(255,0,255),new Color(0,255,255)};;
	Color [] agentColors = {new Color(255,0,0),new Color(0,255,0),new Color(0,0,255),
							   new Color(255,255,0),new Color(255,0,255),new Color(0,255,255)};
	/*/
	Color [] teamDomColors = {new Color(0,158,115),new Color(0,114,178),new Color(240,228,66),
			new Color(204,121,167),new Color(86,180,233),new Color(230,159,0),new Color(213,94,0)};
	Color [] agentColors = {new Color(0,158,115),new Color(0,114,178),new Color(240,228,66),
			new Color(204,121,167),new Color(86,180,233),new Color(230,159,0),new Color(213,94,0)};
	//*/
	

	// RMI info
//	public boolean monitorStarted = false;
	public String rmihost="localhost";
	public int rmiport = 1099;
	public String service;
	
	@SuppressWarnings("unused")
	private boolean simulationRunning;
	
	// Window/panels
	WorldView worldView;
	JScrollPane scrollPane;
	InfoPanel infoPanel;
	JButton pauseButton;
	
	// Status
	private boolean parsedDoc = false;
	private Boolean paused = false;
	
	// World
	private Vector<String> teams;
	private Vector<NodeInfo> nodes;
	private Vector<EdgeInfo> edges;
	private Vector<TeamInfo> teamsInfo;
	private String simStep;
	
	// For sync purposes
	Object syncObject = new Object();
	
	String selectedNode = null;
	String selectedAgent = null;

	
	
	// Auxiliary classes:
	
	private class NodeInfo {
		public int x;
		public int y;
		public int gridY;
		public int gridX;
		public int weight;
		public int dominatorTeam;
		public String dominatorTeamName;
		public String name;
		public Vector<AgentInfo> agents;
	}
	
	private class AgentInfo {
		public String name;
		public String role;
		public int team;
		public String teamName;
		public String node;
		public int maxHealth;
		public int health;
		public int strength;
		public int maxEnergy;
		public int maxEnergyDisabled;
		public int energy;
		public int visRange;
		public int x;
		public int y;
		public String lastAction;
		public String lastActionParam;
		public String lastActionResult;
	}
	
	private class EdgeInfo {
		public int weight;
		public NodeInfo node1;
		public NodeInfo node2;
	}

	private class TeamInfo {
		public int zonesScore;
		public int score;
		public int achievementPoints;
		public int usedAchievementPoints;
		public int stepScore;
		public String name;
		public Vector<String> achievements;
		public Vector<String> provedNodes;
	}
	
	private class WorldView extends JPanel implements MouseListener, ActionListener {
		
		private static final long serialVersionUID = 3959575661850468381L;

		private int sizeX;
		private int sizeY;
		private int margin = 50;
		private final int nodeRadius = 10;
		private final int agentRadius = 10;
		private Image bgImage = null;
		
		private double[] zooms = {0.1, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0};
		private int zoomIdx = 4;
		private boolean autoZoom = false;
		private double scale = 1.0;
		

		
		public WorldView(){
			this.addMouseListener(this);
			String filesep = "/";
			java.net.URL url =    GraphMonitor.class.getResource("img"+filesep +"Surface.png");
			bgImage = new ImageIcon(url).getImage();
			
			MediaTracker mt = new MediaTracker(this);
			mt.addImage(bgImage, 0);
			try {
				mt.waitForAll();
			} catch (InterruptedException e1) {
				e1.printStackTrace();
			}
		}
		
		private void calculateSize(){
			NodeInfo agentNode = nodes.firstElement();
			int maxX = agentNode.x;
			int maxY = agentNode.y;
			for(NodeInfo node: nodes){
				if (node.x > maxX){
					maxX = node.x;
				}
				if (node.y > maxY){
					maxY = node.y;
				}
			}
			/*
			if (!autoZoom){
				sizeX = (int)Math.round((maxX + margin) * scale);
				sizeY = (int)Math.round((maxY + margin) * scale);
			} else {
				sizeX = scrollPane.getVisibleRect().width-scrollPane.getVerticalScrollBar().getWidth();
				sizeY = scrollPane.getVisibleRect().height-scrollPane.getHorizontalScrollBar().getHeight();				
				scale = Math.min(((double)sizeX) / (maxX + margin), ((double)sizeY) / (maxY + margin));
			}
			this.setPreferredSize(new Dimension(sizeX, sizeY));
			/*/
			if (autoZoom){
				// If autoZoom (fit to screen), define the scale based on the width and height of the graph, and the size of the viewport.
				scale = Math.min(((double)scrollPane.getVisibleRect().width) / (maxX + margin), ((double)scrollPane.getVisibleRect().height) / (maxY + margin));
			}
			sizeX = (int)Math.round((maxX + margin) * scale);
			sizeY = (int)Math.round((maxY + margin) * scale);
			
			if (autoZoom){
				int viewX = scrollPane.getVisibleRect().width - scrollPane.getVerticalScrollBar().getWidth();
				int viewY = scrollPane.getVisibleRect().height - scrollPane.getHorizontalScrollBar().getHeight();
				this.setPreferredSize(new Dimension(viewX, viewY));
			} else {
				this.setPreferredSize(new Dimension(sizeX, sizeY));
			}
			//*/

			this.revalidate();
			
		}
		
		public void paint(Graphics g) {
			super.paint(g);
			
			Graphics2D g2d = (Graphics2D)g;
			
			g2d.scale(1, 1);
			
			RenderingHints renderingHints = new RenderingHints(
					RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			g2d.addRenderingHints(renderingHints);
			
			// clear
			g2d.setColor(new Color(0,0,0));
			g2d.fillRect(0, 0, this.getSize().width, this.getSize().height);
			
			if (!parsedDoc){
				return;
			}
						
			synchronized (syncObject) {
				calculateSize();
				
				g2d.drawImage(bgImage, 0, 0, sizeX, sizeY, null);
				
				g2d.scale(scale, scale);				
				
				for ( EdgeInfo e : edges ) {
					
					

					
					int domTeam = -1;
					if (e.node1.dominatorTeam != -1 && e.node1.dominatorTeam == e.node2.dominatorTeam){
						domTeam = e.node1.dominatorTeam;
						
						g2d.setColor(new Color(000,000,000));
						g2d.setStroke(new BasicStroke(4));
						g2d.drawLine(e.node1.x, e.node1.y,
									 e.node2.x, e.node2.y);
					}			
					if( domTeam < -1 || domTeam >= teamDomColors.length) {
						throw new AssertionError("Implement (not enough colors defined)");
					}
					g2d.setColor(domTeam == -1? new Color(200,200,200): teamDomColors[domTeam]);
					g2d.setStroke(new BasicStroke(2));
					g2d.drawLine(e.node1.x, e.node1.y,
								 e.node2.x, e.node2.y);
	
					int x = (int) (( e.node1.x + e.node2.x) / 2.0f);
					int y = (int) (( e.node1.y + e.node2.y) / 2.0f);
//					g2d.setColor(new Color(0,0,0));
//					g2d.setColor(new Color(255,255,255));
					g2d.setStroke(new BasicStroke(1));
					g2d.fillOval(x-7, y-7, 13, 13);
//					g2d.setColor(new Color(255,255,255));
					g2d.setColor(new Color(0,0,0));
					String str = "" + e.weight;
					g2d.drawChars(str.toCharArray(), 0, str.length(), x-4, y+4);
					
				} 
				
				for ( NodeInfo n : nodes) {
					
					if ( n.name.equals(selectedNode) ) {
						
						g2d.setColor(new Color(255,255,255));
						g2d.setStroke(new BasicStroke(6));
						g2d.drawOval(n.x - nodeRadius, n.y - nodeRadius, 2*nodeRadius, 2*nodeRadius);

					}
					
					int domTeam = n.dominatorTeam;
					if( domTeam < -1 || domTeam >= teamDomColors.length) {
						throw new AssertionError("Implement (not enough colors defined)");
					}
					g2d.setColor(new Color(0,0,0));
					g2d.setStroke(new BasicStroke(1));
					g2d.fillOval(n.x - (nodeRadius + 0 ), n.y - (nodeRadius + 0 ), 2*(nodeRadius + 0 ), 2*(nodeRadius + 0 ));
					
					g2d.setColor(domTeam == -1? new Color(155,155,155): teamDomColors[domTeam]);
					g2d.setStroke(new BasicStroke(2));
					g2d.drawOval(n.x - nodeRadius, n.y - nodeRadius, 2*nodeRadius, 2*nodeRadius);
					
					g2d.setColor(new Color(0,0,0));
					g2d.setStroke(new BasicStroke(1));
					g2d.drawOval(n.x - (nodeRadius + 1), n.y - (nodeRadius + 1), 2*(nodeRadius + 1), 2*(nodeRadius + 1));
			
//					g2d.setColor(new Color(0,0,0));
//					g2d.setStroke(new BasicStroke(1));
//					g2d.fillOval(n.x - nodeRadius, n.y - nodeRadius, 2*nodeRadius, 2*nodeRadius);
					
					
					TeamInfo team = null;
					if (selectedNode != null){
						NodeInfo node = searchNode(selectedNode, nodes);
						team = searchTeam(node.dominatorTeamName, teamsInfo);
					} else if (selectedAgent != null){
						AgentInfo agent = searchAgent(selectedAgent, nodes);
						team = searchTeam(agent.teamName, teamsInfo);
					}
					
					if (team != null && team.provedNodes != null && team.provedNodes.contains(n.name)){
						g2d.setColor(new Color(128,255,255));
					} else {
						g2d.setColor(new Color(255,255,255));
					}
					String str = "" + n.weight;
					g2d.drawChars(str.toCharArray(), 0, str.length(), n.x-4, n.y+4);

					float offset = nodeRadius + agentRadius + 4.0f;
					float angle = 3.14159f / 16.0f;
					float agentsPerCircle = 6.0f;
					int nextCircle = (int) agentsPerCircle;
					int i = 0;			
					
					for ( AgentInfo ag : n.agents ) {
						
						i++;
						
						ag.x = (int) (Math.sin(angle) * offset) + n.x;
						ag.y = (int) (Math.cos(angle) * offset) + n.y;
											
						if ( ag.name.equals(selectedAgent) ) {
							
							g2d.setColor(new Color(255,255,255));
							g2d.setStroke(new BasicStroke(6));
							g2d.drawRoundRect(ag.x - agentRadius, ag.y - agentRadius, 2*agentRadius, 2*agentRadius, 3, 3);

						}
						
						
						if( ag.team < 0 || ag.team >= agentColors.length) {
							throw new AssertionError("Implement (not enough colors defined)");
						}
						Color agentColor = agentColors[ag.team];
						
						g2d.setStroke(new BasicStroke(1));
						g2d.setColor(ag.health > 0? new Color(0,0,0): new Color(120,120,120));
						//g2d.fillOval(agX - agentRadius, agY - agentRadius, 2*agentRadius, 2*agentRadius);
						g2d.fillRoundRect(ag.x - (agentRadius+0), ag.y - (agentRadius+0), 2*(agentRadius+0), 2*(agentRadius+0), 6, 6);
	
						
						
						g2d.setColor(agentColor);
						g2d.setStroke(new BasicStroke(2));
						//g2d.drawOval(agX - agentRadius, agY - agentRadius, 2*agentRadius, 2*agentRadius);
						g2d.drawRoundRect(ag.x - agentRadius, ag.y - agentRadius, 2*agentRadius, 2*agentRadius, 6, 6);
						
						g2d.setColor(new Color(0,0,0));
						g2d.setStroke(new BasicStroke(1));
						//g2d.drawOval(agX - agentRadius, agY - agentRadius, 2*agentRadius, 2*agentRadius);
						g2d.drawRoundRect(ag.x - (agentRadius+1), ag.y -  (agentRadius+1), 2* (agentRadius+1), 2* (agentRadius+1), 6, 6);
						
						g2d.setColor(new Color(255,255,255));
	
						str = "" + ag.energy + "|" + ag.health;
						g2d.drawChars(str.toCharArray(), 0, str.length(), ag.x-8, ag.y+5);
						
						//angle += 2.0f * 3.14159f / 8.0f;
						angle += 2.0f * 3.14159f / agentsPerCircle;
						
						if (i == nextCircle){
							offset += 2.0f * (float)agentRadius +4.0f;
							agentsPerCircle = (float)((int)(offset * 3.14159f / ((float)agentRadius + 1.0f)));
							nextCircle += (int)agentsPerCircle;
						}					
					}
				}
			}
		}

		@Override
		public void mouseClicked(MouseEvent e) {		
			if ( e.getClickCount() == 1 && parsedDoc) {			
				clickAt(e.getPoint().x,e.getPoint().y, e.getButton());			
			}		
			this.repaint();
		}

		@Override
		public void mouseEntered(MouseEvent e) {}

		@Override
		public void mouseExited(MouseEvent e) {}

		@Override
		public void mousePressed(MouseEvent e) {}

		@Override
		public void mouseReleased(MouseEvent e) {}
		
		public void clickAt(int x, int y, int button) {
			
			x = (int)Math.round(x/scale);
			y = (int)Math.round(y/scale);
			
			synchronized (syncObject) {
				for (NodeInfo n : nodes) {
					if ((n.x - x) * (n.x - x) + (n.y - y) * (n.y - y) <= nodeRadius
							* nodeRadius) {
						String oldNode = selectedNode;
						selectedNode = n.name;
						selectedAgent = null;
						firePropertyChange("nodeSelected", oldNode,
								selectedNode);
						return;
					}
					for (AgentInfo ag : n.agents) {
						//if( (ag.x - x)*(ag.x - x) + (ag.y - y)*(ag.y - y) <= agentRadius*agentRadius ) {
						if (Math.abs(ag.x - x) <= agentRadius
								&& Math.abs(ag.y - y) <= agentRadius) {
							String oldAgent = selectedAgent;
							selectedAgent = ag.name;
							selectedNode = null;
							firePropertyChange("agentSelected", oldAgent,
									selectedAgent);
						}
					}
				}
			}
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if ("zoomIn".equals(e.getActionCommand())){
				autoZoom = false;
				if ( zoomIdx < zooms.length-1){
					zoomIdx++;
				}
				scale = zooms[zoomIdx];
			}
			else if ("zoomOut".equals(e.getActionCommand())){
				autoZoom = false;
				if ( zoomIdx > 0){
					zoomIdx--;
				}
				scale = zooms[zoomIdx];
			}
			else if ("fitWindow".equals(e.getActionCommand())){
				if (autoZoom){
					scale = zooms[zoomIdx];
				}
				autoZoom = !autoZoom;
			}
			this.repaint();
		}
	
	}
	
	private class InfoPanel extends JPanel implements PropertyChangeListener{

		private static final long serialVersionUID = 1663621252127221096L;
		
		
		private Vector<JLabel> labels;
		
		public InfoPanel() {
			super(true);
			this.setPreferredSize(new Dimension(150, 0));
			this.setLayout(new BoxLayout(this,BoxLayout.Y_AXIS));
			labels = new Vector<JLabel>(25);
			
		}
		
		@Override
		public void propertyChange(PropertyChangeEvent evt) {
			if ("agentSelected".equals(evt.getPropertyName()) ||
					"nodeSelected".equals(evt.getPropertyName())) {
				String newValue = (String)evt.getNewValue();
				String oldValue = (String)evt.getOldValue();
				if (newValue.equals(oldValue)){
					return;
				}
				update();
			}
		}
		
		public void update(){
			NodeInfo node = null;
			AgentInfo agent = null;
			TeamInfo team = null;
			String step = null;
			String simName = null;
			int i = 0;
			
			synchronized (syncObject) {
				step = simStep;
				simName = simId;
				if (selectedNode != null){
					node = searchNode(selectedNode, nodes);
					team = searchTeam(node.dominatorTeamName, teamsInfo);
				} else if (selectedAgent != null){
					agent = searchAgent(selectedAgent, nodes);
					team = searchTeam(agent.teamName, teamsInfo);
				}
			}
			
			getLabel(i++).setText("     ----SIMULATION----   ");
			getLabel(i++).setText("  " + simName);
			getLabel(i++).setText("  Step:  " + step);
			getLabel(i++).setText("  Current ranking:");
			for (TeamInfo rkTeam: teamsInfo){
				if (team != null){
					if (team.equals(rkTeam)){
						getTeamColoredLabel(i++,rkTeam).setText("    " + rkTeam.name + ":     " + rkTeam.score);
					} else {
						getLabel(i++).setText("    " + rkTeam.name + ":     " + rkTeam.score);
					}
				} else {
					getTeamColoredLabel(i++,rkTeam).setText("    " + rkTeam.name + ":     " + rkTeam.score);
				}
			}
			if (node != null){
				getLabel(i++).setText("    ");
				getLabel(i++).setText("     ----VERTEX----   ");
				getLabel(i++).setText("  Node name:   " + node.name);
				getLabel(i++).setText("  Weight:   " + node.weight);
				getLabel(i++).setText("  Domintad by:   " + (node.dominatorTeamName!= null ? node.dominatorTeamName:"-"));
			} else if (agent != null){
				getLabel(i++).setText("    ");
				getLabel(i++).setText("     ----AGENT----   ");
				getLabel(i++).setText("  Agent name:   " + agent.name);
				getLabel(i++).setText("  Team:   " + agent.teamName);
				getLabel(i++).setText("  Role:   " + agent.role);
				getLabel(i++).setText("  Status:   " + (agent.health == 0? "Disabled" : "Normal"));
				getLabel(i++).setText("  Energy:   " + agent.energy + "/" + agent.maxEnergy);
				getLabel(i++).setText("  Health:   " + agent.health + "/" + agent.maxHealth);
				getLabel(i++).setText("  Strenght:   " + agent.strength);
				getLabel(i++).setText("  Visibility range:   " + agent.visRange);
				getLabel(i++).setText("  Last action:   " + agent.lastAction);
				getLabel(i++).setText("  - Parameter:   " + agent.lastActionParam);
				getLabel(i++).setText("  - Result:   " + agent.lastActionResult);
			}
			if (team != null) {
				getLabel(i++).setText("   ");
				getLabel(i++).setText("     ----TEAM----   ");
				getLabel(i++).setText("  Team name:   " + team.name);
				getLabel(i++).setText("  Total score:   " + team.score);
				getLabel(i++).setText("  Step score:   " + team.stepScore);
				getLabel(i++).setText("  Zones score:   " + team.zonesScore);
				getLabel(i++).setText("  Current ach. pts:   " + team.achievementPoints);
				getLabel(i++).setText("  Used ach. pts:   " + team.usedAchievementPoints);
				getLabel(i++).setText("  ");
				if (team.achievements.size() > 0) {
					getLabel(i++).setText("  Achievements:");
					for (String ach : team.achievements) {
						getLabel(i++).setText("   * " + ach);
					}
				}
			}
			
			clearLabels(i);
			this.revalidate();
			this.repaint();
		}
		
		private JLabel getTeamColoredLabel(int i, TeamInfo team){
			return getColoredLabel(i,agentColors[getTeamNr(team.name)]);
		}
		
		private JLabel getColoredLabel(int i, Color color){
			JLabel l = getLabel(i);
			l.setForeground(color);
			return l;
		}
		
		private JLabel getLabel(int i){
			if (i < this.labels.size()){
				JLabel l = this.labels.get(i);
				l.setForeground(new Color(0,0,0));
				return l;
			}
			JLabel l = null;
			while (i >= this.labels.size()){
				l =  new JLabel("");
				l.setForeground(new Color(0,0,0));
				l.setAlignmentX(LEFT_ALIGNMENT);
				this.add(l);
				this.labels.add(l);
			}
			return l;
		}
		
		private void clearLabels(int i){
			while (i < this.labels.size()){
				this.labels.get(i).setText("");
				i++;
			}
		}
		
	}
	
	
	
	// Beginin of main class

	public GraphMonitor(String[] args){
		
		System.out.println("GraphMonitor [-rmihost <host>] [-rmiport <port>] [-savexmls [<folder>]]");
		
		for(int i = 0; i<args.length;i++){
			if(args[i].equalsIgnoreCase("-rmihost"))
				this.rmihost = args[i+1];
			if(args[i].equalsIgnoreCase("-rmiport"))
				this.rmiport = Integer.parseInt(args[i+1]);
			if(args[i].equalsIgnoreCase("-savexmls")){
				saveXMLs = true;
				if (i+1 < args.length && !args[i+1].startsWith("-")){
					xmlsFolder = args[i+1];
				}
			}
		}
		
		init();
		
	}
	
	public GraphMonitor(){
		init();
	}

	/**
	 * 
	 */
	protected void init() {
		this.setSize(800, 700);
		//this.createBufferStrategy(2);
		this.setTitle("Agent Contest 2011 (Prototype game)");
		worldView = new WorldView();

		worldView.setDoubleBuffered(true);		
		scrollPane = new JScrollPane(worldView);
		
		// New panel
		JPanel mainPanel = new JPanel(new BorderLayout(), true);
		mainPanel.add(scrollPane, BorderLayout.CENTER);
		
		// Upper Menu
		JPanel menuPanel = createUpperMenu();
		
		mainPanel.add(menuPanel, BorderLayout.NORTH);
		
		// Right menu
		infoPanel = new InfoPanel();
		infoPanel.setLayout(new BoxLayout(infoPanel,BoxLayout.Y_AXIS));
		worldView.addPropertyChangeListener("nodeSelected", infoPanel);
		worldView.addPropertyChangeListener("agentSelected", infoPanel);
		mainPanel.add(infoPanel, BorderLayout.EAST);
		
		
		this.add(mainPanel);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setVisible(true);
	}

	/**
	 * @return
	 */
	protected JPanel createUpperMenu() {
		JPanel menuPanel = new JPanel();
		menuPanel.setLayout(new BoxLayout(menuPanel,BoxLayout.X_AXIS));
		pauseButton = new JButton("Pause");
		pauseButton.addActionListener(this);
		pauseButton.setActionCommand("pause");
		menuPanel.add(pauseButton);
		
		menuPanel.add(Box.createRigidArea(new Dimension(25, 0)));
		
		addZoomButtons(menuPanel);
		return menuPanel;
	}

	/**
	 * @param menuPanel
	 */
	protected void addZoomButtons(JPanel menuPanel) {
		JButton button = new JButton("Zoom in");
		button.addActionListener(worldView);
		button.setActionCommand("zoomIn");
		menuPanel.add(button);
		
		button = new JButton("Zoom out");
		button.addActionListener(worldView);
		button.setActionCommand("zoomOut");
		menuPanel.add(button);
		
		menuPanel.add(Box.createRigidArea(new Dimension(10, 0)));
		
		button = new JButton("Fit to Window");
		button.addActionListener(worldView);
		button.setActionCommand("fitWindow");
		menuPanel.add(button);
	}
	
	private void runMonitor() {
		
		while (true) {
			try {
				this.searchService();
				while (true) {
					Document xmlDoc = getRMIObject(this.rmihost, this.rmiport,
							this.service);
					if (xmlDoc != null) {

						synchronized (paused) {

//							if (!paused.booleanValue()) {
								// Parse doc				
								this.parseXML(xmlDoc);

//								monitorStarted = true;
//							}
							
						}

					} else {
						parsedDoc = false;

						// this.runMonitor();
					}
					// worldView.repaint();
					// scrollPane.repaint();
					updateView();

					try {
						Thread.sleep(500);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			} catch (Exception e) {
				try {
					Thread.sleep(500);
				} catch (InterruptedException e2) {
					e.printStackTrace();
				}
//				teams = null;				
			}
		}
	}

	/**
	 * 
	 */
	protected void updateView() {
		infoPanel.update();
		worldView.repaint();
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
				try {
					Thread.sleep(3000);
				} catch (InterruptedException e) {				
					//e.printStackTrace();
				}
				//e1.printStackTrace();			
			}
		}		
	}
	
	public void parseXML(Document doc) {
		
		try {			
			String step;
			String simName;
			NodeList nl = doc.getElementsByTagName("state");
			Element e = (Element) nl.item(0);
			step = e.getAttribute("step");
			simName = e.getAttribute("simulation");
			
			Vector<NodeInfo> newNodes = new Vector<NodeInfo>();
			nl = doc.getElementsByTagName("vertex");
			for (int i = 0; i < nl.getLength(); i++) {
				Element e1 = (Element) nl.item(i);
				NodeInfo node = new NodeInfo();
				
				node.name = e1.getAttribute("name");
				node.gridX =  Integer.parseInt(e1.getAttribute("gridX"));
				node.gridY =  Integer.parseInt(e1.getAttribute("gridY"));
				node.x =  Integer.parseInt(e1.getAttribute("x"));
				node.y =  Integer.parseInt(e1.getAttribute("y"));
				node.weight =  Integer.parseInt(e1.getAttribute("weight"));
				if(e1.hasAttribute("dominatorTeam")){
					node.dominatorTeamName = e1.getAttribute("dominatorTeam");
				} else {
					node.dominatorTeamName = null;
				}
				node.dominatorTeam = getTeamNr(node.dominatorTeamName);
				
				node.agents = new Vector<AgentInfo>();
				NodeList agentsNl = e1.getFirstChild().getChildNodes();
				for (int j = 0; j < agentsNl.getLength(); j++) {
					Element agentEl = (Element) agentsNl.item(j);
					AgentInfo agent = new AgentInfo();
					
					agent.name = agentEl.getAttribute("name");
					agent.teamName = agentEl.getAttribute("team");
					agent.team = getTeamNr(agentEl.getAttribute("team"));
					agent.node = agentEl.getAttribute("node");
					agent.role = agentEl.getAttribute("roleName");
					agent.strength = Integer.parseInt(agentEl.getAttribute("strength"));
					agent.maxEnergy = Integer.parseInt(agentEl.getAttribute("maxEnergy"));
					agent.energy = Integer.parseInt(agentEl.getAttribute("energy"));
					agent.health = Integer.parseInt(agentEl.getAttribute("health"));
					agent.maxHealth = Integer.parseInt(agentEl.getAttribute("maxHealth"));
					agent.visRange = Integer.parseInt(agentEl.getAttribute("visRange"));
					agent.lastAction = agentEl.getAttribute("lastAction");
					agent.lastActionParam = agentEl.getAttribute("lastActionParam");
					agent.lastActionResult = agentEl.getAttribute("lastActionResult");
					node.agents.add(agent);
				}				
				newNodes.add(node);
			}
			
			Vector<EdgeInfo> newEdges = new Vector<EdgeInfo>();
			nl = doc.getElementsByTagName("edge");
			for (int i = 0; i < nl.getLength(); i++) {
				Element e1 = (Element) nl.item(i);
				EdgeInfo edge = new EdgeInfo();
				
				edge.weight =  Integer.parseInt(e1.getAttribute("weight"));
				edge.node1 = searchNode(e1.getAttribute("node1"), newNodes);
				edge.node2 = searchNode(e1.getAttribute("node2"), newNodes);
				
				newEdges.add(edge);
			}
			
			Vector<TeamInfo> currentTeams = new Vector<TeamInfo>();
			nl = doc.getElementsByTagName("team");
			for (int i = 0; i < nl.getLength(); i++) {
				Element e1 = (Element) nl.item(i);
				TeamInfo team = new TeamInfo();
				
				team.name = e1.getAttribute("name");
				team.score =  Integer.parseInt(e1.getAttribute("score"));
				team.achievementPoints =  Integer.parseInt(e1.getAttribute("achievementPoints"));
				team.usedAchievementPoints =  Integer.parseInt(e1.getAttribute("usedAchievementPoints"));
				team.stepScore =  Integer.parseInt(e1.getAttribute("stepScore"));
				team.zonesScore =  Integer.parseInt(e1.getAttribute("zonesScore"));
				
				team.achievements = new Vector<String>();
//				NodeList agentsNl = e1.getFirstChild().getChildNodes();
				NodeList nodeList = e1.getElementsByTagName("achievements");
				if (nodeList.getLength() > 0){
					nodeList = nodeList.item(0).getChildNodes();
					for (int j = 0; j < nodeList.getLength(); j++) {
						Element agentEl = (Element) nodeList.item(j);
						String name = agentEl.getAttribute("name");					
						team.achievements.add(name);
					}
				}
				
				////////
				team.provedNodes = new Vector<String>();
				nodeList = e1.getElementsByTagName("provedNodes");
				if (nodeList.getLength() > 0){
					nodeList = nodeList.item(0).getChildNodes();
					for (int j = 0; j < nodeList.getLength(); j++) {
						Element agentEl = (Element) nodeList.item(j);
						String name = agentEl.getAttribute("name");					
						team.provedNodes.add(name);
					}
				}
				////////
				
				currentTeams.add(team);
			}
			
			Collections.sort(currentTeams, new Comparator<TeamInfo>(){

				@Override
				public int compare(TeamInfo o1, TeamInfo o2) {
					if (o1.score > o2.score){
						return -1;
					} else if (o1.score < o2.score){
						return +1;
					} else {
						return o1.name.compareTo(o2.name);
					}
				}
				
			});
			
			if (!paused.booleanValue()) {
				synchronized (syncObject) {
					nodes = newNodes;
					edges = newEdges;
					teamsInfo = currentTeams;
					simStep = step;
					parsedDoc = true;
					simId = simName;
				}
			}
			if (saveXMLs){
				try {
					TransformerFactory tFactory = TransformerFactory.newInstance();
					Transformer transformer = tFactory.newTransformer();
					DOMSource source = new DOMSource(doc);
					File f = new File(xmlsFolder, simName+File.separator);
					if (!f.exists()){
						f.mkdirs();
					}
					f = new File(xmlsFolder, simName+File.separator+ simName + "_" + step + ".xml");
					if (!f.exists()){
						f.createNewFile();
					}
					StreamResult result = new StreamResult(f);
					transformer.transform(source, result);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
			
			
		} catch (Exception e) {
			parsedDoc = false;
			e.printStackTrace();
		}
	}
	
	public int getTeamNr(String name) {
		if (name == null || "".equals(name)) { return -1; }
		if (teams == null ) { teams = new Vector<String>(); }
		if (teams.indexOf(name) == -1) {
			
			teams.add(name); 
		
		
		}
		return teams.indexOf(name);
	}
	
	private NodeInfo searchNode(String name, Vector<NodeInfo> nodes) {
		for (NodeInfo node: nodes){
			if(node.name.equals(name)){ return node; }
		}
		return null;
	}
	
	private AgentInfo searchAgent(String name, Vector<NodeInfo> nodes) {
		for (NodeInfo node: nodes){
			for (AgentInfo agent: node.agents){
				if(agent.name.equals(name)){ return agent; }
			}
		}
		return null;
	}
	
	/*
	private AgentInfo searchAgent(String name, Vector<AgentInfo> agents) {
		for (AgentInfo agent: agents){
			if(agent.name.equals(name)){ return agent; }
		}
		return null;
	}
	*/
	
	private TeamInfo searchTeam(String name, Vector<TeamInfo> teams) {
		if (name == null || teams == null){
			return null;
		}
		for (TeamInfo team: teams){
			if(team.name.equals(name)){ return team; }
		}
		return null;
	}
	
	/**
	 * 
	 * @param host
	 * @param port
	 * @param service
	 * @throws RemoteException 
	 * @throws NotBoundException 
	 */
	protected Document getRMIObject(String host, int port, String service) throws RemoteException, NotBoundException {
		Document	xmlDoc = null;
		try {
			simulationRunning = true;
			Registry registry = LocateRegistry.getRegistry(host,port); 
			XMLDocumentServer serverState = (XMLDocumentServer) registry.lookup(service);
			try {
				xmlDoc = serverState.getXMLDocument();
			} catch (NullPointerException e) {
				simulationRunning = false;
				throw new RemoteException(
						"NullPointerException while trying to get XMLDocument",
						e);

			}
		} catch (RemoteException e) {
			System.err.println("Currently no simulation running on " + e + " " + host
					+ " " + port + "...\n");
			throw e;

		} catch (NotBoundException e) {
			System.err.println("Currently no simulation running on " + e + " " + host
					+ " " + port + "...\n");
			throw e;
		}
		return xmlDoc;
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String args[]) {
		GraphMonitor graph = new GraphMonitor(args);
		graph.runMonitor();

	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if ("pause".equals(e.getActionCommand())){			
			synchronized (paused) {
				if (paused.booleanValue()){
					pauseButton.setText("pause");
					paused = false;
				} else {
					pauseButton.setText("resume");
					paused = true;
				}
			}
		}
	}
	
}
