package massim.server;


import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.log;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import massim.framework.AgentManager;
import massim.framework.AgentProviderAgentManager;
import massim.framework.ArrayAgentProvider;
import massim.framework.Component;
import massim.framework.TeamAgentFilter;
import massim.framework.backup.BackupReader;
import massim.framework.backup.BackupWriter;
import massim.framework.connection.InetSocketListener;
import massim.framework.connection.UsernamePasswordSocketLoginManager;
import massim.framework.rmi.RMI_Infor;
import massim.framework.rmi.XMLDocumentServer;
import massim.framework.simulation.AbstractSimulation;
import massim.framework.util.XMLUtilities;
import massim.test.InvalidConfigurationException;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


public class Server extends AbstractServer {
	public final int AGENT_PORT_DEFAULT=12300; 
	public final int AGENT_BACKLOG_DEFAULT=10;
	private String serverstatus ="NOTCONFIGURED";//, CONFIGURED, SIMSTART,SIMSTOP, SIMEND
	public static Document backup_serverconfig=null  ;
	public static String backuppath = "";
	public static String recoveryDir=backuppath+BackupWriter.file_sep+"recovery_serverconfig";
	public static String recoverFile="recovery_serverconfig.xml";
	public static String recoverstep="";
	public static Map<String, int[]> agent_pos=null;
	
	public int  score[];
	protected InetSocketListener socketlistener;
	protected ServerInetSocketListener serverinetsocketlistener;
	protected ServerSimulationAgents serversimulationagents;
	protected UsernamePasswordSocketLoginManager loginsocketmanager;
	protected ArrayAgentProvider arrayagentprovider;
	protected AgentManager agentmanager;
	protected TeamAgentFilter teamagentfilter;
	protected Registry rmiregistry;
	protected RMIServerStatus rmiinfoserver2;
	protected RMITournamentServer rmitournamentserver;
	protected Document xmlTournamentReport; 
	protected File xmlTournamentReportFile;
	protected List<String> manual;

	protected String tournamentname;
	
	protected LaunchSync launchSync;
	
	protected int tournamentmode;
	protected BackupReader reader;
	public HashMap<String, List<String>> team_member = new HashMap<String, List<String>>();
	
	interface LaunchSync {
		void waitForStart();
	}
	
	class TimerLaunchSync implements LaunchSync {
		long time;
		public TimerLaunchSync(Element e) {
			time = Long.parseLong(e.getAttribute("time-to-launch"));
		}
		public void waitForStart() {
			try {
				Thread.sleep(time);
			} catch (InterruptedException e) {}
		}
	}
	
	class KeyLaunchSync implements LaunchSync {
		public void waitForStart() {
			try {System.in.read();} catch (IOException e) {}
		}
	}
	
	private class RMITournamentServer extends UnicastRemoteObject implements XMLDocumentServer {
		private static final long serialVersionUID = 6468903192414320442L;

		public RMITournamentServer() throws RemoteException {
			super();
		}

		public Document getXMLDocument() {
			return xmlTournamentReport;//FIXME: missing synchronization
		}
	}
	public Server(){
		serverstatus="NOTCONFIGURED";
	}

	public Server(String[] args) throws InvalidConfigurationException {
		Element conf =parseCommandLineToConfig(args);
		this.config(conf);
		serverstatus = "CONFIGURED";
			}
	
public void config(Element conf) throws InvalidConfigurationException {
		
		Document doc = this.cloneServerConfigFile(conf);
		
		Element xmlconfiguration = doc.getDocumentElement(); 
		
		backup_serverconfig = this.cloneServerConfigFile(xmlconfiguration);
		
		backuppath= xmlconfiguration.getAttribute("backuppath");
		recoveryDir=backuppath+BackupWriter.file_sep+"recovery_serverconfig";
		recoverstep=xmlconfiguration.getAttribute("recoverstep");
		
		if(!recoverstep.equalsIgnoreCase("")){
			agent_pos = this.createAgent_Pos_Map(xmlconfiguration);
			
		}
		this.createTeamInfor(xmlconfiguration);
		
		log(LOGLEVEL_NORMAL,"Server launched");
		tournamentname = xmlconfiguration.getAttribute("tournamentname");
		tournamentmode = Integer.parseInt(xmlconfiguration.getAttribute("tournamentmode"));
		//create socket listener
		serverinetsocketlistener = new ServerInetSocketListener((Element)XMLUtilities.getChildsByTagName(xmlconfiguration, "simulation-server").item(0));
		socketlistener = serverinetsocketlistener.object;
		//read rmiport and host
	
		this.readRMIinfor(xmlconfiguration);
		
		
		//read account list
		NodeList nl=XMLUtilities.getChildsByTagName(xmlconfiguration, "accounts");
		serversimulationagents = new ServerSimulationAgents((Element) nl.item(0)); 
		
		nl = XMLUtilities.getChildsByTagName(xmlconfiguration,"match");
		for (int i=0;i<nl.getLength();i++) System.out.println("node:"+nl.item(i));
		if (nl.getLength()!=1) {
			log(LOGLEVEL_CRITICAL,"simulation configuration invalid");
			System.exit(1);
		}
		el_match = (Element) nl.item(0);
		
		//create UsernamePasswordSocketLoginManager 
		loginsocketmanager = new UsernamePasswordSocketLoginManager(serversimulationagents.accounts,serversimulationagents.accountSocketHandlerMap);

		//connect loginsocketmanager with socketlistener
		socketlistener.setSocketHandler(loginsocketmanager);
		
		//create arrayagentprovider
		arrayagentprovider = new ArrayAgentProvider(serversimulationagents.agents);

		//create team filter
		teamagentfilter = new TeamAgentFilter(serversimulationagents.accountTeamMap,arrayagentprovider);
		
		//create agent manager, based on teamagentfilter
		agentmanager = new AgentProviderAgentManager(teamagentfilter);
		
		//create launch sync
		String launchsynctype = xmlconfiguration.getAttribute("launch-sync-type");
		if (launchsynctype.equalsIgnoreCase("key")) {
			launchSync = new KeyLaunchSync();
		} else if (launchsynctype.equalsIgnoreCase("timer")) {
			launchSync = new TimerLaunchSync(xmlconfiguration);
		}
		xmlTournamentReportFile = new File(xmlconfiguration
				.getAttribute("reportpath"), tournamentname + "_report.xml");

		if (this.tournamentmode == 2) {

			manual = new LinkedList<String>();

			try {

				nl = XMLUtilities.getChildsByTagName(xmlconfiguration,
						"manual-mode");
				Element el_match = (Element) nl.item(0);
				NodeList matches = el_match.getElementsByTagName("match");

				for (int i = 0; i < matches.getLength(); i++) {

					Element match = (Element) matches.item(i);
					manual.add(match.getAttribute("team1") + "VS"
							+ match.getAttribute("team2"));
				}
			}

			catch (Exception e) {

				log(LOGLEVEL_CRITICAL, "manual mode configuration invalid");
				System.exit(1);
			}
			;
		}
		serverstatus = "CONFIGURED";
	}
private void createTeamInfor(Element xmlconfiguration) {
	
	 team_member = new HashMap<String, List<String>>();
	NodeList nl = xmlconfiguration.getElementsByTagName("account");

	for(int i = 0; i< nl.getLength(); i++){
		Element el = (Element) nl.item(i);
		String team = el.getAttribute("team");
		
		if(!team_member.containsKey(team)){
			List<String> member = new Vector<String>();
			team_member.put(team, member);
		}
		List<String> member = team_member.get(team);
		
		member.add(el.getAttribute("username"));
	}
}

/**
 * this method will be used with recovery mode
 * the method creates a Map for agent's username and agent's position
 * the dispensation of agent's position does not depend on connection time
 * of agent but it depends on username of agent
 * also:
 * Agent'username A--> posx0,posy0
 * Agent'username B--> posx1,posy1
 * 
 * And not:
 * Agent B connect firstly -->posx0,posy0
 * Agent B connect secondly --> posx1,posy1 
 * @param xmlconfiguration
 * @return
 */
	private Map<String, int[]> createAgent_Pos_Map(Element xmlconfiguration) {
		
		Map<String, int[]> agent_pos = new HashMap<String, int[]>();
		NodeList accounts = xmlconfiguration.getElementsByTagName("account");
		NodeList nl = xmlconfiguration.getElementsByTagName("array");
		Element agent_posx = null;
		Element agent_posy = null;
		for(int i = 0 ; i< nl.getLength();i++){
			Element e = (Element) nl.item(i);
			String meta_name = e.getAttribute("meta:name");
			if(meta_name.equalsIgnoreCase("agentPositionX")){
				agent_posx = e;
			}
			else if (meta_name.equalsIgnoreCase("agentPositionY")){
				agent_posy = e;
			}
		}
		for(int i  = 0 ; i<accounts.getLength();i++){
			Element account = (Element) accounts.item(i);
			String username = account.getAttribute("username");
			int[] posx_y = new int[2];
			posx_y[0] = Integer.parseInt(agent_posx.getAttribute("item"+i));
			posx_y[1] = Integer.parseInt(agent_posy.getAttribute("item"+i));
			agent_pos.put(username, posx_y);
		}
	return agent_pos;
}
/**
 * read informations for rmi service from conf element and save them in RMI_Infor.java    
 * @param conf in xml format 
 */
	private void readRMIinfor(Element conf) {
//			NodeList nl  = XMLUtilities.getChildsByTagName(conf, "visualization");
//			Element visualization = (Element) nl.item(0);
//			RMI_Infor.FLASH_SERVER = visualization.getAttribute("flashserver");
//			if(!RMI_Infor.FLASH_SERVER.equals("")) RMI_Infor.FLASH_SERVER_ACTIVATED=true;
//			RMI_Infor.FLASH_PORT = Integer.parseInt(visualization.getAttribute("flashport"));
//			RMI_Infor.FLASH_SERVICE = visualization.getAttribute("flashservice");
//			RMI_Infor.RMI_URL_DEFAULT = "rmi://"+RMI_Infor.RMI_HOST_DEFAULT+":"+RMI_Infor.RMI_PORT_DEFAULT+"/";
		
		NodeList nl = conf.getElementsByTagName("simulation");
		Element simulation=(Element) nl.item(0);
		RMI_Infor.RMI_HOST_DEFAULT = simulation.getAttribute("rmixmlobsserverhost");
		RMI_Infor.RMI_PORT_DEFAULT = Integer.parseInt(simulation.getAttribute("rmixmlobsserverport"));
	}

	protected ServerSimulationRun2 sr;
	protected Element el_match;
	private String[] team;
	private HashMap<String, Integer> team_score=null;
	
	public void runMatch(Element el_match, Map<String, String> teammap, String name, Node statmatchparent) throws InvalidConfigurationException {
		NodeList nl = XMLUtilities.getChildsByTagName(el_match,"simulation");

		Document statdoc = statmatchparent.getOwnerDocument();
		
		Element el_statmatch=statdoc.createElement("match");
		
		for (String a : teammap.keySet()) {
			el_statmatch.setAttribute(a,teammap.get(a));
		}
		
		statmatchparent.appendChild(el_statmatch);
		for (int i=0;i<nl.getLength();i++) {
			
			Element simuconfig = (Element) nl.item(i);
			String simname = tournamentname+"_";
			String[] teams=teammap.values().toArray(new String[teammap.size()]);
			for (int j=0;j<teams.length;j++) {
				simname += teams[j];
			}
			simname +="_"+simuconfig.getAttribute("id");
			sr=new ServerSimulationRun2(simuconfig,teammap,simname,tournamentname);
			sr.setAgentmanager(agentmanager);
			long starttime = System.currentTimeMillis();
			
			sr.runSimulation();
			long endtime = System.currentTimeMillis();
			Element n = sr.xmlstatisticsobserver.getDocument().getDocumentElement();
			Element el_statsim = statdoc.createElement("simulation");
			Element el_simresult = statdoc.createElement("result");
			
			el_statsim.setAttribute("starttime",Long.toString(starttime));
			el_statsim.setAttribute("endtime",Long.toString(endtime));
			el_statsim.setAttribute("name",simname);
			
			el_statsim.appendChild(el_simresult);
			
			el_statmatch.appendChild(el_statsim);
			el_statmatch.getOwnerDocument().adoptNode(n);
			
			for (int j=0;j<n.getAttributes().getLength();j++) {
				Attr a = (Attr) n.getAttributes().item(j);
				el_simresult.setAttribute(a.getName(),a.getValue());
			}
			for (int j=0;j<n.getChildNodes().getLength();j++) {
				el_simresult.appendChild(n.getChildNodes().item(j));
			}
		}
	}
	public int[] runMatch(Element el_match, Map<String, String> teammap,
			String name, Node statmatchparent, String team1Name,
			String team2Name) throws InvalidConfigurationException {

		NodeList nl = XMLUtilities.getChildsByTagName(el_match, "simulation");
		Document statdoc = statmatchparent.getOwnerDocument();
		Element el_statmatch = statdoc.createElement("match");

		int[] score = { 0, 0 };

		for (String a : teammap.keySet()) {
			el_statmatch.setAttribute(a, teammap.get(a));
		}

		statmatchparent.appendChild(el_statmatch);

		for (int i = 0; i < nl.getLength(); i++) {
			Element simuconfig = (Element) nl.item(i);
			String simname = tournamentname + "_";
			String[] teams = teammap.values().toArray(
					new String[teammap.size()]);

			for (int j = 0; j < teams.length; j++) {
				simname += teams[j];
			}

			simname += "_" + simuconfig.getAttribute("id");
			sr = new ServerSimulationRun2(simuconfig, teammap, simname,tournamentname);
			sr.setAgentmanager(agentmanager);

			long starttime = System.currentTimeMillis();
			String winner = sr.runSimulation();
			long endtime = System.currentTimeMillis();

			if (winner == team1Name) {
				score[0] += 3;
			}
			if (winner == team2Name) {
				score[1] += 3;
			}
			if (winner == "draw") {
				score[0] += 1;
				score[1] += 1;
			}

			System.out.println("::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
			System.out.println(":: Winner of " + simname + " : " + winner);
			System.out.println(":: " + team1Name + ": " + score[0]);
			System.out.println(":: " + team2Name + ": " + score[1]);
			System.out.println("::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::");
		
			

			if (sr.xmlstatisticsobserver != null) {
				Element n = sr.xmlstatisticsobserver.getDocument().getDocumentElement();
				Element el_statsim = statdoc.createElement("simulation");
				Element el_simresult = statdoc.createElement("result");

				el_statsim.setAttribute("starttime", Long.toString(starttime));
				el_statsim.setAttribute("endtime", Long.toString(endtime));
				el_statsim.setAttribute("name", simname);

				el_statsim.appendChild(el_simresult);

				el_statmatch.appendChild(el_statsim);
				el_statmatch.getOwnerDocument().adoptNode(n);

				for (int j = 0; j < n.getAttributes().getLength(); j++) {
					Attr a = (Attr) n.getAttributes().item(j);
					el_simresult.setAttribute(a.getName(), a.getValue());
				}

				for (int j = 0; j < n.getChildNodes().getLength(); j++) {
					el_simresult.appendChild(n.getChildNodes().item(j));
				}
				
			}
			

			
		}

		return score;
	}

	
	public void run() throws InvalidConfigurationException {
		//initialize xml report
		this.createRMI();
		
		serverstatus = "SIMSTART";
		score = new int[serversimulationagents.teams.length];
		team = new String[serversimulationagents.teams.length];
		team_score = new HashMap<String, Integer>();
		
		for (int i = 0; i < serversimulationagents.teams.length; i++) {
			team[i] = serversimulationagents.teams[i];
			team_score.put(team[i], score[i]);
		}
		
		try {
			xmlTournamentReport = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
		} catch (ParserConfigurationException e) {
			throw new RuntimeException(e);
		}
		Element report_el_root = xmlTournamentReport.createElement("tournament");
		report_el_root.setAttribute("tournament-name", tournamentname);
		

		for (int i = 0; i < serversimulationagents.teams.length; i++) {	
			Element el_team = xmlTournamentReport.createElement("team");
			el_team.setAttribute("name",serversimulationagents.teams[i]);
			report_el_root.appendChild(el_team);
		}
		
		xmlTournamentReport.appendChild(report_el_root); 
		
		agentmanager.start();
		
		for (int i=0;i<serversimulationagents.agents.length;i++) 
			((Component)serversimulationagents.agents[i]).start();
		
		socketlistener.start();
		//start RMI server info component
		
		try {
			//create RMIInfoServer
			rmiinfoserver2 = new RMIServerStatus(serversimulationagents);
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.rebind("server2",rmiinfoserver2);
		
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"Error: couldn't bind RMIInfoServer");
			e.printStackTrace();
			System.exit(1);
		}

		try { 
			//create RMIXMLServer
			rmitournamentserver = new RMITournamentServer();
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			r.rebind("statistics",rmitournamentserver);
		
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"Error: couldn't bind RMIInfoServer");
			e.printStackTrace();
			System.exit(1);
		} 

		// launch synchronization
		launchSync.waitForStart();
		switch (tournamentmode) {
		case 0:
			
			/*
			 * Tournament mode: each team plays against each other team
			 */
			for (int t1 = 0; t1 < serversimulationagents.teams.length; t1++) {
				for (int t2 = t1 + 1; t2 < serversimulationagents.teams.length; t2++) {

					log(LOGLEVEL_NORMAL, "now playing: "
							+ serversimulationagents.teams[t1] + " vs "
							+ serversimulationagents.teams[t2]);
					Map<String, String> m = new HashMap<String, String>();
					m.put("red", serversimulationagents.teams[t1]);
					m.put("blue", serversimulationagents.teams[t2]);
					
					int[] a = runMatch(el_match, m,
							serversimulationagents.teams[t1] + "_VS_"
									+ serversimulationagents.teams[t2],
							report_el_root, serversimulationagents.teams[t1],
							serversimulationagents.teams[t2]);
					
					score[t1] += a[0];
					score[t2] += a[1];
				//	this.writeResult(t1,t2,a,score,"red","blue");
					team_score.put(serversimulationagents.teams[t1], score[t1]);
					team_score.put(serversimulationagents.teams[t2], score[t2]);
					try {
						TransformerFactory.newInstance().newTransformer()
								.transform(new DOMSource(xmlTournamentReport),
										new StreamResult(System.out));
					} catch (Exception e) {
					}
				}
			}
			break;
		case 1: {
			/*
			 * Testing phase mode: each team against the Bot
			 */
			int t2 = 0;
			for (int i = 0; i < serversimulationagents.teams.length; i++)
				if (serversimulationagents.teams[i].equalsIgnoreCase("TUCBot"))
					t2 = i;
			for (int t1 = 0; t1 < serversimulationagents.teams.length; t1++) {
				// search bot team
				if (t1 == t2)
					continue;
				log(LOGLEVEL_NORMAL, "now playing: "
						+ serversimulationagents.teams[t1] + " vs "
						+ serversimulationagents.teams[t2]);
				Map<String, String> m = new HashMap<String, String>();
				m.put("red", serversimulationagents.teams[t1]);
				m.put("blue", serversimulationagents.teams[t2]);

				int[] a = runMatch(el_match, m,
						serversimulationagents.teams[t1] + "_VS_"
								+ serversimulationagents.teams[t2],
						report_el_root, serversimulationagents.teams[t1],
						serversimulationagents.teams[t2]);
				score[t1] += a[0];
				score[t2] += a[1];

				team_score.put(serversimulationagents.teams[t1], score[t1]);
				team_score.put(serversimulationagents.teams[t2], score[t2]);
				try {
					TransformerFactory.newInstance().newTransformer().transform(new DOMSource(xmlTournamentReport),new StreamResult(System.out));
				} catch (Exception e) {
				}
			}
		}
		
		break;
	
		case 2: {

			for (int i = 0; i < manual.size(); i++) {

				String teamsTogether[] = manual.get(i).split("VS");
				String team1 = teamsTogether[0];
				String team2 = teamsTogether[1];

				int t1 = findTeam(team1);
				int t2 = findTeam(team2);

				log(LOGLEVEL_NORMAL, "now playing: "
						+ serversimulationagents.teams[t1] + " vs "
						+ serversimulationagents.teams[t2]);
				Map<String, String> m = new HashMap<String, String>();
				m.put("red", serversimulationagents.teams[t1]);
				m.put("blue", serversimulationagents.teams[t2]);

				runMatch(el_match, m, serversimulationagents.teams[t1] + "_VS_"
						+ serversimulationagents.teams[t2], report_el_root);

				try {
					TransformerFactory.newInstance().newTransformer()
							.transform(new DOMSource(xmlTournamentReport),
									new StreamResult(System.out));
				} catch (Exception e) {
				}

			}
		}
		
			break;
		}
	

		
		socketlistener.stop();
		for (int i=0;i<serversimulationagents.agents.length;i++) 
			((Component)serversimulationagents.agents[i]).stop();
		agentmanager.stop();
		
		//write result to file
		writeTournamentReportToFile();
		this.unbindRMI();

		serverstatus = "SIMEND";
	}
	
	/*
	private void writeResult(int t1, int t2, int[] a, int[] score2,
			String red, String blue) {
		 try{
			    // Create file 
			 String team1 = serversimulationagents.teams[t1];
			 String team2 = serversimulationagents.teams[t2];
			 
			    FileWriter fstream = new FileWriter(team1+team2+tournamentname+".txt");
			    String s = "Team: "+team1+" VS "+"Team: "+team2;
			     s += "\n"+a[t1]+" Total: "+score[t1]+"\n"+a[t2]+" Total: "+score[t2]+"\n";
			    
			    BufferedWriter out = new BufferedWriter(fstream);
			    out.write(s);
			    //Close the output stream
			    out.close();
			    }catch (Exception e){//Catch exception if any
			      System.err.println("Error: " + e.getMessage());
			    }
		
	}

*/
	private void unbindRMI() {
		try {
			Registry r = LocateRegistry.getRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			String[] rmiservices = r.list();
		for(int i=0 ; i< rmiservices.length;i++ ){
		//	if(!rmiservices[i].equalsIgnoreCase("SPSSERVER")){
				r.unbind(rmiservices[i]);
				log(LOGLEVEL_NORMAL,"Unbind rmiservice: "+rmiservices[i]);
				
		//	}
		}
		UnicastRemoteObject.unexportObject(rmitournamentserver, true);
		UnicastRemoteObject.unexportObject(this.rmiinfoserver2, true);
		
	
		//	r.unbind("statistics");
			rmitournamentserver = null;
			
	//		r.unbind("server2");
	//		rmiinfoserver2 = null;
		
		} catch (RemoteException e) {
			log(LOGLEVEL_ERROR,"Error: couldn't unbind RMIInfoServer");
			e.printStackTrace();
			System.exit(1);
		} catch (NotBoundException e) {
			log(LOGLEVEL_ERROR,"Error: couldn't unbind RMIInfoServer");
			e.printStackTrace();
			System.exit(1);
		}

	}

	private void createRMI() {
		try {
			LocateRegistry.createRegistry(RMI_Infor.RMI_PORT_DEFAULT);
			log(LOGLEVEL_NORMAL,"Create rmiregistry on port  "+RMI_Infor.RMI_PORT_DEFAULT);
		} catch (RemoteException e1) {
		//	e1.printStackTrace();
		//	System.exit(0);
			log(LOGLEVEL_NORMAL,"rmi existed on port:  "+RMI_Infor.RMI_PORT_DEFAULT);
		}
		
	}

	private int findTeam(String name) {
		int index = 0;
		for (int i = 0; i < serversimulationagents.teams.length; i++) {
			if (serversimulationagents.teams[i].equals(name)) {
				index = i;
				break;
			}
		}
		return index;
	}

	private void writeTournamentReportToFile() throws InvalidConfigurationException {
		try {
			FileOutputStream out = new FileOutputStream(xmlTournamentReportFile);
			Transformer transformer = TransformerFactory.newInstance().newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT,"yes");
			transformer.transform(new DOMSource(xmlTournamentReport), new StreamResult(out));
			
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			throw new InvalidConfigurationException(e1);
		} catch (TransformerException e2) {
			e2.printStackTrace();
			throw new InvalidConfigurationException(e2);
		}
	}

	public static void main(String[] args) throws InvalidConfigurationException {
		Server server = new Server(args);	
		server.run();
		
	}

	
	private Document cloneServerConfigFile(Element xmlconfiguration) {
		Document clonedoc = null;
		try {
			Document c = xmlconfiguration.getOwnerDocument();
			
			File a = new File(".tmp_serverconfig.xml");
			try {
				FileOutputStream out = new FileOutputStream(a);
				Transformer transformer = TransformerFactory.newInstance().newTransformer();
				transformer.setOutputProperty(OutputKeys.INDENT, "yes");
				transformer.transform(new DOMSource(c), new StreamResult(out));
		
				try {
					DocumentBuilderFactory dbfactory =DocumentBuilderFactory.newInstance();
					dbfactory.setNamespaceAware(true);
					clonedoc = dbfactory.newDocumentBuilder().parse(a);
				
				} catch (SAXException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			catch (FileNotFoundException e1) {
				e1.printStackTrace();
			}
			catch (TransformerException e2) {
				e2.printStackTrace();
			}
		} catch (ParserConfigurationException e1) {
			e1.printStackTrace();
		}
		return clonedoc;
	}


	int stoppedstep = 0;
	public boolean stopped = false;
	
	public void startSimulation() {
		
		if(serverstatus.equalsIgnoreCase("CONFIGURED"))
			try {
				stopped = false;
				run();
			} catch (InvalidConfigurationException e) {	
				e.printStackTrace();
			}
			
		else log(LOGLEVEL_ERROR,"server needs configured before running ");
	}
	public synchronized void stopSimulation() {
		if(sr != null){
		AbstractSimulation sim = (AbstractSimulation) sr.getSimulation();
		stoppedstep = sim.getSteps();
		stopped = true;
		sim.setSteps(Integer.MAX_VALUE);
		}
	}
	public synchronized String getServerstatus(){return serverstatus;}
	
	public synchronized int getStep(){
		int step = stoppedstep;
		if(sr != null && !stopped){
			AbstractSimulation simulation = (AbstractSimulation) sr.getSimulation();
			step = simulation.getSteps();
		}
		return step;
	}
	public synchronized HashMap<String, Integer>getTeam_Score(){
	
		return team_score;
	}
}
