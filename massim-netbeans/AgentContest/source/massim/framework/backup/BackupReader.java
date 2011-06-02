package massim.framework.backup;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class BackupReader {

	HashMap<String, Integer> map = null;
	String rootDirectory;

	/**
	 * Constructor
	 */
	public BackupReader(String path) {

		this.rootDirectory = path;
		map = new HashMap<String, Integer>();
	}

	/**
	 * parse an xml
	 * 
	 * @param doc
	 * @param myFile
	 * @return the new File
	 */
	public static Document openFile( String myFile) {
		Document doc ;
		DocumentBuilderFactory factory;
		DocumentBuilder builder = null;
		factory = DocumentBuilderFactory.newInstance();

		try {
			builder = factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		builder.setErrorHandler(new org.xml.sax.ErrorHandler() {
			public void warning(SAXParseException arg0) throws SAXException {
				// TODO Auto-generated method stub
			}

			public void error(SAXParseException arg0) throws SAXException {
				// TODO Auto-generated method stub
			}

			public void fatalError(SAXParseException arg0) throws SAXException {
				// TODO Auto-generated method stub
			}
		});
		try {
			doc = builder.parse(new File(myFile));
			return doc;
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			//doc = generateXML();
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * 
	 */
	public void restore() {

		File file = new File(rootDirectory);

		String path = "";

		for (File f : file.listFiles()) {

			if (f.length() > 0) {
				File files[] = f.listFiles();
				int length = files.length;

				try {
					path = files[length - 1].getCanonicalPath();
				} catch (IOException e) {
				}
				;

				countPoints(path);
			}
		}
	}

	/**
	 * 
	 */
	public void countPoints(String path) {

		File file = new File(path);
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

		NodeList nl1 = root.getElementsByTagName("team1");
		NodeList nl2 = root.getElementsByTagName("team2");

		Element e1 = (Element) nl1.item(0);
		Element e2 = (Element) nl2.item(0);

		String team1 = e1.getAttribute("name");
		String team2 = e2.getAttribute("name");
		String score1 = e1.getAttribute("score");
		String score2 = e2.getAttribute("score");

		if (map.containsKey(team1)) {

			map.remove(team1);
			map.put(team1, Integer.parseInt(score1));
		}

		else {

			map.put(team1, Integer.parseInt(score1));
		}

		if (map.containsKey(team2)) {

			map.remove(team2);
			map.put(team2, Integer.parseInt(score2));
		}

		else {

			map.put(team2, Integer.parseInt(score2));
		}
	}

	/**
	 * 
	 * @return Map wit points and teams
	 */
	public HashMap<String, Integer> getStatistik() {

		return this.map;
	}

	/**
	 * Print of table with results
	 */
	public void printTable() {

		Set<String> keys = map.keySet();
		Iterator<String> iterator = keys.iterator();

		System.out.println("Team" + "\t" + "\t" + "Score");

		while (iterator.hasNext()) {

			String team = iterator.next();
			int score = map.get(team);

			if (team.length() < 6) {

				System.out.println(team + "\t" + "\t" + score);
			}

			else {

				System.out.println(team + "\t" + score);
			}
		}
	}
}
