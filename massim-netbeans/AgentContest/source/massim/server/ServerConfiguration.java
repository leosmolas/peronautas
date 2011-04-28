package massim.server;


import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public class ServerConfiguration {
	private DocumentBuilder documentbuilder;
	private Element el_serverconfig;
	public ServerConfiguration(String configdir) {
		DocumentBuilderFactory dbfactory=DocumentBuilderFactory.newInstance();
		try {
			documentbuilder=dbfactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		File f = new File(configdir,"config.xml");
		Document doc = null;
		try {
			doc = documentbuilder.parse(f);
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		Element el_config=(Element)doc.getElementsByTagName("config").item(0);
		el_serverconfig=(Element)el_config.getElementsByTagName("server").item(0);
	}
	public Element getRoot() {
		return el_serverconfig;
	}
}
