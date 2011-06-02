package massim.framework.connection;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class XMLCommunicationUtilities {

	static public Document createDefaultMessage(String messagetype) {
		DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
		DocumentBuilder documentbuilder=null;
		try {
			documentbuilder=factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {throw new RuntimeException(e);}

		Document doc = documentbuilder.newDocument();
		Element root = doc.createElement("message");
		doc.appendChild(root);
		long timestamp = System.currentTimeMillis();
		root.setAttribute("timestamp",Long.toString(timestamp));
		root.setAttribute("type",messagetype);
		return doc;
	}
}
