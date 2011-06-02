package massim.visualization.svg;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.batik.dom.svg.SVGDOMImplementation;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class SvgXmlFile {

	protected String svgEnding = ".svg";

	/**
	 * generate a XML-File
	 * 
	 * @param return
	 *            genertated document
	 */
	public Document generateXML() {

		DOMImplementation impl = SVGDOMImplementation.getDOMImplementation();
		// DOMImplementation impl =
		// DOMImplementationImpl.getDOMImplementation();

		// we are using a constant available on the SVGDOMImplementation
		// but we could have used "http://www.w3.org/2000/svg"

		String svgNS = SVGDOMImplementation.SVG_NAMESPACE_URI;
		Document doc = impl.createDocument(svgNS, "svg", null);
		// for scale the output svg
		// create a group and add there everything
		
		Node root = doc.getDocumentElement();
		Element group = doc.createElement("g");
		group.setAttribute("id", "scaleSvg");
		root.appendChild(group);
		return doc;
	}

	/**
	 * parse an xml
	 * 
	 * @param doc
	 * @param myFile
	 * @return the new File
	 */
	public Document openFile(Document doc, String myFile) {
		
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
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			doc = generateXML();
			e.printStackTrace();
		}
		return doc;
	}

	/**
	 * save given document
	 * 
	 * @param mainDoc
	 *            Document to save
	 * @param myFile
	 *            path where Document will save
	 */
	public void saveXML(Document mainDoc, String myFile) {
		Transformer transformer = null;
		try {
			transformer = TransformerFactory.newInstance().newTransformer();
		} catch (TransformerConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TransformerFactoryConfigurationError e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		DOMSource source = new DOMSource(mainDoc);
		FileOutputStream os;
		try {
			os = new FileOutputStream(new File(myFile));
			StreamResult result = new StreamResult(os);
			try {
				transformer.transform(source, result);
				
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
