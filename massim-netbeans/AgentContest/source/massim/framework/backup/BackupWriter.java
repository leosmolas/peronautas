package massim.framework.backup;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class BackupWriter {


	public static String file_sep = System.getProperty("file.separator");
	public static void write(Document doc, String dir,String filename) {

		File xmlStepFile = new File(dir+file_sep+filename);
		if(!new File(dir).exists()){
			new File(dir).mkdirs();
		}
		try{

			FileOutputStream out = new FileOutputStream(xmlStepFile);
			Transformer transformer = TransformerFactory.newInstance()
					.newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			transformer
					.transform(new DOMSource(doc), new StreamResult(out));
		}
		catch (FileNotFoundException e1) {
			e1.printStackTrace();
			// throw new InvalidConfigurationException(e1);
		}

		catch (TransformerException e2) {
			e2.printStackTrace();
			// throw new InvalidConfigurationException(e2);
		}
	}
	public static String getDate() {
		Date dt = new Date();
		SimpleDateFormat df = new SimpleDateFormat("HH-mm-ss_dd-MM-yyyy");
		return df.format(dt);
	}
	public static Document createDocument() {
		try {
			Document xmlStep = DocumentBuilderFactory.newInstance().newDocumentBuilder()
					.newDocument();
			Element root = xmlStep.createElement("simulation");
			xmlStep.appendChild(root);
			return xmlStep;
		}
		catch (ParserConfigurationException e) {
			throw new RuntimeException(e);
		} 
		
	}
}
