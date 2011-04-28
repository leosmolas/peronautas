package massim.server;

/**
 * This class provides some utilities to easily develop a new server.
 *
 */

import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import massim.test.InvalidConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public class AbstractServer {
	
	
	/**
	 * This help string will be printed to stderr when this server is called with --help.
	 */
	String commandLineHelpText =
		"Massim Server\n" +
		"parameters are:" +
		"--help  -- display this help text" +
		"--conf <configfile>  -- set config file to use";
	
	/**
	 * This will parse the command line. It currently accepts two parameters:
	 * --conf filename
	 * This will load filename as XML configuration
	 * --help
	 * This will print commandLineHelpText.
	 * @param args
	 * @return
	 */
	
	String defaultConfigurationFilename = "conf.xml";
	public static String configurationFilenamePath =".";
	
	Element parseCommandLineToConfig(String[] args) throws InvalidConfigurationException {
		String configfilename = defaultConfigurationFilename;
		// parse command line
		for (int i=0;i<args.length;i++) {
			if (args[i].equals("--conf")) {
				if (i+1<args.length) {
					configfilename=args[i+1];
					i++;
				} else {
					log(LOGLEVEL_ERROR,"argument for config file missing. Aborting.");
					System.exit(1);
				}
			} else
			if (args[i].equals("--help")) {
				System.err.println(commandLineHelpText);
				return null;
			} else {
				System.err.println("Invalid command line arguments. Use --help to see help.");
				throw new InvalidConfigurationException();
			}
		}

		// Read and parse configuration
		File configfile = new File(configfilename);
		try {
			AbstractServer.configurationFilenamePath = configfile.getCanonicalFile().getParent();
		} catch (IOException e1) {
			System.err.println("Error while determining the configfile path.");
			throw new InvalidConfigurationException(e1);
		}
		DocumentBuilder documentbuilder=null;
		DocumentBuilderFactory dbfactory=DocumentBuilderFactory.newInstance();
		dbfactory.setNamespaceAware(true);
		try {
			documentbuilder=dbfactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			System.err.println("Error while creating document builder.");
			throw new InvalidConfigurationException(e);
		}
		Document doc = null;
		try {
			doc = documentbuilder.parse(configfile);
		} catch (SAXException e) {
			System.err.println("Error parsing configuration.");
			throw new InvalidConfigurationException(e);
		} catch (IOException e) {
			System.err.println("Error reading configuration.");
			throw new InvalidConfigurationException(e);
		}
		Element configroot=doc.getDocumentElement();
		if (doc==null) {
			System.err.println("Error parsing configuration: Missing root element");
			throw new InvalidConfigurationException();
		}
		return configroot;
	}
	

}
