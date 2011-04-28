package massim.server;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import massim.framework.ObjectDumperObserver;
import massim.framework.Observer;

import org.w3c.dom.Element;


public class ObjectDumperObserverFactory implements ObserverFactory {
	public Observer createObserver(Element config, String simulationid) {
		String outputDirectory=".";
		ObjectDumperObserver objectDumperObserver = null;
		try {
			File file = new File(outputDirectory,simulationid+".raw");
			objectDumperObserver = new ObjectDumperObserver(new FileOutputStream(file));
		} catch (IOException e) {
			log(LOGLEVEL_ERROR,"unable to init log-file");
			e.printStackTrace();
			throw new RuntimeException("unable to init log-file");
		}
		return objectDumperObserver;
	}
}
