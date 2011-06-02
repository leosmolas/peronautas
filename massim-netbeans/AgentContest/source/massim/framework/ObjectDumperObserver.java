package massim.framework;

import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.log;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

public class ObjectDumperObserver extends DefaultObserver {
	public enum Marker {SIMULATIONSTART, SIMULATIONEND, SIMULATIONSTATE};
	private OutputStream destinationFile;
	private ObjectOutputStream objectsink;
	
	public ObjectDumperObserver(OutputStream sink) throws IOException {
		destinationFile = sink;
		objectsink = new ObjectOutputStream(destinationFile);
	}
	
	public ObjectDumperObserver() {
		destinationFile = null;
		objectsink = null;
	}

	/**
	 * @return Returns the destinationFile.
	 */
	public OutputStream getDestinationFile() {
		return destinationFile;
	}

	/**
	 * @param destinationFile The destinationFile to set.
	 */
	public void setDestinationFile(OutputStream destinationFile) throws IOException {
		this.destinationFile = destinationFile;
		objectsink = new ObjectOutputStream(destinationFile);
	}
	
	@Override
	public void notifySimulationEnd() {
		log(LOGLEVEL_DEBUG);
		try {objectsink.writeObject(Marker.SIMULATIONEND);}
		catch (IOException e) {log(LOGLEVEL_ERROR, e.toString());}
	}

	@Override
	public void notifySimulationStart() {
		log(LOGLEVEL_DEBUG);
		try {objectsink.writeObject(Marker.SIMULATIONSTART);}
		catch (IOException e) {log(LOGLEVEL_ERROR, e.toString());}
	}

	@Override
	public void notifySimulationState(SimulationState state) {
		log(LOGLEVEL_DEBUG);
		try {
			objectsink.writeObject(Marker.SIMULATIONSTATE);
			objectsink.writeObject(state);
		}
		catch (IOException e) {log(LOGLEVEL_ERROR, e.toString());}
	}

	@Override
	public void start() {
		super.start();
		log(LOGLEVEL_DEBUG);
	}

	@Override
	public void stop() {
		log(LOGLEVEL_DEBUG);
		super.stop();
	}

}
