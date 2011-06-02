package massim.framework.connection;

import static massim.framework.util.DebugLog.LOGLEVEL_CRITICAL;
import static massim.framework.util.DebugLog.LOGLEVEL_DEBUG;
import static massim.framework.util.DebugLog.LOGLEVEL_ERROR;
import static massim.framework.util.DebugLog.LOGLEVEL_NORMAL;
import static massim.framework.util.DebugLog.log;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import massim.framework.AbstractSimulationAgent;
import massim.framework.Action;
import massim.framework.Component;
import massim.framework.FinalPerception;
import massim.framework.InitialStickyPerception;
import massim.framework.InvalidAction;
import massim.framework.Perception;
import massim.framework.TimeCriticalPerception;
import massim.framework.UniqueSimulationAgent;
import massim.framework.util.XMLCodec;
import massim.framework.util.XMLUtilities;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * This class manages the communication to an agent, offering methods that will make it possible to treat instances of this object as
 * a simulation agent.
 * 
 * This special class uses XML to communicate with an agent.
 *
 */
public class XMLSocketSimulationAgent extends AbstractSimulationAgent implements Component, SocketHandler, UniqueSimulationAgent {
	
	// general stuff
	private boolean active; // is this component running (thus accepting sockets)?
	private Object generalSync; // general all purpose lock
	
	// current socket
	private Socket socket;
	
	// threads associated with current socket aswell as a synchronization method
	private Thread socketThreadReceiver;
	private Thread socketThreadSender;
	private Object socketSync;
	
	// configuration
	private int maximumPacketLength;
	private long timeOut;
	private long auxiliaryTimeOut;
	private long disconnectTimeOut;
	private Map<String,Class> actionClassMap;
	private XMLCodec.XMLToObjectConverter xmlToObjectConverter;
	
	//associated account
	private Object identifier;
	
	// message id counter and sync object
	private long messageIdentifier;
	private Object messageIdentifierSync;
	
	// limit pinging
	private boolean allowPing;
	private long pingCounter;
	private long pingMaximum;
	private long[] pingTimes;
	private boolean limitPingsPerTime;
	private long pingsPerTimeFrame;
	private long pingTimeFrame;
	private int pingtimelimit_current;
	private int pingtimelimit_oldest;
	
	// queues for sending and receiving
	private LinkedBlockingQueue<Document> sendQueue;
	private boolean disconnecting;
	private Object disconnectSync;
	private Map<Long,Document> actionQueue;

	
	// statistics
	private long statDisconnects;
	private long statTimeouts;
	private long statProtocolErrors;
	
	//Pfuschvariables
	private boolean finalMessageSent;
	private Document stickyMessage;
	private Document lastSimEndMessage;
	/**
	 * Namespace for all administrative elements and attributes.
	 */
	public final String metainfoNamespace="http://www.tu-clausthal.de/";

	/**
	 * Namespace prefix for all administrative elements and attributes.
	 */
	public final String metainfoPrefix="meta";

	/**
	 * Construct a new XMLSocketSimulationAgent that will try to generate action of a certain class.
	 * It's set to wait forever and will allow packets of any size.
	 * @param account 
	 * @param actionClass
	 */
	public XMLSocketSimulationAgent() {
		active = false;
		generalSync = new Object();
		disconnectSync = new Object();
		allowPing = false;
		pingCounter = 0;
		pingMaximum = Integer.MAX_VALUE;

		limitPingsPerTime = false;
		pingTimes = null;
		pingTimeFrame = 1;
		pingsPerTimeFrame = 0;
		
		identifier = this;
		socketSync = new Object();
		socket=null;
		socketThreadReceiver=null;
		socketThreadSender=null;
		actionClassMap = new HashMap<String,Class>();
		actionQueue = new HashMap<Long,Document>();
		
		timeOut=Long.MAX_VALUE;
		disconnectTimeOut=10000;
		sendQueue = new LinkedBlockingQueue<Document>();
		
		maximumPacketLength = Integer.MAX_VALUE;
		messageIdentifier = 0;
		messageIdentifierSync = new Object();
		
		stickyMessage = null;
		lastSimEndMessage = null;
	}
	
	/*
	 * Configuration methods
	 * 
	 */
	
	//no javadoc. should be copied from UniqueSimulationAgent
	public Object getIdentifier() {
		return identifier;
	}

	/**
	 * Set identifier for this object
	 */
	public void setIdentifier(Object identifier) {
		this.identifier = identifier;
	}

	/**
	 * Retrieve the maximum major time that is granted to an agent to process a perception and answer with an action.
	 * @param timeOut timeout in milliseconds 
	 */
	public long getTimeout() {
		return timeOut;
	}

	/**
	 * Set the maximum major time that is granted to an agent to process a perception and answer with an action.
	 * @param timeOut timeout in milliseconds 
	 */
	public void setTimeout(long timeOut) {
		this.timeOut = timeOut;
	}
	
	/**
	 * Retrieve the maximum auxiliary time that is granted to an agent to process a perception and answer with an action.
	 * @param timeOut timeout in milliseconds 
	 */
	public long getAuxiliaryTimeout() {
		return auxiliaryTimeOut;
	}

	/**
	 * Set the maximum auxiliary time that is granted to an agent to process a perception and answer with an action.
	 * @param timeOut timeout in milliseconds 
	 */
	public void setAuxiliaryTimeout(long auxiliaryTimeOut) {
		this.auxiliaryTimeOut = auxiliaryTimeOut;
	}
	
	/**
	 * Set maximum packet length allowed to receive.
	 * @param maximumPacketLength maximum packet length in bytes
	 */
	public void setMaximumPacketLength(int maximumPacketLength) {
		this.maximumPacketLength = maximumPacketLength;
	}
	
	/**
	 * Retrieve maximum packet length allowed to receive.
	 * @return maximum packet length in bytes
	 */
	public long getMaximumPacketLength() {
		return maximumPacketLength;
	}
	
	/**
	 * Enable/disable ping.
	 * @param ping true iff ping should be enabled
	 */
	public void setPingAllowed(boolean ping) {
		this.allowPing=ping;
	}
	
	/**
	 * Return true iff ping is enabled.
	 * @return true iff ping is enabled.
	 */
	public boolean isPingAllowed() {
		return this.allowPing;
	}
	
	/**
	 * Return maximum number of pings.
	 * @return maximum number of pings
	 */
	public long getPingMaximum() {
		return pingMaximum;
	}
	
	/**
	 * Set maximum number of pings.
	 * @return maximum number of pings
	 */
	public void setPingMaximum(long maxping) {
		pingMaximum = maxping;
	}
	
	/**
	 * Return current action class.
	 * @return current action class.
	 */
	public Map<String,Class> getActionClassMap() {
		return actionClassMap;
	}
	
	/**
	 * Set action class.
	 * @param actionclass action to generate.
	 */
	public void setActionClassMap(Map<String,Class> actionclassmap) {
		this.actionClassMap=actionclassmap;
	}

	/**
	 * @return Returns the xmlToObjectConverter.
	 */
	public XMLCodec.XMLToObjectConverter getXmlToObjectConverter() {
		return xmlToObjectConverter;
	}

	/**
	 * @param xmlToObjectConverter The xmlToObjectConverter to set.
	 */
	public void setXmlToObjectConverter(
			XMLCodec.XMLToObjectConverter xmlToObjectConverter) {
		this.xmlToObjectConverter = xmlToObjectConverter;
	}
	
	/**
	 * Configure ping per timeframe limit
	 */
	public void configurePingPerTimeFrameLimit(long pings, long timeframe) {
		pingTimeFrame = timeframe;
		pingsPerTimeFrame = pings;
	}
	
	/**
	 * Activate/Deachtivate ping per timeframe limit
	 */
	public void setPingPerTimeFrameEnabled(boolean v) {
		synchronized(generalSync) {
			if (v) {
				pingTimes = new long[(int)pingsPerTimeFrame];
				pingtimelimit_oldest = pingtimelimit_current=0;
			} else pingTimes = null;
		}
	}
	
	/**
	 * Retrieve maximum time to use when disconnecting to send messages that are still in the queue.
	 * @return maximum disconnect time 
	 */
	public long getDisconnectTimeOut() {
		return disconnectTimeOut;
	}
	
	/**
	 * Set maximum time to use when disconnecting to send messages that are still in the queue
	 * @param t new maximum disconnect time 
	 */
	public void setDisconnectTimeOut(long t) {
		this.disconnectTimeOut = t;
	}
	
	// no javadoc. should be copied from SimulationAgent
	public Action getAction(Perception perception) {
		//FIXME: What if there is no connection?
		//Answer: For now the timeout will be granted anyway.
		
		long timeOut = this.timeOut;
		
		if (perception instanceof TimeCriticalPerception) {
			timeOut=((TimeCriticalPerception)perception).getTimeout(this.timeOut);
		}
		
		// convert perception to XML
		finalMessageSent=false;
		Document perceptiondoc = null;
		Element perceptionelement = null;
		long timestamp = System.currentTimeMillis();
		if (perception instanceof massim.framework.FinalPerception) {
			perceptiondoc = XMLCommunicationUtilities.createDefaultMessage("sim-end");
			finalMessageSent=true;
			perceptionelement = perceptiondoc.createElement("sim-result");
		} else
		if (perception instanceof InitialStickyPerception) {
			perceptiondoc = XMLCommunicationUtilities.createDefaultMessage("sim-start");
			perceptionelement = perceptiondoc.createElement("simulation");
		} else {
			perceptiondoc = XMLCommunicationUtilities.createDefaultMessage("request-action");
			perceptionelement = perceptiondoc.createElement("perception");
			perceptionelement.setAttribute("deadline",Long.toString(timestamp+timeOut));
		}
		Element perceptionroot = perceptiondoc.getDocumentElement();
		perceptionroot.setAttribute("timestamp",Long.toString(timestamp));
		
		try {
			massim.framework.util.XMLCodec.convertObjectToXML(perception, perceptionelement);
		} catch (IllegalAccessException e) {
			log(LOGLEVEL_CRITICAL,"illegal access exception");
			e.printStackTrace();
			throw new RuntimeException();
		}
		perceptionroot.appendChild(perceptionelement);

		long myperceptionid; 
		synchronized(messageIdentifierSync) {
			myperceptionid = messageIdentifier;
			messageIdentifier ++;
			
			//PNO - 21.03.2007, sim-end message contained id element! That is against the protocol specs.
			if (!(perception instanceof massim.framework.FinalPerception)) 
				perceptionelement.setAttribute("id",Long.toString(myperceptionid));
		}

		Document actiondoc = null;
		String content;

		synchronized (actionQueue) {
			actionQueue.put(myperceptionid, null);
			synchronized (sendQueue) {
				if (perception instanceof FinalPerception) {
					sendQueue.remove(lastSimEndMessage);
					lastSimEndMessage = perceptiondoc;
					stickyMessage = null;
				} else if (perception instanceof InitialStickyPerception) {
					stickyMessage = perceptiondoc;
				}
				sendMessage(perceptiondoc);
			}
			content = perceptiondoc.getChildNodes().item(0).getChildNodes().item(0).getNodeName();

                        // consider only request-action message!!!
			if (content.equals("perception")) {

				// now wait for an answer
                                int timeschecked = 0;
				long currentTime = System.currentTimeMillis();
				while (true) {
					// check if there is already some answer
                                        timeschecked++;
					actiondoc = actionQueue.get(myperceptionid);
					if (actiondoc != null) {
                                                System.out.println("666: @getAction:401 myperceptionid = " + myperceptionid);
                                                System.out.println("666: @getAction:402 actiondoc no es null");
                                                printmotherfucker("666: @getAction:403", actiondoc);
						break; // So we've got some answer. We're done. Break!
                                        }
					else {
						if (currentTime > timestamp + timeOut + auxiliaryTimeOut) {
                                                        System.out.println("666: @getAction:408 salio por timeout: " + timeschecked);
							break;
                                                }
						// no answer yet. Se we'll wait up to dead line or if
						// sth. changes and actionQueue gets notified
						long wait = Math.max(
								(timestamp + timeOut + auxiliaryTimeOut)
										- currentTime, 1);
						try {
							actionQueue.wait(wait);
						} catch (InterruptedException e) {
							log(LOGLEVEL_CRITICAL, "Thread was interrupted");
						}// This thread shouldn't be interrupted.
						currentTime = System.currentTimeMillis();
					}
				}
			}
			actionQueue.remove(myperceptionid);
		}
	
		if(!content.equals("perception")){
			
			return new InvalidAction();
			
		}
		// So we've got possibly an answer or timeout has occured
		else if (actiondoc==null) {
			log(LOGLEVEL_DEBUG, "No valid document received in time. Returning InvalidAction.");
			synchronized(sendQueue) {
				sendQueue.remove(perceptiondoc);			
			}
			return new InvalidAction();
		}
		
		Element actionroot = actiondoc.getDocumentElement();
		NodeList actionelements = actionroot.getElementsByTagName("action");
		if (actionelements.getLength()<1) {
			return new InvalidAction();
		}
		try {
			return (Action) xmlToObjectConverter.decodeXMLToObject((Element)actionelements.item(0));
		} catch (InstantiationException e) {log(LOGLEVEL_CRITICAL,"unable to instantiate class");
		} catch (IllegalAccessException e) {log(LOGLEVEL_CRITICAL,"unable to access field");
		} catch (AgentCodecProtocolErrorException e) {log(LOGLEVEL_ERROR,"unable to convert received DOM to Action");
		}
		return new InvalidAction();//Agent must have done sth wrong so ignore it's action!
	}

	public void remove() {
		if (!finalMessageSent) {
			synchronized (sendQueue) {
				if (lastSimEndMessage != null) {//remove old remove message
					sendQueue.remove(lastSimEndMessage);
				}
				Document doc = XMLCommunicationUtilities.createDefaultMessage("sim-end");
				lastSimEndMessage = doc;
				stickyMessage = null;
				sendMessage(doc);
			}
		}
	}

	private byte[] convertMessage(Document doc) {
		//convert doc to byte array
		ByteArrayOutputStream bufferstream = new ByteArrayOutputStream();

		TransformerFactory transFactory = TransformerFactory.newInstance();
		Transformer transformer = null;
		try {
			transformer = transFactory.newTransformer();
			transformer.setOutputProperty("indent","yes");
		} catch (TransformerConfigurationException e) {throw new RuntimeException(e);}

		try {
			transformer.transform(new DOMSource(doc),new StreamResult(bufferstream));
		} catch (TransformerException e) {
			log(LOGLEVEL_ERROR,"transformer error");
			throw new RuntimeException();
		}
		//put it onto the sending queue.
		
		return bufferstream.toByteArray();
	}
	
	private void sendMessage(Document doc) {
		synchronized(sendQueue) {

			try {sendQueue.put(doc);
			}
/*
			ByteArrayOutputStream temp = new ByteArrayOutputStream();
			try {
				TransformerFactory.newInstance().newTransformer().transform(new DOMSource(doc),new StreamResult(temp));
				log(LOGLEVEL_NORMAL,temp.toString());
			} catch (TransformerConfigurationException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TransformerFactoryConfigurationError e) {
				// TODO Auto-generated catch block

				e.printStackTrace();
			}
	*/		
			catch (InterruptedException e) {throw new RuntimeException();}//shouldn't happen with a LinkedBlockingQueue
		}
	}
	
	/*
	 * lower level communication
	 * 
	 */
	
	public void handleSocket(final Socket s) {
		log(LOGLEVEL_CRITICAL,"received socket:"+s.toString());
		if (!active) return;
		synchronized (socketSync) {
			if (socket!=null) {
				socketThreadReceiver.interrupt();
				socketThreadSender.interrupt();
				try {socket.close();}
				catch (IOException e) {
					log(LOGLEVEL_ERROR, "Error while closing socket");
					throw new RuntimeException();// This is not supposed to happen
				}
			}
			socket = s;
			synchronized(sendQueue) {
				//before resuming normal sending thread, send possibly initial message
				if (stickyMessage==null) {//if there is no sticky message, possibly remove old sim-end-message
					sendQueue.remove(lastSimEndMessage);
					lastSimEndMessage=null;
				} else if (stickyMessage!=null && !sendQueue.contains(stickyMessage)) {
					byte[] b=convertMessage(stickyMessage);
					try {
						sendPacket(s,b);
					} catch (IOException e) {
						try {s.close();} catch (IOException e2){}
						log(LOGLEVEL_DEBUG, "Error while writing to socket. Abandoning socket");
						return;
					}
				}
			}

			socketThreadReceiver = new Thread() {public void run() {XMLSocketSimulationAgent.this.socketThreadReceive(s);}};
			socketThreadReceiver.start();
			socketThreadSender = new Thread() {public void run() {XMLSocketSimulationAgent.this.socketThreadSend(s);}};
			socketThreadSender.start();
		}
	}
			
	public Socket getCurrentSocket() {
		synchronized (socketSync) {
			return socket;
		}
	}
	
	private void socketThreadReceive(Socket s) {
		DocumentBuilderFactory factory=DocumentBuilderFactory.newInstance();
		DocumentBuilder documentbuilder=null;
		try {
			documentbuilder=factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {throw new RuntimeException(e);}

		InputStream in = null;
		try {in = s.getInputStream();} catch (IOException e) {
			log(LOGLEVEL_ERROR,"Unable to get InputStream from Socket.");
			return;
		}

		ByteArrayOutputStream packetbuffer = new ByteArrayOutputStream();
		boolean seeknextend = false; // is the terminating null-byte overdue?
		int packetlen = 0; // length of current packet so far 
		
		try {
			while (true) {
				// Read at least 1 byte or as many as are available, up to maximum packet length
				byte[] buffer; // read buffer
				int amount = in.available();
				if (amount==0) amount = 1;
				if (amount>maximumPacketLength) amount = maximumPacketLength;
				buffer = new byte[amount];
				System.out.println("666: @socketThreadReceive:593 about to read something");
				in.read(buffer);
                                System.out.println("666: @socketThreadReceive:595 read something, process it IT IS DIFFERENTE");
                                System.out.println("666: check");
				// process read bytes
				int firstnotcopied=0;
				for (int i=0;i<amount;i++) {
					// if we've found a null byte or if we're at the end of the buffer (or both)
					if ((buffer[i]==0 || i==amount-1) && !seeknextend) {
						// first check if we're breaching maximum packet length
						packetlen+=i-firstnotcopied;
						if (packetlen>maximumPacketLength) {
							log(LOGLEVEL_NORMAL, "packet too long");
							seeknextend=true;
						} else // and write possibly data to packet buffer
						packetbuffer.write(buffer,firstnotcopied,i-firstnotcopied+(buffer[i]==0 ? 0 : 1));
                                                System.out.println(packetbuffer.toString());
					}
					if (buffer[i] == 0) { // if we've found a null byte some packet has to end here.
						//convert packet to XML
						Document doc = documentbuilder.parse(new ByteArrayInputStream(packetbuffer.toByteArray()));
						processReceivedDocument(doc);
						packetbuffer = new ByteArrayOutputStream();
						seeknextend=false;
						packetlen=0;
						firstnotcopied = i+1;
					}
				}
			}
		} catch (IOException e) {
			
		} catch (SAXException e) {
			
		}
	}

	private void socketThreadSend(Socket s) {
		while (true) {
			byte[] tosend;
			Document tosenddoc;
			try {
				// if there is no message to send and disconnection is in progress then a disconnect
				// message must have been sent. So let's quit this task and continue with disconnection
				synchronized(disconnectSync) {
					if (disconnecting && sendQueue.isEmpty()) {
						break;
					}
				}
				tosenddoc = sendQueue.take();
				tosend = convertMessage(tosenddoc);
			}
			catch (InterruptedException e) {
				log(LOGLEVEL_DEBUG, "Thread interrupted.");
				break;
			}
			try {
				
				sendPacket(s,tosend);
				sendQueue.remove(tosend);
			} catch (IOException e) {
				// IOException caught. Cannot continue now since we don't know anything about the current socket state.
				// So abandon this socket and possibly wait for another socket.
				try {s.close();} catch (IOException e2){}
				log(LOGLEVEL_DEBUG, "Error while writing to socket. Abandoning socket");
				break;
			}
		}
		log(LOGLEVEL_DEBUG, "Thread finished successfully.");
	}
	
	private void sendPacket(Socket s, byte[] b) throws IOException {
		OutputStream out = s.getOutputStream();
		out.write(b);
		out.write(0);
		out.flush();
	}
	
	private void processReceivedDocument(Document doc) {
                System.out.println("666: @processReceivedDocument:668");
		TransformerFactory transFactory = TransformerFactory.newInstance();
		Transformer transformer = null;
		try {
			transformer = transFactory.newTransformer();
			transformer.setOutputProperty("indent","yes");
		} catch (TransformerConfigurationException e) {throw new RuntimeException(e);}

		
		Element root = doc.getDocumentElement();
		if (root==null) {log(LOGLEVEL_NORMAL,"received document misses root element");return;}
		if (root.getNodeName().equals("message")) { 
			if (root.getAttribute("type").equals("action")) {
                            processAction(doc);
                        } else
			if (root.getAttribute("type").equals("ping")) processPing(doc); else {
				log(LOGLEVEL_NORMAL,"received unknown message type");
				try {transformer.transform(new DOMSource(doc),new StreamResult(System.out));}catch(Exception e) {}
				return;
			}
		} else {
			log(LOGLEVEL_NORMAL,"received invalid message");
			try {transformer.transform(new DOMSource(doc),new StreamResult(System.out));}catch(Exception e) {}
			return;
		}
	}
	
	private void processAction(Document doc) {
//		log(LOGLEVEL_NORMAL,"processing action");
		Element root = doc.getDocumentElement();
		long actionid;
//		log(LOGLEVEL_NORMAL,"action id is actually:"+root.getAttribute("actionId"));
		Element action = (Element) XMLUtilities.getChildsByTagName(root,"action").item(0);
		if (action==null) {
			log(LOGLEVEL_ERROR,"no action element inside action message");
			return;
		}
		try {actionid=Long.parseLong(action.getAttribute("id"));
		} catch (NumberFormatException e) {
			log(LOGLEVEL_ERROR,"received invalid or no actionid");
			return;
		}
		//deliver action document to appropriate waiting getAction thread
		synchronized(actionQueue) {
			if (actionQueue.containsKey(new Long(actionid))) {
                                System.out.println("666: @processAction:713 I AM PUTTING IT IN!");
				actionQueue.put(actionid,doc);
				actionQueue.notify();

			}
		}
	}

	private void processPing(Document pingdoc) {
		log(LOGLEVEL_NORMAL,"processing ping");
		synchronized(generalSync) {
		if (!allowPing) {log(LOGLEVEL_NORMAL,"client tried to ping although pinging is disabled");return;} 
		if (pingMaximum<=pingCounter) {log(LOGLEVEL_NORMAL,"client tried to ping although his pinging account is used up (ping "+ pingCounter +" of "+ pingMaximum +").");return;}
			pingCounter++;
		}
		long t = System.currentTimeMillis();
		if (limitPingsPerTime) {
			//first delete unused entries
			while (pingtimelimit_current!=pingtimelimit_oldest) {
				if (pingTimes[pingtimelimit_oldest]<t-pingTimeFrame) {
					pingtimelimit_oldest++;
					if (pingtimelimit_oldest>=pingTimes.length) pingtimelimit_oldest=0;
				} else break;
			}
			if (((pingtimelimit_current+1) % pingTimes.length) == pingtimelimit_oldest) {//meaning that there is no place to remember the new time
				log(LOGLEVEL_NORMAL,"client tried to ping too often in a certain time frame");return;
			}
		}
		Element payload_ping = (Element) XMLUtilities.getChildsByTagName(pingdoc.getDocumentElement(),"payload").item(0);
		if (payload_ping==null) {
			log(LOGLEVEL_ERROR,"received invalid ping");
			return; //Invalid ping
		}
		
		Document pongdoc = XMLCommunicationUtilities.createDefaultMessage("pong");
		
		Element payload_pong = pongdoc.createElement("payload");
		if (payload_ping.getAttribute("value").length()>100) {
			log(LOGLEVEL_ERROR,"ping-payload too big, ignoring");
			return; //Invalid ping
		}
		payload_pong.setAttribute("value",payload_ping.getAttribute("value"));
		pongdoc.getDocumentElement().appendChild(payload_pong);
		sendMessage(pongdoc);
		if (limitPingsPerTime) {
			pingTimes[pingtimelimit_current] = t;
			pingtimelimit_current++;
			if (pingtimelimit_current>=pingTimes.length) pingtimelimit_current=0;
		}
	}
	
	public void start() {
		active=true;
		disconnecting=false;
	}

	public void stop() {
		active=false;
		synchronized(disconnectSync) {
			sendMessage(XMLCommunicationUtilities.createDefaultMessage("bye"));
			disconnecting=true;
		}
		System.err.println("syncing...");
		synchronized(socketSync) {
			if (socket!=null) {
				try {
					socketThreadSender.join(disconnectTimeOut);
				} catch(InterruptedException e) {
					log(LOGLEVEL_CRITICAL,"Interrupted while waiting to shut down. This is not supposed to happen.");
				}
				
				socketThreadReceiver.interrupt();
				socketThreadSender.interrupt();
				try {socket.close();}
				catch (IOException e) {
					log(LOGLEVEL_ERROR,"Error while closing socket");
				}
			}
		}
	}


        public void printmotherfucker(String preambulo, Document doc) {
            TransformerFactory transFactory = TransformerFactory.newInstance();
            Transformer transformer = null;
            try {
                    transformer = transFactory.newTransformer();
                    transformer.setOutputProperty("indent","yes");
            } catch (TransformerConfigurationException e) {throw new RuntimeException(e);}

            System.out.println(preambulo);
            try {transformer.transform(new DOMSource(doc),new StreamResult(System.out));}catch(Exception e) {}
        }
}
