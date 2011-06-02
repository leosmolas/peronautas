package massim.framework.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import org.w3c.dom.Document;

public interface XMLDocumentServer extends Remote {
	Document getXMLDocument() throws RemoteException;
}
