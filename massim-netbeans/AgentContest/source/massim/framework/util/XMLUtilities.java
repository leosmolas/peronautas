package massim.framework.util;

import java.util.Vector;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class XMLUtilities {
	public static NodeList getChildsByTagName(Element root, String name) {
		final Vector<Node> v = new Vector<Node>();
		NodeList nl = root.getChildNodes();
		for (int i=0;i<nl.getLength();i++) {
			Node n = nl.item(i);
			if (n.getNodeType()==Element.ELEMENT_NODE) {
				Element e = (Element)n;
				if (name.equals("*") || e.getNodeName().equalsIgnoreCase(name)) v.add(n);
			}
		}
		
		return new NodeList() {
			public Node item(int index) {
				if (index>=v.size() || index<0) return null;
				else return v.get(index);}
			public int getLength() {return v.size();}
		};
	}
}
