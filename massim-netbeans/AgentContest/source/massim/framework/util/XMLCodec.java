package massim.framework.util;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import massim.framework.connection.AgentCodecProtocolErrorException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * This class provides static methods to encode objects to XML and to decode XML back to objects.
 *
 */
public class XMLCodec {
	/**
	 * Classes that implement this interface provide their own method to encode themselves to XML.
	 *
	 */
	public static interface XMLEncodable {
		void encodeToXML(Element target);
	}
	/**
	 * Classes that implement this interface provide their own method to decode themselves from XML.
	 *
	 */
	public static interface XMLDecodable {
		void decodeFromXML(Element source);
	}
	
	/**
	 * Classes that implement this interface can be used to convert XML to an object.
	 */
	
	public static interface XMLToObjectConverter {
		Object decodeXMLToObject(Element source) throws AgentCodecProtocolErrorException, IllegalAccessException, InstantiationException;
	}
	
	public static class DefaultXMLToObjectConverter implements XMLToObjectConverter{
		private Map<String,Class> classMap;
		private Class defaultClass;
		public DefaultXMLToObjectConverter() {
			classMap=new HashMap<String,Class>();
			defaultClass = Object.class;
		}

		public Object decodeXMLToObject(Element source) throws AgentCodecProtocolErrorException, IllegalAccessException, InstantiationException {
			String classname = source.getAttribute("class");
			Class cls = classMap.get(classname);
			if (cls==null) cls = defaultClass;
			return convertXMLToObject(source, cls);
		}

		public Map<String,Class> getClassMap() {
			return classMap;
		}
		
		public void setClassMap(Map<String,Class> classMap) {
			this.classMap = classMap;
		}
		
		public Class getDefaultClass() {
			return defaultClass;
		}

		public void setDefaultClass(Class cls) {
			defaultClass = cls;
		}
	}
	
	/**
	 * Namespace for all administrative elements and attributes.
	 */
	public final static String metainfoNamespace="http://www.tu-clausthal.de/";
	public final static String metainfoPrefix="meta";
	
	public static void convertEntryToXML(Object o, Class type, Element target, String name) throws IllegalAccessException {
		if (type.isPrimitive() || type.isEnum() || type == String.class) {
			target.setAttribute(name,String.valueOf(o));
		} else if (type.isArray()) {
			if (o!=null) {
				int arraylength = Array.getLength(o);
				Element arrayelement = target.getOwnerDocument().createElement("array");
				arrayelement.setAttributeNS(metainfoNamespace,"name",name);
				arrayelement.setAttributeNS(metainfoNamespace,"length",Integer.toString(arraylength));
				for (int i=0;i<arraylength;i++) {
					convertEntryToXML(Array.get(o,i), type.getComponentType(), arrayelement, "item"+Integer.toString(i));
				}
				target.appendChild(arrayelement);
			} else
				target.setAttribute(name,"null");

		} else { // use object conversion
			if (o!=null) {
				Element objectelement = target.getOwnerDocument().createElement("object");
				target.appendChild(objectelement);
				convertObjectToXML(o, objectelement);
				objectelement.setAttributeNS(metainfoNamespace,"name",name);
			} else
				target.setAttribute(name,"null");
		}
	}
	
	public static void convertObjectToXML(Object o, Element target) throws IllegalAccessException {
		if (o!=null) {
			if (XMLEncodable.class.isAssignableFrom(o.getClass())) {
				XMLEncodable enc = (XMLEncodable) o;
				enc.encodeToXML(target);
			} else {
				Field[] fields = o.getClass().getFields();
				for (int i=0;i<fields.length;i++) {
					if (!fields[i].getType().isSynthetic() && !fields[i].isEnumConstant()) {
						convertEntryToXML(fields[i].get(o), fields[i].getType(), target, fields[i].getName());
					}
				}
			}
		}
	}
	
	public static Object convertXMLToEntry(Element xmlsource, String name, Class fieldtype) throws IllegalAccessException, InstantiationException, AgentCodecProtocolErrorException {
//		log(LOGLEVEL_CRITICAL,"plong: "+name);
		Object v=null;
		if (fieldtype.isSynthetic()) return null;
		//works: byte, short, int, long
		//       float, double
		//       String,
		//       boolean,
		//       enum
		//TODO: char
		
		if (fieldtype == char.class) {
			// FIXME: Character unsupported
		} else if (fieldtype == float.class) {
			try {v=Float.parseFloat(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Float("0");}
		} else if (fieldtype == double.class) {
			try {v=Double.parseDouble(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Double("0");}
		} else if (fieldtype == byte.class) {
			try {v=Byte.decode(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Byte("0");}
		} else if (fieldtype == short.class) {
			try {v=Short.decode(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Short("0");}
		} else if (fieldtype == int.class) {
			try {v=Integer.decode(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Integer("0");}
		} else if (fieldtype == long.class) {
			try {v=Long.decode(xmlsource.getAttribute(name));}
			catch (NumberFormatException e) {v=new Long("0");}
		} else if (fieldtype == boolean.class) {
			v=Boolean.parseBoolean(xmlsource.getAttribute(name));
		} else if (fieldtype == String.class) {
			v=xmlsource.getAttribute(name);
		} else if (fieldtype.isEnum()) {
			try {v=Enum.valueOf(fieldtype,xmlsource.getAttribute(name));}
			catch (IllegalArgumentException e) {}
		} else if (fieldtype.isArray()) {
			//Maybe this array is supposed to be null. This would be indicated by an appropriate attribute, set to "null"
			if (xmlsource.getAttribute(name).equals("null")) return null;

			//First find the appropriate array element
			NodeList nl=XMLUtilities.getChildsByTagName(xmlsource,"array");
			Element e=null;
			for (int i=0;i<nl.getLength();i++) {
				e=(Element)nl.item(i);
				if (e.getAttributeNS(metainfoNamespace,"name").equals(name)) break;
				e=null; //if we leave this loop without break, no element has been found, which is an error
			}
			if (e==null) {
				System.out.println(name + " is missing"); 
				throw new AgentCodecProtocolErrorException();//Array element is missing and not set to null
			}

			// retrieve indicated length, throw an error if invalid
			int len=0;
			try {len=Integer.decode(e.getAttributeNS(metainfoNamespace,"length"));}//decode length
			catch (NumberFormatException e1) {throw new AgentCodecProtocolErrorException();}

			// retrieve array values
			Vector arrayvals = new Vector();
			for (int i=0;i<len;i++) {
				//if some element is invalid this will throw an error
				arrayvals.add(convertXMLToEntry(e,"item"+i,fieldtype.getComponentType()));
			}
			v=Array.newInstance(fieldtype.getComponentType(),arrayvals.size());
			for (int i=0;i<arrayvals.size();i++) Array.set(v,i,arrayvals.get(i));
			
		} else {
			if (xmlsource.getAttribute(name)=="null") return null;
			v=convertXMLToObject(xmlsource, fieldtype);
		}
		return v;
	}

	public static Object convertXMLToObject(Element source, Class cls) throws InstantiationException, IllegalAccessException, AgentCodecProtocolErrorException {
//		log(LOGLEVEL_CRITICAL,"plueng: "+cls.getName());

		Object o = cls.newInstance();
		if (XMLDecodable.class.isAssignableFrom(o.getClass())) {
			XMLDecodable enc = (XMLDecodable) o;
			enc.decodeFromXML(source);
		} else {
			Field[] fields = cls.getFields();
			for (int i=0;i<fields.length;i++) {
				if (!fields[i].getType().isSynthetic() && !fields[i].isEnumConstant()) {
					Object r=convertXMLToEntry(source, fields[i].getName(), fields[i].getType());
					fields[i].set(o, r);
				}
			}
		}
		return o;
	}
}
