package massim.goldsimulations;

import java.lang.reflect.Field;

import massim.gridsimulations.SimulationAgentPerception;

import org.w3c.dom.Element;

/**
 * This class produces the normal perception which is sent to the agent.
 * 
 */
public class GoldGridSimulationAgentPerception extends SimulationAgentPerception {

	private static final long serialVersionUID = -5791498406627754380L;

	public GridSimulationPerceptionCell cur;
	public GridSimulationPerceptionCell n;
	public GridSimulationPerceptionCell nw;
	public GridSimulationPerceptionCell w;
	public GridSimulationPerceptionCell sw;
	public GridSimulationPerceptionCell s;
	public GridSimulationPerceptionCell se;
	public GridSimulationPerceptionCell e;
	public GridSimulationPerceptionCell ne;

	/*
	 * (non-Javadoc)
	 * 
	 * @see massim.util.XMLCodec.XMLEncodable#encodeToXML(org.w3c.dom.Element)
	 */
	public void encodeToXML(Element target) {
		Field[] fields = this.getClass().getFields();
		for (int i = 0; i < fields.length; i++) {
			try {
				convertEntryToXML(fields[i].get(this), fields[i].getType(),
						target, fields[i].getName());
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * This method converts the perception-class-element to xml
	 * 
	 * @param o
	 *            The object to convert
	 * @param type
	 *            The classtype of the object.
	 * @param target
	 *            The target
	 * @param name
	 *            The objectname
	 * @throws IllegalAccessException
	 */
	public static void convertEntryToXML(Object o, Class type, Element target,
			String name) throws IllegalAccessException {
		if (type.isPrimitive() || type.isEnum() || type == String.class) {
			target.setAttribute(name, String.valueOf(o));
		} else {
			if (o != null) {
				GridSimulationPerceptionCell perceptionCell = (GridSimulationPerceptionCell) o;
				Element classElement = target.getOwnerDocument().createElement(
						"cell");
				classElement.setAttribute("id", name);
				if (perceptionCell.unknown) {
					Element objectElement = target.getOwnerDocument()
							.createElement("unknown");
					classElement.appendChild(objectElement);
				} else {
					boolean empty = true;
					if (perceptionCell.agent) {
						Element objectElement = target.getOwnerDocument()
								.createElement("agent");
						objectElement.setAttribute("type",
								perceptionCell.agentType);
						classElement.appendChild(objectElement);
						empty = false;
					}
					if (perceptionCell.mark) {
						Element objectElement = target.getOwnerDocument()
								.createElement("mark");
						objectElement.setAttribute("value",
								perceptionCell.markText);
						classElement.appendChild(objectElement);
						empty = false;
					}
					if (perceptionCell.gold) {
						Element objectElement = target.getOwnerDocument()
								.createElement("gold");
						classElement.appendChild(objectElement);
						empty = false;
					}
					if (perceptionCell.depot) {
						Element objectElement = target.getOwnerDocument()
								.createElement("depot");
						classElement.appendChild(objectElement);
						empty = false;
					}
					if (perceptionCell.obstacle) {
						Element objectElement = target.getOwnerDocument()
								.createElement("obstacle");
						classElement.appendChild(objectElement);
						empty = false;
					}
					if (empty) {
						Element objectElement = target.getOwnerDocument()
								.createElement("empty");
						classElement.appendChild(objectElement);
					}
				}
				target.appendChild(classElement);
			}
		}
	}
}

