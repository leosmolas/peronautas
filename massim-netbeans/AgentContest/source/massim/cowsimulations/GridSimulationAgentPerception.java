package massim.cowsimulations;

import java.lang.reflect.Field;

import massim.gridsimulations.SimulationAgentPerception;

import org.w3c.dom.Element;


/**
 * This class produce the normal perception, which is sent to the agent.
 *
 */
public class GridSimulationAgentPerception extends SimulationAgentPerception{

	private static final long serialVersionUID = -5791498406627754380L;
	public int score;
	//public GridSimulationPerceptionCell cur;
	public GridSimulationPerceptionCell perception[][];

	/* (non-Javadoc)
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
	 * @param o The object to convert
	 * @param type The classtype of the object.
	 * @param target The target
	 * @param name The objectname
	 * @throws IllegalAccessException
	 */
	@SuppressWarnings("unchecked")
	public static void convertEntryToXML(Object o, Class type, Element target,
			String name) throws IllegalAccessException {
		
		if (type.isPrimitive() || type.isEnum() || type == String.class) {
			target.setAttribute(name, String.valueOf(o));
		} else {
			if (o != null) {
				GridSimulationPerceptionCell[][] perceptionCell = (GridSimulationPerceptionCell[][]) o;
				int i, k;
				int length = perceptionCell.length;
				int half = length / 2;
				for (i=0;i<length;i++)
				{
					for (k=0;k<length;k++)
					{		
						if (perceptionCell[i][k] != null)
						{
							int downX = i-half;
							int downY = k-half;
							Element classElement = target.getOwnerDocument().createElement(
								"cell");
							//classElement.setAttribute("id", name);
							classElement.setAttribute("x", String.valueOf(downX));
							classElement.setAttribute("y", String.valueOf(downY));
							boolean empty = true;
							if (perceptionCell[i][k].unknown)
							{
								Element objectElement = target.getOwnerDocument()
								.createElement("unknown");
								classElement.appendChild(objectElement);
							}
							else
							{
							if (perceptionCell[i][k].agent) {
									Element objectElement = target.getOwnerDocument()
										.createElement("agent");
									objectElement.setAttribute("type", perceptionCell[i][k].agentType);
									classElement.appendChild(objectElement);
									empty = false;
							}
							if (perceptionCell[i][k].cow) {
								Element objectElement = target.getOwnerDocument()
									.createElement("cow");
								objectElement.setAttribute("ID", perceptionCell[i][k].cowID);
								classElement.appendChild(objectElement);
								empty = false;
							}
					
							if (perceptionCell[i][k].obstacle) {
								Element objectElement = target.getOwnerDocument()
									.createElement("obstacle");
								classElement.appendChild(objectElement);
								empty = false;
							} 
							if (perceptionCell[i][k].corral)
							{
								Element objectElement = target.getOwnerDocument()
								.createElement("corral");
								objectElement.setAttribute("type", perceptionCell[i][k].stableType);
								classElement.appendChild(objectElement);
								empty = false;
							}
							if (perceptionCell[i][k].switcher){

								Element objectElement = target.getOwnerDocument()
								.createElement("switch");
								
								classElement.appendChild(objectElement);
								empty = false;
								
							}
							if(perceptionCell[i][k].fence){

									Element objectElement = target.getOwnerDocument()
									.createElement("fence");
									
								 objectElement.setAttribute("open", ""+perceptionCell[i][k].open);
								
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
		}
	}
}
