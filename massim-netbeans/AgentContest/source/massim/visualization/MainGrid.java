package massim.visualization;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import massim.visualization.svg.SvgFunction;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * TODO create Grid
 */
public class MainGrid extends SvgFunction {
	/* width and height of the grid */
	private double widthGrid;
	private double heightGrid;
	/* width and height of a cell, value read from config file, default: 42 */
	private double widthGridCell;
	private double heightGridCell;
	/* background colour of the grid, read from config file, default: cornsilk */
	private String backgroundColour0 = "afdbb1";
	private String backgroundColour1 = "9ec7a0";
	/* stroke colour of the grid, read from config file, default: black */
	private String strokeColour = "black";
	/* stroke width, read from config file, default: 3 */
	private double strokeWidth = 3;
	/* font size of the text under the grid, read from config file, default: 30 */
	private double fontSize = 30;
	/*
	 * font family of the text under the grid, read from config file, default:
	 * Arial Unicode
	 */
	private String fontFamily = "Arial Unicode";
	/* statistic */
	private double statisticWidth;

	/* setters (non-java doc) */
	public void setWidthGrid(double newValue) {
		this.widthGrid = newValue;
	}

	public void setHeightGrid(double newValue) {
		this.heightGrid = newValue;
	}

	public void setWidthGridCell(double newValue) {
		this.widthGridCell = newValue;
	}

	public void setHeightGridCell(double newValue) {
		this.heightGridCell = newValue;
	}

	public void setBackgroundColour0(String newColour) {
		this.backgroundColour0 = newColour;
	}
	public void setBackgroundColour1(String newColour) {
		this.backgroundColour1 = newColour;
	}

	public void setStrokeColour(String newColour) {
		this.strokeColour = newColour;
	}

	public void setStrokeWidth(double newValue) {
		this.strokeWidth = newValue;
	}

	public void setFontSize(double newValue) {
		fontSize = newValue;
	}

	public void setFontFamily(String newValue) {
		fontFamily = newValue;
	}

	private void setStatisticWidth(double width) {
		this.statisticWidth = width;
	}

	public double getStatisticWidth() {
		return statisticWidth;
	}
	
	/**
	 * This method returns an element. If the element does not exist, it creates
	 * a new one.
	 * 
	 * @param readDoc
	 *            xml document
	 * @param id
	 *            identifier of the xml element
	 * @param nodeName
	 *            nodeName of the xml element
	 */
	private Element getElement(Document readDoc, String id, String nodeName) {
		Element defs = readDoc.getElementById(id);
		if (defs == null) {
			defs = readDoc.createElement(nodeName);
			defs.setAttribute("id", id);
		}
		return defs;
	}
	

	/**
	 * generate the grid
	 * 
	 * @param readDoc
	 *            document
	 * @param id
	 *            identify the given object
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return grid element
	 */
	private Element createGrid(Document readDoc, String id, int x, int y) {
		Element groupGrid = readDoc.createElement("g");
		Element grid = readDoc.createElement("polygon");
		groupGrid.setAttribute("id", id);
		String points = "0,0" + " " + "0," + heightGrid + " " + widthGrid + ","
				+ heightGrid + " " + widthGrid + ",0" + " 0,0";
		//grid.setAttribute("fill",this.backgroundColour0);
		grid.setAttribute("points", points);
		grid.setAttribute("style", "fill:#"+this.backgroundColour0+"; stroke:" + strokeColour + ";stroke-width:"
				+ strokeWidth + ";");
		groupGrid.appendChild(grid);

		Element defs = getElement(readDoc, "definitions", "defs");
		Element defaultLine1 = readDoc.createElement("line");
		defaultLine1.setAttribute("id", "gridLineX");
		String lineGridY = Double.toString(heightGridCell * y);
		defaultLine1.setAttribute("y1", "0");
		defaultLine1.setAttribute("y2", lineGridY);
		defaultLine1.setAttribute("style", "stroke:" + strokeColour
				+ ";stroke-width:" + strokeWidth + ";");
		defs.appendChild(defaultLine1);

		Element defaultLine2 = readDoc.createElement("line");
		defaultLine2.setAttribute("id", "gridLineY");
		String lineGridX = Double.toString(widthGridCell * x);
		defaultLine2.setAttribute("x1", "0");
		defaultLine2.setAttribute("x2", lineGridX);
		defaultLine2.setAttribute("style", "stroke:" + strokeColour
				+ ";stroke-width:" + strokeWidth + ";");
		defs.appendChild(defaultLine2);
		groupGrid.appendChild(defs);

		for (int i = 1; i < x; i++) {
			Element use = readDoc.createElement("use");
			use.setAttribute("xlink:href", "#gridLineX");
			use.setAttribute("x", Double.toString(widthGridCell * i));
			groupGrid.appendChild(use);
		}
		for (int i = 1; i < y; i++) {
			Element use = readDoc.createElement("use");
			use.setAttribute("xlink:href", "#gridLineY");
			use.setAttribute("y", Double.toString(heightGridCell * i));
			groupGrid.appendChild(use);
		}
		return groupGrid;
	}
	/**
	 * generate the grid
	 * 
	 * @param readDoc
	 *            document
	 * @param id
	 *            identify the given object
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return grid element
	 */
	private Element createGrid_ChessStyle(Document readDoc, String id, int x, int y){
		Element defs = readDoc.createElement("defs");
		Element pattern = readDoc.createElement("pattern");
		pattern.setAttribute("id", "chessboard");
		pattern.setAttribute("x", "0");
		pattern.setAttribute("y", "0");
		pattern.setAttribute("width", ""+this.widthGridCell*2.0);
		pattern.setAttribute("height", ""+this.heightGridCell*2.0);
		pattern.setAttribute("patternUnits", "userSpaceOnUse");
		
		String style0 = "fill:#"+this.backgroundColour0+ ";fill-opacity:1";
		String style1 = "fill:#"+this.backgroundColour1+ ";fill-opacity:1";
		
		Element rect1 = this.createRecht(readDoc, style0, 0.0, 0.0,0.0,0.0);
		Element rect2 = this.createRecht(readDoc, style1, 0.0, 0.0,0.0,this.heightGridCell);
		Element rect3 = this.createRecht(readDoc, style0, 0.0, 0.0,this.widthGridCell,this.heightGridCell);
		Element rect4 = this.createRecht(readDoc, style1, 0.0, 0.0,this.widthGridCell,0.0);
		
		pattern.appendChild(rect1);
		pattern.appendChild(rect2);
		pattern.appendChild(rect3);
		pattern.appendChild(rect4);
		
		defs.appendChild(pattern);
		
		Element root = readDoc.getDocumentElement();
		root.appendChild(defs);
		
		
		Element groupGrid = readDoc.createElement("g");
		groupGrid.setAttribute("id", ""+id);
		Element rect = readDoc.createElement("rect");
		rect.setAttribute("x", "0");
		rect.setAttribute("y", "0");
		rect.setAttribute("width", ""+this.widthGrid);
		rect.setAttribute("height", ""+this.heightGrid);
		rect.setAttribute("fill", "url(#chessboard)");
		
		groupGrid.appendChild(rect);
		
		return groupGrid;
		

	}
	private Element createRecht(Document readDoc , String style , double rx , double ry, double x, double y ){
		Element rect = readDoc.createElement("rect");
		rect.setAttribute("rx", ""+rx);
		rect.setAttribute("ry", ""+ry);
		rect.setAttribute("x", ""+x);
		rect.setAttribute("y", ""+y);
		rect.setAttribute("height", ""+this.heightGridCell);
		rect.setAttribute("width", ""+this.widthGridCell);
		
		rect.setAttribute("style", style);
		return rect;
	}
	/**
	 * generate the grid
	 * 
	 * @param readDoc
	 *            document
	 * @param id
	 *            identify the given object
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return grid element
	 */
	private Element createGrid_ChessStyle2(Document readDoc, String id, int x, int y){
		String style0="fill:#"+this.backgroundColour0+";fill-opacity:1";
		String style1="fill:#"+this.backgroundColour1+";fill-opacity:1";
		 
		Element groupGrid= readDoc.createElement("g");
		groupGrid.setAttribute("id", id);
		for(int i=0; i<x;i++){
			for(int j =0; j<y;j++){
				double a = widthGridCell*i;
				double b = heightGridCell*j;
				Element rect;
				if(i%2==j%2){
				 rect = funcRect(readDoc, a, b, widthGridCell, this.heightGridCell, 0, 0, style0);
				}
				else
				 rect = funcRect(readDoc, a, b, widthGridCell, this.heightGridCell, 0, 0, style1);
				groupGrid.appendChild(rect);
				
			}
		}
		return groupGrid;
	}

	

	/**
	 * generate the grid
	 * 
	 * @param readDoc
	 *            document
	 * @param id
	 *            identify the given object
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return grid element
	 */
	private Element createGrid_ChessStyle3(Document readDoc, String id, int x, int y) {
		Element groupGrid = readDoc.createElement("g");
		Element grid = readDoc.createElement("polygon");
		groupGrid.setAttribute("id", id);
		String points = "0,0" + " " + "0," + heightGrid + " " + widthGrid + ","
				+ heightGrid + " " + widthGrid + ",0" + " 0,0";
		grid.setAttribute("id", "GridWithoutText");
		//grid.setAttribute("fill",this.backgroundColour0);
		grid.setAttribute("points", points);
		grid.setAttribute("style", "fill:#"+this.backgroundColour0+"; stroke:" + strokeColour + ";stroke-width:"
				+ strokeWidth + ";");
		groupGrid.appendChild(grid);

		Element defs = readDoc.createElement("defs");
		Element defaultLine1 = readDoc.createElement("line");
		defaultLine1.setAttribute("id", "gridLineX1");
		String lineGridY = Double.toString(heightGridCell * y);
		defaultLine1.setAttribute("y1", "0");
		defaultLine1.setAttribute("y2", lineGridY);
		defaultLine1.setAttribute("style", "stroke:#" + this.backgroundColour1
				+ ";stroke-width:" + this.widthGridCell + ";");
		defs.appendChild(defaultLine1);

		Element defaultLine3 = readDoc.createElement("line");
		defaultLine3.setAttribute("id", "gridLineX2");
		
		defaultLine3.setAttribute("y1", "0");
		defaultLine3.setAttribute("y2", lineGridY);
		defaultLine3.setAttribute("style", "stroke:#" + this.backgroundColour0
				+ ";stroke-width:" + this.widthGridCell + ";");
		defs.appendChild(defaultLine3);

		
		Element defaultLine2 = readDoc.createElement("line");
		defaultLine2.setAttribute("id", "gridLineY1");
		String lineGridX = Double.toString(widthGridCell * x);
		defaultLine2.setAttribute("x1", "0");
		defaultLine2.setAttribute("x2", lineGridX);
		defaultLine2.setAttribute("style", "stroke:#" + this.backgroundColour1
				+ ";stroke-width:" + this.widthGridCell + ";");
		defs.appendChild(defaultLine2);
		
		Element defaultLine4 = readDoc.createElement("line");
		defaultLine4.setAttribute("id", "gridLineY2");
		
		defaultLine4.setAttribute("x1", "0");
		defaultLine4.setAttribute("x2", lineGridX);
		defaultLine4.setAttribute("style", "stroke:#" + this.backgroundColour0
				+ ";stroke-width:" + this.widthGridCell + ";");
		defs.appendChild(defaultLine4);
		groupGrid.appendChild(defs);

		for (int i = 1; i < x; i++) {
			Element use = readDoc.createElement("use");
			if(i%2 == 0){
			use.setAttribute("xlink:href", "#gridLineX1");
			use.setAttribute("x", Double.toString(widthGridCell * i));
			
			}
			else{
				use.setAttribute("xlink:href", "#gridLineX2");
				use.setAttribute("x", Double.toString(widthGridCell * i));
			}
			groupGrid.appendChild(use);
		}

		int a = 1 ;
		int b = 0;
		 while(a<y){
				if(b%2==0){
					Element use = readDoc.createElement("use");
					use.setAttribute("xlink:href", "#gridLineY1");
					use.setAttribute("y", Double.toString(heightGridCell * a));
					groupGrid.appendChild(use);
				}else {
					
						Element use = readDoc.createElement("use");
						use.setAttribute("xlink:href", "#gridLineY2");
						use.setAttribute("y", Double.toString(heightGridCell * a));
						groupGrid.appendChild(use);
					
				}
			 b+=1;
			 a +=2;
		 }
		return groupGrid;
	}
	/**
	 * methode to generate a grid without text
	 * 
	 * @param readDoc
	 * @param id
	 *            identify the given object
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return
	 */
	public Document makeDrawGrid(Document readDoc, String id, int x, int y) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element groupGrid = createGrid(readDoc, id, x, y);
	//	Element groupGrid = this.createGrid_ChessStyle(readDoc, id, x, y);
		//Element groupGrid = this.createGrid_ChessStyle3(readDoc, id, x, y);
		mainGroup.appendChild(groupGrid);
		
	//	Element g = this.createGrid(readDoc, ""+1, x, y);
	//	mainGroup.appendChild(g);
		
		
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * methode to generate a grid with text
	 * 
	 * @param readDoc
	 * @param id
	 *            identify the given object
	 * @param text
	 *            text under the grid
	 * @param x
	 *            number of cells (horizontal)
	 * @param y
	 *            number of cells (vertical)
	 * @return
	 */
	public Document makeDrawGrid(Document readDoc, String id, String text,
			int x, int y) {
		Node root = readDoc.getDocumentElement();
		double halfGrid = widthGrid / 2;
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element groupGrid = createGrid(readDoc, id, x, y);
		Element textElement = funcText(readDoc, "textGrid", halfGrid,
				(heightGrid + fontSize), text, "font-family:" + fontFamily
						+ ";font-size:" + fontSize + ";");
		groupGrid.appendChild(textElement);
		mainGroup.appendChild(groupGrid);
		root.appendChild(mainGroup);
		return readDoc;
	}

	public Document setGridStatistic(int x, int y, Document readDoc,
			Map<String, String> myMap) {
		/* set Letters dim */
		double letters =  this.fontSize;
		
		// int cutKey = 25;
		// int cutValue = 25;

		/* calcualte the vertical position for the statistic */
		double posX1 = widthGridCell * x + 30;

		/* horizontal start position of the statistic */
		double posY = heightGridCell + letters;

		/* is the space between the lines */
		int addposY = 15;
		/* copy the map */
		Map<String, String> myMapCopy = myMap;
		/* min number of letters in a line for the key */
		int cutKey = 0;
		/* min number of letters in a line for the value */
		int cutValue = 0;

		Set<String> eCopy = myMapCopy.keySet();
		Iterator<String> bCopy = eCopy.iterator();
		/* search the max length of the key and value */

		/*
		 * hier berechnet längste zeile sollte gemacht werden wenn cutWert
		 * nicht gesetzt wurde
		 */
		while (bCopy.hasNext()) {
			String keyCopy = bCopy.next();
			String valueCopy = myMapCopy.get(keyCopy);
			if (keyCopy.length() > cutKey) {
				cutKey = keyCopy.length();
			}
			if (valueCopy.length() > cutValue) {
				cutValue = valueCopy.length();
			}
		}
		/* ende */
		double posX2 = posX1 + (letters - 4) * cutKey;

		setStatisticWidth(posX2 + (cutValue * (letters - 4)));
		Set<String> e = myMap.keySet();
		Iterator<String> b = e.iterator();

		String stringPosX1 = Double.toString(posX1);
		String stringPosX2 = Double.toString(posX2);
		boolean finished = true;
		int linenumberKey = 0;
		int linenumberValue = 0;

		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", "statistik");
		Node root = readDoc.getDocumentElement();

		while (b.hasNext()) {
			String key = b.next();
			String value = myMap.get(key);

			if (key.length() > cutKey) {
				while (finished) {
					String keyPart = key.substring(0, cutKey);
					String keyRest = key.substring(cutKey, key.length());
					key = keyRest;

					String stringPosY = Double.toString(posY);

					Text keyText1 = readDoc.createTextNode(keyPart);
					Element KeyTextElement1 = readDoc.createElement("text");
					KeyTextElement1.setAttribute("x", stringPosX1);
					KeyTextElement1.setAttribute("y", stringPosY);// font-family:Arial;font-style:oblique;
					KeyTextElement1.setAttribute("style",
							"font-family:Arial Unicode;font-size:" + letters
									+ "px;");
					KeyTextElement1.appendChild(keyText1);
					group.appendChild(KeyTextElement1);

					linenumberKey = linenumberKey + 1;
					posY = posY + addposY;
					if (key.length() <= cutKey) {
						finished = false;
					}
				}
			}
			String stringPosY2 = Double.toString(posY);
			// System.out.println("Y "+posY);
			if (key.length() != 0) {
				Text keyText = readDoc.createTextNode(key);
				Element KeyTextElement = readDoc.createElement("text");
				KeyTextElement.setAttribute("x", stringPosX1);
				KeyTextElement.setAttribute("y", stringPosY2);// font-family:Arial;font-style:oblique;
				KeyTextElement.setAttribute("style",
						"font-family:Arial Unicode;font-size:" + letters
								+ "px;");
				KeyTextElement.appendChild(keyText);
				group.appendChild(KeyTextElement);
			}

			// reset the line to the top of the column
			// hiermit wird in der value-spalte in der gleichen zeile wie in
			// key-spalte begonnen
			posY = posY - addposY * linenumberKey;
			finished = true;

			if (value.length() > cutValue) {
				while (finished) {
					String valuePart = value.substring(0, cutValue);
					String valueRest = value
							.substring(cutValue, value.length());
					value = valueRest;

					String stringPosY = Double.toString(posY);

					Text valueText1 = readDoc.createTextNode(valuePart);
					Element valueTextElement1 = readDoc.createElement("text");
					valueTextElement1.setAttribute("x", stringPosX2);
					valueTextElement1.setAttribute("y", stringPosY);// font-family:Arial;font-style:oblique;
					valueTextElement1.setAttribute("style",
							"font-family:Arial Unicode;font-size:" + letters
									+ "px;");
					valueTextElement1.appendChild(valueText1);
					group.appendChild(valueTextElement1);
					linenumberValue = linenumberValue + 1;
					posY = posY + addposY;
					if (value.length() <= cutValue) {
						finished = false;
					}
				}
			}
			stringPosY2 = Double.toString(posY);
			if (value.length() != 0) {
				Text valueText = readDoc.createTextNode(value);
				Element valueTextElement = readDoc.createElement("text");
				valueTextElement.setAttribute("x", stringPosX2);
				valueTextElement.setAttribute("y", stringPosY2);// font-family:Arial;font-style:oblique;
				valueTextElement.setAttribute("style",
						"font-family:Arial Unicode;font-size:" + letters
								+ "px;");
				valueTextElement.appendChild(valueText);
				group.appendChild(valueTextElement);
			}
			posY = posY - addposY * linenumberValue;
			finished = true;

			if (linenumberKey > linenumberValue) {
				posY = posY + addposY * linenumberKey + addposY + addposY;
				linenumberValue = 0;
				linenumberKey = 0;
			} else {
				posY = posY + addposY * linenumberValue + addposY + addposY;
				linenumberValue = 0;
				linenumberKey = 0;
			}
		}

		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

}