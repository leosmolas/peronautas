package massim.visualization;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * 
 * This class is the mainclass to draw an agent. Its provide mehtods to create
 * svg elements
 * 
 */

public class Agent extends massim.visualization.svg.SvgFunction {

	/* default values (can be edit by config-file) */
	private double dimBoxWidth;
	private double dimBoxHeight;
	private double dimLineX = 2;
	@SuppressWarnings("unused")
	private double dimLineY = 2;
	private String labelColour = "#FEEFB8";
	private String labelTextColour = "black";
	private String labelHeadColour = "#FEEFB8";
	private String labelHeadTextColour = "black";

	/*
	 * (non-Javadoc) only setters and getters
	 */
	public void setDimBoxHeight(double dimBoxHeight) {
		this.dimBoxHeight = dimBoxHeight;
	}

	public void setDimBoxWidth(double dimBoxWidth) {
		this.dimBoxWidth = dimBoxWidth;
	}

	public void setDimLineY(double dimLineY) {
		this.dimLineY = dimLineY;
	}

	public void setDimLine() {
		dimLineX = dimBoxWidth / 21;
		dimLineY = dimBoxHeight / 21;
	}

	public void setLabelColour(String labelColour) {
		this.labelColour = labelColour;
	}

	public void setLabelHeadColour(String labelHeadColour) {
		this.labelHeadColour = labelHeadColour;
	}

	public void setLabelHeadTextColour(String labelHeadTextColour) {
		this.labelHeadTextColour = labelHeadTextColour;
	}

	public void setLabelTextColour(String labelTextColour) {
		this.labelTextColour = labelTextColour;
	}

	/**
	 * end setters
	 */

	/**
	 * calculate a colour in dependency of the id
	 * 
	 * @param id
	 *            calculate from given id the colour for the agent
	 * @return calculated colour
	 */
	private String getColour(long id) {
		long colourValue1 = (id * 233 + 5 * id) % 255;
		long colourValue2 = (id * 421 / 5 + 69) % 255;
		long colourValue3 = (id + 32 * id) % 255;
		String colour = "rgb(" + colourValue1 + "," + colourValue2 + ","
				+ colourValue3 + ")";
		return colour;
	}

	/**
	 * create the label for the agent
	 * 
	 * @param doc
	 * @param x
	 *            horizontal position of the label (cell number)
	 * @param y
	 *            vertical position of the label (cell number)
	 * @param theText
	 *            the text on the label
	 * @return label element
	 */
	private Element label(Document doc, int x, int y, String theText) {
		/* position of the label */
		double startPointX = (dimBoxWidth * x) + 2;
		double endPointX = (dimBoxWidth * (x + 1)) - 2;
		double width = endPointX - startPointX;
		double startPointY = (dimBoxHeight * y) + dimBoxHeight / 2;
		double endPointY = (dimBoxHeight * y) + dimBoxHeight / 1.11;
		double height = endPointY - startPointY;
		/* text style */
		String style = "fill:" + labelColour + "; stroke:black;";
		String styleText;
		/* fit text to label */
		if (dimBoxWidth < dimBoxHeight)
			styleText = " font-family:Arial Unicode;fill:" + labelTextColour
					+ ";font-size:" + dimBoxWidth / 3.5
					+ "px;text-anchor:middle;letter-spacing:-1.5;";
		else
			styleText = "font-family:Arial Unicode;fill:" + labelTextColour
					+ ";font-size:" + dimBoxHeight / 3.5
					+ "px;text-anchor:middle;letter-spacing:-1.5;";
		Element group = doc.createElement("g");
		Element label = funcRect(doc, startPointX, startPointY, width, height,
				4, 4, style);
		Element text = funcText(doc, (dimBoxWidth * x + dimBoxWidth / 2),
				endPointY - dimBoxHeight / 21, theText, styleText);
		group.appendChild(label);
		group.appendChild(text);
		return group;
	}

	/**
	 * create the body of the gold digger
	 * 
	 * @param readDoc
	 * @param colour
	 *            colour to fill the body
	 * @param x
	 *            horizontal position of the body (cell number)
	 * @param y
	 *            vertical position of the body (cell number)
	 * @return body element
	 */
	private Element body(Document readDoc, String colour, int x, int y) {
		Element bodyStruct = readDoc.createElement("g");

		/* generate the head of the gold digger */
		/* calculate position on grid */
		double pointX = dimBoxWidth * x + dimBoxWidth / 2;
		double pointY = dimBoxHeight * y + dimBoxHeight / 2;

		Element rec = funcEllipse(readDoc, pointX, pointY, dimBoxWidth / 4,
				dimBoxHeight / 4, "fill:" + colour + ";");
		bodyStruct.appendChild(rec);

		/*
		 * double dimLineLowValue; if(dimLineX < dimLineY) { dimLineLowValue =
		 * dimLineX; } else { dimLineLowValue = dimLineY; } /* draw the mainbody
		 * as an rectangle / String stringPoint = (dimBoxWidth * x + dimBoxWidth /
		 * 2 - dimBoxWidth/10) +","+(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/6) +" "+(dimBoxWidth * x + dimBoxWidth / 2 +
		 * dimBoxWidth/10) +","+(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/6) +" "+(dimBoxWidth * x + dimBoxWidth / 2 +
		 * dimBoxWidth/10) +","+(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(2.5)) +" "+(dimBoxWidth * x + dimBoxWidth / 2 -
		 * dimBoxWidth/10) +","+(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(2.5)); String style =
		 * "stroke:"+colour+";stroke-width:"+dimLineLowValue+";fill:"+colour+";";
		 * Element body = funcPolygon(readDoc,stringPoint,style);
		 * bodyStruct.appendChild(body);
		 * 
		 * //draw left leg String stringLeftLegX1 = Double.toString(dimBoxWidth *
		 * x + dimBoxWidth / 2 - dimBoxWidth/21); String stringLeftLegX2 =
		 * Double.toString(dimBoxWidth * x + dimBoxWidth / 2 - dimBoxWidth/10);
		 * String stringLeftLegY1 = Double.toString(dimBoxHeight * y +
		 * dimBoxHeight / 4 + dimBoxHeight/(2.5)); String stringLeftLegY2 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(1.4));
		 * 
		 * //draw right leg String stringRightLegX1 =
		 * Double.toString(dimBoxWidth * x + dimBoxWidth / 2 + dimBoxWidth/21);
		 * String stringRightLegX2 = Double.toString(dimBoxWidth * x +
		 * dimBoxWidth / 2 + dimBoxWidth/10); String stringRightLegY1 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(2.5)); String stringRightLegY2 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(1.4));
		 * 
		 * //draw right arm String stringRightArmX1 =
		 * Double.toString(dimBoxWidth * x + dimBoxWidth / 2 + dimBoxWidth/10);
		 * String stringRightArmX2 = Double.toString(dimBoxWidth * x +
		 * dimBoxWidth / 2 + dimBoxWidth/(3.8)); String stringRightArmY1 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(4.2)); String stringRightArmY2 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/6);
		 * 
		 * //draw left arm String stringLeftArmX1 = Double.toString(dimBoxWidth *
		 * x + dimBoxWidth / 2 - dimBoxWidth/10); String stringLeftArmX2 =
		 * Double.toString(dimBoxWidth * x + dimBoxWidth / 2 -
		 * dimBoxWidth/(3.8)); String stringLeftArmY1 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/(4.2)); String stringLeftArmY2 =
		 * Double.toString(dimBoxHeight * y + dimBoxHeight / 4 +
		 * dimBoxHeight/6); // add points to a string String legPoints ="M"+
		 * stringLeftLegX1 +" "+ stringLeftLegY1 +" L"+ stringLeftLegX2 +" "+
		 * stringLeftLegY2 +" M"+ stringRightLegX1 +" "+ stringRightLegY1 +" L"+
		 * stringRightLegX2 +" "+ stringRightLegY2; String edgePoints = "M" +
		 * stringRightArmX1 + " " + stringRightArmY1 +" L" + stringRightArmX2 + " " +
		 * stringRightArmY2 +" M" + stringLeftArmX1 + " " + stringLeftArmY1 +"
		 * L" + stringLeftArmX2 + " " + stringLeftArmY2;;
		 * 
		 * Element leg =
		 * funcPath(readDoc,legPoints,"stroke:"+colour+";stroke-width:"+dimLineX+";");
		 * Element edge =
		 * funcPath(readDoc,edgePoints,"stroke:"+colour+";stroke-width:"+dimLineY+";");
		 * bodyStruct.appendChild(leg); bodyStruct.appendChild(edge);
		 */
		return bodyStruct;
	}

	/**
	 * create the goldbag of the gold digger
	 * 
	 * @param readDoc
	 * @param x
	 *            horizontal position of the goldbag (cell number)
	 * @param y
	 *            vertical position of the goldbag (cell number)
	 * @return goldbag element
	 */
	private Element goldBag(Document readDoc, int x, int y) {
		Element g = readDoc.createElement("g");
		/* calculate position of goldbag on the grid */
		double pointX = dimBoxWidth * x;
		double pointY = dimBoxHeight * y;

		/* svg path element - path arround the bag */
		Element bag = readDoc.createElement("path");
		bag.setAttribute("id", "bag");

		String points1 = "M" + (pointX + 0.82 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + "C"
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + " "
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + " "
				+ (pointX + 0.77 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + "C"
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + " "
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.27 * dimBoxHeight) + " "
				+ (pointX + 0.72 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + "C"
				+ (pointX + 0.71 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + " "
				+ (pointX + 0.73 * dimBoxWidth) + ","
				+ (pointY + 0.31 * dimBoxHeight) + " "
				+ (pointX + 0.74 * dimBoxWidth) + ","
				+ (pointY + 0.32 * dimBoxHeight) + "C"
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.32 * dimBoxHeight) + " "
				+ (pointX + 0.76 * dimBoxWidth) + ","
				+ (pointY + 0.33 * dimBoxHeight) + " "
				+ (pointX + 0.76 * dimBoxWidth) + ","
				+ (pointY + 0.35 * dimBoxHeight) + "C"
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.37 * dimBoxHeight) + " "
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.37 * dimBoxHeight) + " "
				+ (pointX + 0.73 * dimBoxWidth) + ","
				+ (pointY + 0.37 * dimBoxHeight) + "C"
				+ (pointX + 0.71 * dimBoxWidth) + ","
				+ (pointY + 0.38 * dimBoxHeight) + " "
				+ (pointX + 0.70 * dimBoxWidth) + ","
				+ (pointY + 0.39 * dimBoxHeight) + " "
				+ (pointX + 0.69 * dimBoxWidth) + ","
				+ (pointY + 0.40 * dimBoxHeight) + "C"
				+ (pointX + 0.67 * dimBoxWidth) + ","
				+ (pointY + 0.40 * dimBoxHeight) + " "
				+ (pointX + 0.67 * dimBoxWidth) + ","
				+ (pointY + 0.41 * dimBoxHeight) + " "
				+ (pointX + 0.67 * dimBoxWidth) + ","
				+ (pointY + 0.44 * dimBoxHeight) + "C"
				+ (pointX + 0.67 * dimBoxWidth) + ","
				+ (pointY + 0.45 * dimBoxHeight) + " "
				+ (pointX + 0.66 * dimBoxWidth) + ","
				+ (pointY + 0.48 * dimBoxHeight) + " "
				+ (pointX + 0.67 * dimBoxWidth) + ","
				+ (pointY + 0.49 * dimBoxHeight) + "C"
				+ (pointX + 0.69 * dimBoxWidth) + ","
				+ (pointY + 0.50 * dimBoxHeight) + " "
				+ (pointX + 0.68 * dimBoxWidth) + ","
				+ (pointY + 0.51 * dimBoxHeight) + " "
				+ (pointX + 0.70 * dimBoxWidth) + ","
				+ (pointY + 0.52 * dimBoxHeight) + "C"
				+ (pointX + 0.70 * dimBoxWidth) + ","
				+ (pointY + 0.53 * dimBoxHeight) + " "
				+ (pointX + 0.72 * dimBoxWidth) + ","
				+ (pointY + 0.53 * dimBoxHeight) + " "
				+ (pointX + 0.74 * dimBoxWidth) + ","
				+ (pointY + 0.53 * dimBoxHeight) + "C"
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.54 * dimBoxHeight) + " "
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.54 * dimBoxHeight) + " "
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.54 * dimBoxHeight) + "C"
				+ (pointX + 0.82 * dimBoxWidth) + ","
				+ (pointY + 0.54 * dimBoxHeight) + " "
				+ (pointX + 0.85 * dimBoxWidth) + ","
				+ (pointY + 0.54 * dimBoxHeight) + " "
				+ (pointX + 0.86 * dimBoxWidth) + ","
				+ (pointY + 0.53 * dimBoxHeight) + "C"
				+ (pointX + 0.88 * dimBoxWidth) + ","
				+ (pointY + 0.52 * dimBoxHeight) + " "
				+ (pointX + 0.89 * dimBoxWidth) + ","
				+ (pointY + 0.51 * dimBoxHeight) + " "
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.50 * dimBoxHeight) + "C"
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.50 * dimBoxHeight) + " "
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.47 * dimBoxHeight) + " "
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.45 * dimBoxHeight) + "C"
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.44 * dimBoxHeight) + " "
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.42 * dimBoxHeight) + " "
				+ (pointX + 0.90 * dimBoxWidth) + ","
				+ (pointY + 0.40 * dimBoxHeight) + "C"
				+ (pointX + 0.89 * dimBoxWidth) + ","
				+ (pointY + 0.39 * dimBoxHeight) + " "
				+ (pointX + 0.88 * dimBoxWidth) + ","
				+ (pointY + 0.38 * dimBoxHeight) + " "
				+ (pointX + 0.87 * dimBoxWidth) + ","
				+ (pointY + 0.38 * dimBoxHeight) + "C"
				+ (pointX + 0.85 * dimBoxWidth) + ","
				+ (pointY + 0.37 * dimBoxHeight) + " "
				+ (pointX + 0.83 * dimBoxWidth) + ","
				+ (pointY + 0.36 * dimBoxHeight) + " "
				+ (pointX + 0.82 * dimBoxWidth) + ","
				+ (pointY + 0.36 * dimBoxHeight) + "C"
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.35 * dimBoxHeight) + " "
				+ (pointX + 0.81 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.8 * dimBoxWidth) + ","
				+ (pointY + 0.32 * dimBoxHeight) + "C"
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.30 * dimBoxHeight) + " "
				+ (pointX + 0.84 * dimBoxWidth) + ","
				+ (pointY + 0.27 * dimBoxHeight) + " "
				+ (pointX + 0.82 * dimBoxWidth) + ","
				+ (pointY + 0.28 * dimBoxHeight) + "z";

		bag.setAttribute("d", points1);
		bag
				.setAttribute(
						"style",
						"fill:gold;stroke:black;stroke-width:0.5;stroke-linecap:butt;stroke-linejoin:miter");

		/* give the braid of bag */
		Element braid = readDoc.createElement("path");
		String points2 = "M" + (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + "C"
				+ (pointX + 0.75 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.76 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.76 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + "C"
				+ (pointX + 0.77 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.77 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.78 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + "C"
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.79 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + "C"
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + "C"
				+ (pointX + 0.80 * dimBoxWidth) + ","
				+ (pointY + 0.34 * dimBoxHeight) + " "
				+ (pointX + 0.81 * dimBoxWidth) + ","
				+ (pointY + 0.33 * dimBoxHeight) + " "
				+ (pointX + 0.81 * dimBoxWidth) + ","
				+ (pointY + 0.33 * dimBoxHeight);
		braid.setAttribute("d", points2);
		braid
				.setAttribute(
						"style",
						"fill:none;stroke:red;stroke-width:0.5;stroke-linecap:butt;stroke-linejoin:miter;");

		g.appendChild(bag);
		g.appendChild(braid);
		return g;
	}

	/**
	 * create the axe of the gold digger
	 * 
	 * @param readDoc
	 * @param colour
	 * @param x
	 *            horizontal position of the axe (cell number)
	 * @param y
	 *            vertical position of the axe (cell number)
	 * @return
	 */
	private Element cowboyhat(Document readDoc, String colour, int x, int y) {
		/* calculate position of the axe in the cell */
		String stringGripAxeX1 = Double.toString(dimBoxWidth * x + 11
				* dimBoxWidth / 14);
		String stringGripAxeY1 = Double.toString(dimBoxHeight * y
				+ dimBoxHeight / 4);
		String stringGripAxeY2 = Double.toString(dimBoxHeight * y
				+ dimBoxHeight / 2);

		Element g = readDoc.createElement("g");
		g.setAttribute("id", "agentAxe");

		// draw the axe
		Element axe = readDoc.createElement("path");
		String pointsAxe = " M" + stringGripAxeX1 + " " + stringGripAxeY1
				+ " L" + stringGripAxeX1 + " " + stringGripAxeY2;
		axe.setAttribute("d", pointsAxe);
		axe.setAttribute("style", "stroke:" + colour + ";stroke-width:"
				+ dimLineX + ";fill:" + colour + ";");

		// draw cutting edge
		Element cuttingEdge = readDoc.createElement("path");
		double pointY = (dimBoxHeight / 4 + dimBoxHeight * y);
		double pointX = (dimBoxWidth * x + 9 * dimBoxWidth / 14);
		String stringPoints = "M" + pointX + " " + pointY + " L"
				+ (pointX + dimBoxWidth / 3.5) + " " + (pointY) + " L"
				+ (pointX + dimBoxWidth / 21) + " "
				+ (pointY - dimBoxHeight / 14) + "Z";
		cuttingEdge.setAttribute("d", stringPoints);
		cuttingEdge.setAttribute("style", "stroke:" + colour
				+ ";stroke-width:1;fill:" + colour + ";");

		g.appendChild(axe);
		g.appendChild(cuttingEdge);
		return g;
	}

	/**
	 * create the label inside the head (background for the number (agent ID))
	 * 
	 * @param readDoc
	 * @param x
	 *            horizontal position of the agent (because number in head)
	 *            (cell number)
	 * @param y
	 *            vertical position of the agent (because number in head) (cell
	 *            number)
	 * @return agentNumberLabel element
	 */
	private Element agentNumberLabel(Document readDoc, int x, int y) {
		double pointX = dimBoxWidth * x + dimBoxWidth / 2;
		double pointY = dimBoxHeight * y + dimBoxHeight / 2;
		Element rec = funcEllipse(readDoc, pointX, pointY, dimBoxWidth / 7,
				dimBoxHeight / 7, "fill:" + labelHeadColour + ";");
		return rec;
	}

	/**
	 * create the "head label" which is use to know which agent will be show (by
	 * an ID number in head)
	 * 
	 * @param readDoc
	 * @param agentId
	 *            the agentId will be show in the head as ID
	 * @param x
	 *            horizontal position of the agent (because number in head)
	 *            (cell number)
	 * @param y
	 *            vertical position of the agent (because number in head) (cell
	 *            number)
	 * @return
	 */
	private Element agentNumberHeat(Document readDoc, long agentId, int x, int y) {
		double xpos = (dimBoxWidth * x) + dimBoxWidth / 2;
		double ypos;
		/* set style for the text (number) in head */
		String style;
		if (dimBoxWidth < dimBoxHeight) {
			style = "font-family:Arial Unicode;fill:" + labelHeadTextColour
					+ ";font-size:" + dimBoxWidth / 6
					+ "px;text-anchor:middle;";
			ypos = dimBoxHeight * y + dimBoxHeight / 2;
		} else {
			style = "font-family:Arial Unicode;fill:" + labelHeadTextColour
					+ ";font-size:" + dimBoxHeight / 6
					+ "px;text-anchor:middle;";
			ypos = dimBoxHeight * y + dimBoxHeight / 1.5;
		}
		Element textOnLabel = funcText(readDoc, xpos, ypos, Long
				.toString(agentId), style);
		return textOnLabel;
	}

	// ****** gold digger with an axe
	// * in this part we "create" the agent. He looks like a gold digger with an
	// axe.
	// * To do this we use the methode defined before.

	/**
	 * create the gold digger
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param x
	 *            horizontal position of the goldDigger (cell number)
	 * @param y
	 *            vertical position of the goldDigger (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @param goldDigger
	 *            element
	 */
	public Document goldDigger(Document readDoc, long id, String teamId, int x,
			int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", Long.toString(id));
		Element body = body(readDoc, teamId, x, y);
		Element hat = cowboyhat(readDoc, teamId, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(hat);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * create the gold digger
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param x
	 *            horizontal position of the goldDigger (cell number)
	 * @param y
	 *            vertical position of the goldDigger (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return goldDigger Element
	 */
	public Document goldDigger(Document readDoc, long id, long teamId, int x,
			int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id + "_" + teamId);
		String colour = getColour(teamId);
		Element body = body(readDoc, colour, x, y);
		Element hat = cowboyhat(readDoc, colour, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(hat);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * create the gold digger with label
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param text
	 *            text on the label of the agent
	 * @param x
	 *            horizontal position of the goldDigger (cell number)
	 * @param y
	 *            vertical position of the goldDigger (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return goldDigger element
	 */
	public Document goldDigger(Document readDoc, long id, String teamId,
			String text, int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", Long.toString(id));
		Element body = body(readDoc, teamId, x, y);
		Element hat = cowboyhat(readDoc, teamId, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);
		Element label = label(readDoc, x, y, text);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(hat);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * create the gold digger with label
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param text
	 *            text on the label of the agent
	 * @param x
	 *            horizontal position of the goldDigger (cell number)
	 * @param y
	 *            vertical position of the goldDigger (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return
	 */
	public Document goldDigger(Document readDoc, long id, long teamId,
			String text, int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id + "_" + teamId);
		String colour = getColour(teamId);
		Element body = body(readDoc, colour, x, y);
		Element hat = cowboyhat(readDoc, colour, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);
		Element label = label(readDoc, x, y, text);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(hat);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	// ****** gold digger with a bag
	// * in this part we "create" the agent. He looks like a gold digger with a
	// bag.
	// * To do this we use the methode defined before.

	/**
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param x
	 *            horizontal position of the goldDiggerWithGold (cell number)
	 * @param y
	 *            vertical position of the goldDiggerWithGold (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 */
	public Document goldDiggerWithGold(Document readDoc, long id, long teamId,
			int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id + "_" + teamId);
		String colour = getColour(teamId);
		Element body = body(readDoc, colour, x, y);
		Element bag = goldBag(readDoc, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(bag);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param x
	 *            horizontal position of the goldDiggerWithGold (cell number)
	 * @param y
	 *            vertical position of the goldDiggerWithGold (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return
	 */
	public Document goldDiggerWithGold(Document readDoc, long id,
			String teamId, int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", "" + id);

		Element body = body(readDoc, teamId, x, y);
		Element bag = goldBag(readDoc, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(bag);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param text
	 *            text on the label of the agent
	 * @param x
	 *            horizontal position of the goldDiggerWithGold (cell number)
	 * @param y
	 *            vertical position of the goldDiggerWithGold (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return
	 */
	public Document goldDiggerWithGold(Document readDoc, long id, long teamId,
			String text, int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id + "_" + teamId);
		String colour = getColour(teamId);
		Element body = body(readDoc, colour, x, y);
		Element bag = goldBag(readDoc, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);
		Element label = label(readDoc, x, y, text);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(bag);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * 
	 * @param readDoc
	 * @param id
	 *            identical the agent
	 * @param teamId
	 *            identical the team of the agent
	 * @param text
	 *            text on the label of the agent
	 * @param x
	 *            horizontal position of the goldDiggerWithGold (cell number)
	 * @param y
	 *            vertical position of the goldDiggerWithGold (cell number)
	 * @param rotate
	 *            NOTE: "N"|"n" rotate agent North, "E"|"e" rotate agent East,
	 *            "S"|"s" rotate agent South, "W"|"w" rotate agent West "12"
	 *            rotate agent 12 degree rightwards
	 * @return
	 */
	public Document goldDiggerWithGold(Document readDoc, long id,
			String teamId, String text, int x, int y, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", "" + id);

		Element body = body(readDoc, teamId, x, y);
		Element bag = goldBag(readDoc, x, y);
		Element agentNumber = agentNumberHeat(readDoc, id, x, y);
		Element agentNumberLabel = agentNumberLabel(readDoc, x, y);
		Element label = label(readDoc, x, y, text);

		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		group.appendChild(body);
		group.appendChild(bag);
		group.appendChild(agentNumberLabel);
		group.appendChild(agentNumber);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}
}
