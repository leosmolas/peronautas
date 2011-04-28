package massim.visualization;

import massim.visualization.svg.SvgFunction;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class ItemsAndObstacles extends SvgFunction {
	private double dimBoxHeight = 42;
	private double dimBoxWidth = 42;

	/*
	 * (non-Javadoc) only setters and getters
	 */
	public void setDimBoxHeight(double dimBoxHeight) {
		this.dimBoxHeight = dimBoxHeight;
	}

	public void setDimBoxWidth(double dimBoxWidth) {
		this.dimBoxWidth = dimBoxWidth;
	}

	/*
	 * end setters and getters
	 */

	// ****** help functions ******//
	private Element getLabel(Document doc, int x, int y, String theText) {
		double startPointX = (dimBoxWidth * x) + 2;
		double endPointX = (dimBoxWidth * (x + 1)) - 2;
		double width = endPointX - startPointX;
		double startPointY = (dimBoxHeight * y) + dimBoxHeight / 2;
		double endPointY = (dimBoxHeight * y) + dimBoxHeight / 1.11;
		double height = endPointY - startPointY;
		String style = "fill:#FEEFB8; stroke:black;";
		String styleText;
		if (dimBoxWidth < dimBoxHeight)
			styleText = "font-family:Arial Unicode;font-size:" + dimBoxWidth
					/ 3.5 + "px;text-anchor:middle;letter-spacing:-1.5;";
		else
			styleText = "font-family:Arial Unicode;font-size:" + dimBoxHeight
					/ 3.5 + "px;text-anchor:middle;letter-spacing:-1.5;";
		Element group = doc.createElement("g");
		Element label = funcRect(doc, startPointX, startPointY, width, height,
				4, 4, style);
		Element text = funcText(doc, (dimBoxWidth * x + dimBoxWidth / 2),
				endPointY - dimBoxHeight / 21, theText, styleText);
		group.appendChild(label);
		group.appendChild(text);
		return group;
	}

	// ****** item gold depot ******//

	public Document makeGoldDepot(Document readDoc, String id, int x, int y,
			boolean use, String rotate) {
		/* get root-element from xml-file */
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		if (!use) {
			Element useFalse = makeGoldDepotDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeGoldDepotUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	public Document makeGoldDepot(Document readDoc, String id, String text,
			int x, int y, boolean use, String rotate) {
		/* get root-element from xml-file */
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		if (use == false) {
			Element useFalse = makeGoldDepotDefs(readDoc, id);
			group.appendChild(useFalse);
			Element useTrue = makeGoldDepotUse(readDoc, id, x, y);
			mainGroup.appendChild(useTrue);
		} else if (use == true) {
			Element useTrue = makeGoldDepotUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		}
		Element label = getLabel(readDoc, x, y, text);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeGoldDepotUse(Document readDoc, String id, double x,
			double y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		use.setAttribute("x", Double.toString(x * dimBoxWidth));
		use.setAttribute("y", Double.toString(y * dimBoxHeight));
		return use;
	}

	private Element makeGoldDepotDefs(Document readDoc, String id) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		String stringPath1 = "M" + (dimBoxWidth / 10.5) + " " + (dimBoxHeight)
				+ " L" + (dimBoxWidth / 1.5) + " " + (dimBoxHeight) + " L"
				+ (0.9 * dimBoxWidth) + " "
				+ (dimBoxHeight - dimBoxHeight / 5.25) + " L"
				+ (0.9 * dimBoxWidth) + " " + (dimBoxHeight / 6) + " L"
				+ (dimBoxWidth / 2.8) + " " + (dimBoxHeight / 6) + " L"
				+ (dimBoxWidth / 10.5) + " " + (dimBoxHeight / 2.8) + " L"
				+ (dimBoxWidth / 10.5) + " " + (dimBoxHeight);
		Element path1 = funcPath(readDoc, stringPath1,
				"fill:darkgray;stroke:black;stroke-width:2;");

		String stringPath2 = "M" + (dimBoxWidth / 10.5) + " "
				+ (dimBoxHeight / 2.8) + " L" + (dimBoxWidth / 1.5) + " "
				+ (dimBoxHeight / 2.8) + " L" + (0.9 * dimBoxWidth) + " "
				+ (dimBoxHeight / 6) + " M" + (dimBoxWidth / 1.5) + " "
				+ (dimBoxHeight / 2.8) + " L" + (dimBoxWidth / 1.5) + " "
				+ (dimBoxHeight);
		Element path2 = funcPath(readDoc, stringPath2,
				"fill:darkgray;stroke:black;stroke-width:2;");

		String stringPoints = (dimBoxWidth / 3.2) + "," + dimBoxHeight + " "
				+ (dimBoxWidth / 2.2) + "," + dimBoxHeight + " "
				+ (dimBoxWidth / 2.2) + ","
				+ (dimBoxHeight - dimBoxHeight / 10.5) + " "
				+ (dimBoxWidth / 3.2) + ","
				+ (dimBoxHeight - dimBoxHeight / 10.5);
		Element polygon = funcPolygon(readDoc, stringPoints,
				"fill:darkgray;stroke:black;stroke-width:2;fill:black");

		Element ellipse = funcEllipse(readDoc, dimBoxWidth / 2.625,
				dimBoxHeight - dimBoxHeight / 2.7, dimBoxWidth / 4.2,
				dimBoxHeight / 4.2, "fill:gold");

		double textSize = 22;
		if (dimBoxHeight < dimBoxWidth) {
			textSize = dimBoxHeight / 1.9;
		} else {
			textSize = dimBoxWidth / 1.9;
		}

		Element text = funcText(readDoc, dimBoxWidth / 2.625,
				dimBoxHeight / 1.24, "$",
				"text-anchor:middle;font-family:Arial Unicode;font-size:"
						+ textSize + ";");

		group.appendChild(path1);
		group.appendChild(path2);
		group.appendChild(polygon);
		group.appendChild(ellipse);
		group.appendChild(text);
		defs.appendChild(group);
		return defs;
	}

	// ****** gold nuggets ******//

	public Document makeGold(Document readDoc, String id, int x, int y,
			boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		if (use == false) {
			Element useFalse = makeGoldNuggetDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
			Element useTrue = makeGoldNuggetUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		} else if (use == true) {
			Element useTrue = makeGoldNuggetUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		}
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	public Document makeGold(Document readDoc, String id, String text, int x,
			int y, boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		if (use == false) {
			Element useFalse = makeGoldNuggetDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
			Element useTrue = makeGoldNuggetUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		} else if (use == true) {
			Element useTrue = makeGoldNuggetUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		}
		Element label = getLabel(readDoc, x, y, text);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeGoldNuggetDefs(Document readDoc, String id) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		// nugget 1
		String points1 = "M" + (dimBoxWidth / 6.46) + ","
				+ (dimBoxHeight / 1.53) + " C" + (dimBoxWidth / 13.13) + ","
				+ (dimBoxHeight / 1.56) + " " + (dimBoxWidth / 16.8) + ","
				+ (dimBoxHeight / 1.54) + " " + (dimBoxWidth / 16.8) + ","
				+ (dimBoxHeight / 1.34) + " C" + (dimBoxWidth / 16.8) + ","
				+ (dimBoxHeight / 1.23) + " " + (dimBoxWidth / 11.35) + ","
				+ (dimBoxHeight / 1.24) + " " + (dimBoxWidth / 6.00) + ","
				+ (dimBoxHeight / 1.24) + " C" + (dimBoxWidth / 4.16) + ","
				+ (dimBoxHeight / 1.24) + " " + (dimBoxWidth / 4.37) + ","
				+ (dimBoxHeight / 1.33) + " " + (dimBoxWidth / 3.36) + ","
				+ (dimBoxHeight / 1.29) + " C" + (dimBoxWidth / 2.92) + ","
				+ (dimBoxHeight / 1.28) + " " + (dimBoxWidth / 2.47) + ","
				+ (dimBoxHeight / 1.28) + " " + (dimBoxWidth / 2.15) + ","
				+ (dimBoxHeight / 1.28) + " C" + (dimBoxWidth / 1.91) + ","
				+ (dimBoxHeight / 1.28) + " " + (dimBoxWidth / 2.16) + ","
				+ (dimBoxHeight / 1.52) + " " + (dimBoxWidth / 2.05) + ","
				+ (dimBoxHeight / 1.65) + " C" + (dimBoxWidth / 1.93) + ","
				+ (dimBoxHeight / 1.85) + " " + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 1.61) + " " + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 2.01) + " C" + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 2.21) + " " + (dimBoxWidth / 2.04) + ","
				+ (dimBoxHeight / 2.35) + " " + (dimBoxWidth / 2.15) + ","
				+ (dimBoxHeight / 2.41) + " C" + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 2.21) + " " + (dimBoxWidth / 2.04) + ","
				+ (dimBoxHeight / 2.35) + " " + (dimBoxWidth / 2.15) + ","
				+ (dimBoxHeight / 2.41) + " C" + (dimBoxWidth / 2.56) + ","
				+ (dimBoxHeight / 2.66) + " " + (dimBoxWidth / 2.8) + ","
				+ (dimBoxHeight / 2.27) + " " + (dimBoxWidth / 3) + ","
				+ (dimBoxHeight / 2.06) + " C" + (dimBoxWidth / 3.11) + ","
				+ (dimBoxHeight / 1.97) + " " + (dimBoxWidth / 4.62) + ","
				+ (dimBoxHeight / 1.91) + " " + (dimBoxWidth / 4.94) + ","
				+ (dimBoxHeight / 1.86) + " C" + (dimBoxWidth / 5.00) + ","
				+ (dimBoxHeight / 1.86) + " " + (dimBoxWidth / 6.36) + ","
				+ (dimBoxHeight / 1.69) + " " + (dimBoxWidth / 6.46) + ","
				+ (dimBoxHeight / 1.69) + " C" + (dimBoxWidth / 7.24) + ","
				+ (dimBoxHeight / 1.67) + " " + (dimBoxWidth / 6.46) + ","
				+ (dimBoxHeight / 1.58) + " " + (dimBoxWidth / 6.46) + ","
				+ (dimBoxHeight / 1.53) + " z";
		Element nugget1 = funcPath(
				readDoc,
				points1,
				"fill:#ffd700;fill-rule:evenodd;stroke:#000000;stroke-width:1.0px;stroke-linecap:butt;stroke-linejoin:miter;");

		// nugget 3
		String points2 = "M" + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 1.29) + " C" + (dimBoxWidth / 1.72) + ","
				+ (dimBoxHeight / 1.46) + " " + (dimBoxWidth / 1.62) + ","
				+ (dimBoxHeight / 1.44) + " " + (dimBoxWidth / 1.45) + ","
				+ (dimBoxHeight / 1.48) + " C" + (dimBoxWidth / 1.37) + ","
				+ (dimBoxHeight / 1.51) + " " + (dimBoxWidth / 1.35) + ","
				+ (dimBoxHeight / 1.64) + " " + (dimBoxWidth / 1.29) + ","
				+ (dimBoxHeight / 1.72) + " C" + (dimBoxWidth / 1.22) + ","
				+ (dimBoxHeight / 1.87) + " " + (dimBoxWidth / 1.11) + ","
				+ (dimBoxHeight / 1.58) + " " + (dimBoxWidth / 1.11) + ","
				+ (dimBoxHeight / 1.56) + " C" + (dimBoxWidth / 1.06) + ","
				+ (dimBoxHeight / 1.47) + " " + (dimBoxWidth / 1.25) + ","
				+ (dimBoxHeight / 1.57) + " " + (dimBoxWidth / 1.14) + ","
				+ (dimBoxHeight / 1.38) + " C" + (dimBoxWidth / 1.08) + ","
				+ (dimBoxHeight / 1.30) + " " + (dimBoxWidth / 1.1) + ","
				+ (dimBoxHeight / 1.29) + " " + (dimBoxWidth / 1.15) + ","
				+ (dimBoxHeight / 1.22) + " C" + (dimBoxWidth / 1.16) + ","
				+ (dimBoxHeight / 1.21) + " " + (dimBoxWidth / 1.25) + ","
				+ (dimBoxHeight / 1.17) + " " + (dimBoxWidth / 1.29) + ","
				+ (dimBoxHeight / 1.14) + " C" + (dimBoxWidth / 1.34) + ","
				+ (dimBoxHeight / 1.11) + " " + (dimBoxWidth / 1.59) + ","
				+ (dimBoxHeight / 1.16) + " " + (dimBoxWidth / 1.65) + ","
				+ (dimBoxHeight / 1.17) + " C" + (dimBoxWidth / 1.73) + ","
				+ (dimBoxHeight / 1.18) + " " + (dimBoxWidth / 1.7) + ","
				+ (dimBoxHeight / 1.25) + " " + (dimBoxWidth / 1.71) + ","
				+ (dimBoxHeight / 1.29) + " z";
		Element nugget2 = funcPath(
				readDoc,
				points2,
				"fill:#ffd700;fill-rule:evenodd;stroke:#000000;stroke-width:1.0px;stroke-linecap:butt;stroke-linejoin:miter;");

		// nugget3 is the right nugget on the top
		String points3 = "M" + (dimBoxWidth / 1.56) + ","
				+ (dimBoxHeight / 4.72) + " C" + (dimBoxWidth / 1.74) + ","
				+ (dimBoxHeight / 4.12) + " " + (dimBoxWidth / 1.68) + ","
				+ (dimBoxHeight / 3.5) + " " + (dimBoxWidth / 1.68) + ","
				+ (dimBoxHeight / 2.64) + " C" + (dimBoxWidth / 1.68) + ","
				+ (dimBoxHeight / 2.21) + " " + (dimBoxWidth / 1.46) + ","
				+ (dimBoxHeight / 2.69) + " " + (dimBoxWidth / 1.45) + ","
				+ (dimBoxHeight / 2.92) + " C" + (dimBoxWidth / 1.43) + ","
				+ (dimBoxHeight / 3.23) + " " + (dimBoxWidth / 1.2) + ","
				+ (dimBoxHeight / 2.96) + " " + (dimBoxWidth / 1.18) + ","
				+ (dimBoxHeight / 2.92) + " C" + (dimBoxWidth / 1.07) + ","
				+ (dimBoxHeight / 2.75) + " " + (dimBoxWidth / 1.11) + ","
				+ (dimBoxHeight / 3.36) + " " + (dimBoxWidth / 1.14) + ","
				+ (dimBoxHeight / 3.85) + " C" + (dimBoxWidth / 1.17) + ","
				+ (dimBoxHeight / 4.88) + " " + (dimBoxWidth / 1.2) + ","
				+ (dimBoxHeight / 5.25) + " " + (dimBoxWidth / 1.22) + ","
				+ (dimBoxHeight / 6.56) + " C" + (dimBoxWidth / 1.25) + ","
				+ (dimBoxHeight / 14) + " " + (dimBoxWidth / 1.42) + ","
				+ (dimBoxHeight / 7.24) + " " + (dimBoxWidth / 1.45) + ","
				+ (dimBoxHeight / 6.56) + " C" + (dimBoxWidth / 1.48) + ","
				+ (dimBoxHeight / 5.92) + " " + (dimBoxWidth / 1.52) + ","
				+ (dimBoxHeight / 5.25) + " " + (dimBoxWidth / 1.56) + ","
				+ (dimBoxHeight / 4.72) + " z";
		Element nugget3 = funcPath(
				readDoc,
				points3,
				"fill:#ffd700;fill-rule:evenodd;stroke:#000000;stroke-width:1.0px;stroke-linecap:butt;stroke-linejoin:miter;");

		String shinePoints = ""
		// nugget1
				+ "M" + (dimBoxWidth / 35) + "," + (dimBoxHeight / 1.86) + "L"
				+ (dimBoxWidth / 13.55) + "," + (dimBoxHeight / 1.71) + "M"
				+ (dimBoxWidth / 13.55) + "," + (dimBoxHeight / 1.18) + "L"
				+ (dimBoxWidth / 24.71) + "," + (dimBoxHeight / 1.11) + "M"
				+ (dimBoxWidth / 2.68) + "," + (dimBoxHeight / 3.53) + "L"
				+ (dimBoxWidth / 2.63) + "," + (dimBoxHeight / 2.88) + "M"
				+ (dimBoxWidth / 5.6) + "," + (dimBoxHeight / 1.16) + "L"
				+ (dimBoxWidth / 5.68) + ","
				+ (dimBoxHeight / 1.08)
				+ "M"
				+ (dimBoxWidth / 2.33)
				+ ","
				+ (dimBoxHeight / 1.19)
				+ "L"
				+ (dimBoxWidth / 2.26)
				+ ","
				+ (dimBoxHeight / 1.11)
				+ "M"
				+ (dimBoxWidth / 7.78)
				+ ","
				+ (dimBoxHeight / 2.31)
				+ "L"
				+ (dimBoxWidth / 5.68)
				+ ","
				+ (dimBoxHeight / 2.1)
				+ "M"
				+ (dimBoxWidth / 4.08)
				+ ","
				+ (dimBoxHeight / 2.86)
				+ "L"
				+ (dimBoxWidth / 3.68)
				+ ","
				+ (dimBoxHeight / 2.44)
				+ "M"
				+ (dimBoxWidth / 3.26)
				+ ","
				+ (dimBoxHeight / 1.21)
				+ "L"
				+ (dimBoxWidth / 3.28)
				+ ","
				+ (dimBoxHeight / 1.12)

				// nugget2
				+ "M" + (dimBoxWidth / 1.09) + "," + (dimBoxHeight / 1.42)
				+ "L" + (dimBoxWidth / 1.02) + "," + (dimBoxHeight / 1.44)
				+ "M" + (dimBoxWidth / 1.08) + "," + (dimBoxHeight / 1.22)
				+ "L" + (dimBoxWidth / 1.01) + "," + (dimBoxHeight / 1.19)
				+ "M" + (dimBoxWidth / 1.03) + "," + (dimBoxHeight / 1.81)
				+ "L" + (dimBoxWidth / 1.08) + "," + (dimBoxHeight / 1.67)
				+ "M" + (dimBoxWidth / 1.27) + "," + (dimBoxHeight / 1.09)
				+ "L" + (dimBoxWidth / 1.26) + "," + (dimBoxHeight / 1.02)
				+ "M" + (dimBoxWidth / 1.56) + "," + (dimBoxHeight / 1.09)
				+ "L" + (dimBoxWidth / 1.59) + "," + (dimBoxHeight / 1.02)
				+ "M"
				+ (dimBoxWidth / 1.13)
				+ ","
				+ (dimBoxHeight / 1.12)
				+ "L"
				+ (dimBoxWidth / 1.09)
				+ ","
				+ (dimBoxHeight / 1.06)
				+ "M"
				+ (dimBoxWidth / 1.12)
				+ ","
				+ (dimBoxHeight / 2.14)
				+ "L"
				+ (dimBoxWidth / 1.16)
				+ ","
				+ (dimBoxHeight / 1.9)

				// nugget3
				+ "M" + (dimBoxWidth / 1.09) + "," + (dimBoxHeight / 4) + "L"
				+ (dimBoxWidth / 1.02) + "," + (dimBoxHeight / 3.96) + "M"
				+ (dimBoxWidth / 1.09) + "," + (dimBoxHeight / 2.63) + "L"
				+ (dimBoxWidth / 1.04) + "," + (dimBoxHeight / 2.37) + "M"
				+ (dimBoxWidth / 1.4) + "," + (dimBoxHeight / 35) + "L"
				+ (dimBoxWidth / 1.36) + "," + (dimBoxHeight / 11.35) + "M"
				+ (dimBoxWidth / 1.11) + "," + (dimBoxHeight / 22.11) + "L"
				+ (dimBoxWidth / 1.17) + "," + (dimBoxHeight / 10.5) + "M"
				+ (dimBoxWidth / 1.04) + "," + (dimBoxHeight / 7.12) + "L"
				+ (dimBoxWidth / 1.11) + "," + (dimBoxHeight / 6) + "M"
				+ (dimBoxWidth / 1.67) + "," + (dimBoxHeight / 16.15) + "L"
				+ (dimBoxWidth / 1.59) + "," + (dimBoxHeight / 8.4) + "M"
				+ (dimBoxWidth / 1.99) + "," + (dimBoxHeight / 6.27) + "L"
				+ (dimBoxWidth / 1.83) + "," + (dimBoxHeight / 4.83);
		Element shine = funcPath(readDoc, shinePoints,
				"stroke:gold;stroke-width:1.0;stroke-linecap:butt;stroke-linejoin:miter;");

		group.appendChild(nugget1);
		group.appendChild(nugget2);
		group.appendChild(nugget3);
		group.appendChild(shine);
		defs.appendChild(group);
		return defs;
	}

	private Element makeGoldNuggetUse(Document readDoc, String id, int x, int y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		use.setAttribute("x", Double.toString(x * dimBoxWidth));
		use.setAttribute("y", Double.toString(y * dimBoxHeight));
		return use;
	}

	// ***fence****
	public Document makeFence(Document readDoc, String id, int x, int y,
			boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "fences", "g");
		if (!use) {
			Element useFalse = makeFenceDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeFenceUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeFenceUse(Document readDoc, String id, int x, int y) {
		Element use = readDoc.createElement("use");
		double dbx = dimBoxWidth * x;
		double dby = dimBoxHeight * y;
		double scale = dimBoxWidth / 10;
		use.setAttribute("transform", "translate(" + dbx + "," + dby + ") " + "scale(" + scale + ")");
		use.setAttribute("xlink:href", "#" + id);
		return use;
	}

	public Element makeFenceDefs(Document readDoc, String id) {
		String fence1 = "M 1.0044097,0.59164699 C 1.3445094,0.5768559 1.8507909,0.35309088 2.4119297,0.35309088 C 2.6681637,0.35309088 3.0127204,0.85106758 3.1938856,0.98924055 C 3.5350789,1.2494659 3.8444165,1.8352061 3.9758408,2.102503 C 4.1917561,2.5416403 4.7125684,2.9427145 5.0705787,3.2157656 C 5.3932959,3.4618989 5.7352875,3.8917221 5.9307301,4.0904719 C 6.2365242,4.4014406 6.5600897,4.7304807 6.7908814,4.9651783 C 7.0900232,5.2693821 7.1541376,5.5449477 7.3382501,5.9194028 C 7.5127419,6.2742925 7.6726857,6.6570908 7.8856193,6.8736275 C 8.1534288,7.1459695 7.7364763,7.8204813 7.6510327,7.9073713 C 7.2953651,8.2690568 7.0603512,8.3049648 6.5562948,8.3049648 C 5.9680923,8.3049648 5.6456408,8.3331262 5.2269697,7.9073713 C 4.8918906,7.5666222 4.6537302,7.4571604 4.4450146,7.032665 C 4.2247934,6.5847709 3.9519609,6.1889101 3.7412542,5.7603653 C 3.5426985,5.3565341 3.3569425,5.1292998 3.0374939,4.8856596 C 2.6066039,4.5570231 2.4462557,4.1476846 2.0209521,3.9314345 C 1.6225847,3.7288803 1.4728193,3.6388295 1.1608008,3.2157656 C 0.92616838,2.897629 0.68550611,2.4876475 0.535236,2.1820218 C 0.39066759,1.8879916 0.56428878,1.0301011 0.61343152,0.83020313 C 0.65029339,0.6802613 0.87408346,0.67116572 1.0044097,0.59164699 z";
		String fence2 = "M 7.0587208,0.84338202 C 6.7476258,0.82808962 6.2845238,0.59673492 5.7712418,0.59673492 C 5.5368618,0.59673492 5.2216908,1.1116011 5.0559768,1.2544608 C 4.7438828,1.5235129 4.4609278,2.1291193 4.3407118,2.4054819 C 4.1432118,2.8595142 3.6668178,3.2741915 3.3393398,3.5565031 C 3.0441458,3.8109847 2.7313228,4.2553862 2.5525478,4.4608773 C 2.2728338,4.7823928 1.9768642,5.1225929 1.765756,5.3652513 C 1.4921263,5.6797726 1.433481,5.9646844 1.2650704,6.35184 C 1.1054601,6.718766 0.95915753,7.114548 0.76438433,7.338429 C 0.51941523,7.620009 0.90080743,8.317397 0.97896373,8.407235 C 1.3042972,8.781187 1.5192683,8.818314 1.9803355,8.818314 C 2.5183718,8.818314 2.8133228,8.84743 3.1962868,8.407235 C 3.5027888,8.054928 3.7206368,7.941754 3.9115528,7.502861 C 4.1129908,7.039776 4.3625548,6.630487 4.5552908,6.1874091 C 4.7369128,5.7698804 4.9068268,5.5349392 5.1990308,5.2830349 C 5.5931708,4.9432513 5.7398428,4.5200296 6.1288748,4.2964455 C 6.4932668,4.0870214 6.6302588,3.9939163 6.9156668,3.5565031 C 7.1302878,3.2275768 7.3504258,2.8036887 7.4878798,2.4876984 C 7.6201188,2.1836953 7.4613058,1.296708 7.4163528,1.090029 C 7.3826348,0.93500222 7.1779318,0.92559722 7.0587208,0.84338202 z";
		String style = "fill:#916f6f;fill-rule:evenodd;stroke:#000000;stroke-width:0.60731536px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1";

		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		Element path1 = funcPath(readDoc, fence1, style);
		Element path2 = funcPath(readDoc, fence2, style);

		group.appendChild(path1);
		group.appendChild(path2);

		defs.appendChild(group);
		return defs;

	}

	// ****** item stable ******//

	public Document makeRedStable(Document readDoc, String id, int x, int y,
			boolean use, boolean team, String rotate) {
		/* get root-element from xml-file */
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "stables", "g");
		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		if (!use) {
			Element useFalse = makeRedStableDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeRedStableUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeRedStableUse(Document readDoc, String id, double x,
			double y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		use.setAttribute("x", Double.toString(x * dimBoxWidth));
		use.setAttribute("y", Double.toString(y * dimBoxHeight));
		return use;
	}

	private Element makeRedStableDefs(Document readDoc, String id) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		Element stable = funcRect(readDoc, 1, 1, dimBoxWidth - 2,
				dimBoxHeight - 2, 0, 0, "fill:darkred");

		/*
		 * group.appendChild(path1); group.appendChild(path2);
		 * group.appendChild(polygon); group.appendChild(ellipse);
		 */
		group.appendChild(stable);
		defs.appendChild(group);
		return defs;
	}

	public Document makeBlueStable(Document readDoc, String id, int x, int y,
			boolean use, boolean team, String rotate) {
		/* get root-element from xml-file */
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "stables", "g");
		/* to rotate the agent */
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}

		if (!use) {
			Element useFalse = makeBlueStableDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeBlueStableUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeBlueStableUse(Document readDoc, String id, double x,
			double y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		use.setAttribute("x", Double.toString(x * dimBoxWidth));
		use.setAttribute("y", Double.toString(y * dimBoxHeight));
		return use;
	}

	private Element makeBlueStableDefs(Document readDoc, String id) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		Element stable = funcRect(readDoc, 1, 1, dimBoxWidth - 2,
				dimBoxHeight - 2, 0, 0, "fill:darkblue");

		/*
		 * group.appendChild(path1); group.appendChild(path2);
		 * group.appendChild(polygon); group.appendChild(ellipse);
		 */
		group.appendChild(stable);
		defs.appendChild(group);
		return defs;
	}

	/**
	 * this method create trees as obstacles
	 * 
	 * @param readDoc
	 *            is the xml document
	 * @param id
	 *            identify this object
	 * @param x
	 *            number of boxes in horizontal direction
	 * @param y
	 *            number of boxes in vertical direction
	 * @return the document with tree
	 */
	public Document makeTrees(Document readDoc, String id, int x, int y,
			boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element treeGroup = getElement(readDoc, "trees", "g");
		if (rotate != "_") {
			Element group = readDoc.createElement("g");
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
			if (!use) {
				Element useFalse = makeBoleAndCrownDefs(readDoc, id, "green");
				treeGroup.appendChild(useFalse);

			}
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			group.appendChild(useTrue);
			treeGroup.appendChild(group);
			mainGroup.appendChild(treeGroup);
			root.appendChild(mainGroup);
			return readDoc;
		} else {
			if (!use) {
				Element useFalse = makeBoleAndCrownDefs(readDoc, id, "green");
				treeGroup.appendChild(useFalse);
			}
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			treeGroup.appendChild(useTrue);
			mainGroup.appendChild(treeGroup);
			root.appendChild(mainGroup);
			return readDoc;
		}
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

	// ****** trees as obstacle ******//

	/**
	 * this method create trees as obstacles
	 * 
	 * @param readDoc
	 *            is the xml document
	 * @param id
	 *            identify this object
	 * @param x
	 *            number of boxes in horizontal direction
	 * @param y
	 *            number of boxes in vertical direction
	 * @return the document with tree
	 */
	public Document makeCirlce(Document readDoc, String id, int x, int y,
			boolean use, String color) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		if (use == false) {
			Element useFalse = makeBoleAndCrownDefs(readDoc, id, color);
			mainGroup.appendChild(useFalse);
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		} else if (use == true) {
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		}
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * this method create trees with text as obstacles
	 * 
	 * @param readDoc
	 *            is the xml document
	 * @param id
	 *            identify this object
	 * @param text
	 *            is the text on the label
	 * @param x
	 *            number of boxes in horizontal direction
	 * @param y
	 *            number of boxes in vertical direction
	 * @return the document with tree
	 */
	public Document makeTrees(Document readDoc, String id, String text, int x,
			int y, boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		if (use == false) {
			Element useFalse = makeBoleAndCrownDefs(readDoc, id, "green");
			mainGroup.appendChild(useFalse);
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		} else if (use == true) {
			Element useTrue = makeBoleAndCrownUse(readDoc, id, x, y);
			group.appendChild(useTrue);
		}
		Element label = getLabel(readDoc, x, y, text);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	/**
	 * create an element with 2 trees
	 * 
	 * @param readDoc
	 *            is the xml document
	 * @param x
	 *            number of boxes in horizontal direction
	 * @param y
	 *            number of boxes in vertical direction
	 * @return the element tree
	 */
	private Element makeBoleAndCrownDefs(Document readDoc, String id,
			String color) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		Element crown1 = funcEllipse(readDoc, dimBoxWidth / 2, dimBoxHeight
				- dimBoxHeight / 2, dimBoxWidth / 2 - 2, dimBoxHeight / 2 - 2,
				"fill:" + color);
		group.appendChild(crown1);
		defs.appendChild(group);
		return defs;
	}

	private Element makeBoleAndCrownUse(Document readDoc, String id, int x,
			int y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		use.setAttribute("x", Double.toString(x * dimBoxWidth));
		use.setAttribute("y", Double.toString(y * dimBoxHeight));
		return use;
	}

	// ****** cows ******//
	public Document makeCow(Document readDoc, String id, int x, int y,
			boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "cows", "g");
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		if (!use) {
			Element useFalse = makeCowDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeCowUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	public Document makeCow(Document readDoc, String id, String text, int x,
			int y, boolean use, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "cows", "g");
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		if (!use) {
			Element useFalse = makeCowDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeCowUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		Element label = getLabel(readDoc, x, y, text);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	private Element makeCowDefs(Document readDoc, String id) {
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		String style = "opacity:0.86345382000000004;fill:#a0892c;fill-opacity:1;stroke-width:0.20000000000000001;stroke-miterlimit:4;stroke-dasharray:none";
		String d = "M 9.8214283,3.3889656 A 4.5535712,2.5446429 0 1 1 0.71428585,3.3889656 A 4.5535712,2.5446429 0 1 1 9.8214283,3.3889656 z";
		Element cow = funcPath(readDoc, d, style);
		group.appendChild(cow);
		defs.appendChild(group);
		return defs;
	}

	private Element makeCowUse(Document readDoc, String id, int x, int y) {
		Element use = readDoc.createElement("use");
		double dbx = x * this.dimBoxWidth;
		double dby = y * this.dimBoxHeight;
		double scale = dimBoxWidth / 10;
		use.setAttribute("transform", "translate(" + dbx + "," + dby + ") " + "scale(" + scale + ")");
		use.setAttribute("xlink:href", "#" + id);
		return use;
	}

	// ****** text maker ******//

	public Document label(Document readDoc, String id, int x, int y,
			String text, String rotate) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		if (rotate != "_") {
			String transform = rotation(x * dimBoxWidth + dimBoxWidth / 2, y
					* dimBoxHeight + dimBoxHeight / 2, rotate);
			group.setAttribute("transform", transform);
		}
		Element label = getLabel(readDoc, x, y, text);
		group.appendChild(label);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;
	}

	// ****** add images ******//

	/**
	 * At this moment don't use this funktion!!! The problem Adobe SVG Viewer
	 * don't support at this moment include image in an included SVG So if you
	 * open the Preview-file you wouldn't see the included file but if you open
	 * the file (e.g. massim-0.svg) you see the included file ...
	 */
	public Document addImage(Document readDoc, double x, double y, String file) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element image = readDoc.createElement("image");
		image.setAttribute("id", "addImage");
		image.setAttribute("height", Double.toString(dimBoxHeight) + "px");
		image.setAttribute("width", Double.toString(dimBoxWidth) + "px");
		image.setAttribute("x", Double.toString(x));
		image.setAttribute("y", Double.toString(y));
		image.setAttribute("xlink:href", file);
		// mainGroup.appendChild(image);
		mainGroup.appendChild(image);
		root.appendChild(mainGroup);
		return readDoc;
	}

	public Document makeSwitch(Document readDoc, String id, int x, int y,
			boolean use, String rotation) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "switches", "g");
		if (!use) {
			Element useFalse = makeSwitchDefs(readDoc, id);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeSwitchUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;

	}

	private Element makeSwitchUse(Document readDoc, String id, int x, int y) {
		Element use = readDoc.createElement("use");
		double dbx = dimBoxWidth * x;
		double dby = dimBoxHeight * y;
		double scale = dimBoxWidth / 10;
		use.setAttribute("transform", "translate(" + dbx + "," + dby + ") " + "scale(" + scale + ")");		
		use.setAttribute("xlink:href", "#" + id);
		return use;
	}

	private Element makeSwitchDefs(Document readDoc, String id) {
		String fence1 = "M 2.5,0.3978969 C 8.0357143,0.3978969 8.3928571,0.57646833 8.0357143,0.3978969 C 7.6785714,0.21932547 8.0357143,9.5050398 8.0357143,9.5050398 L 1.7857143,9.3264683 L 2.5,0.3978969 z";
		String fence2 = "M 0.027090129,2.9459232 C 9.9738604,2.9459232 10.151481,2.7464156 10.151481,2.7464156 L 10.151481,7.3350925 L 10.151481,7.3350925 L 0.20471103,6.9360771 L 0.027090129,2.9459232 z";
		String style = "fill:#ffd42a;fill-rule:evenodd;stroke:#000000;stroke-width:0.93873136999999995px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1";
		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);

		Element path1 = funcPath(readDoc, fence1, style);
		Element path2 = funcPath(readDoc, fence2, style);

		group.appendChild(path1);
		group.appendChild(path2);

		defs.appendChild(group);
		return defs;

	}

	public Document makeAgent(Document readDoc, String id, Integer x,
			Integer y, boolean use, String color) {
		Node root = readDoc.getDocumentElement();
		Element mainGroup = readDoc.getElementById("scaleSvg");
		Element group = this.getElement(readDoc, "agents", "g");
		if (!use) {
			Element useFalse = makeAgentDefs(readDoc, id, color);
			mainGroup.appendChild(useFalse);
		}
		Element useTrue = makeAgentUse(readDoc, id, x, y);
		group.appendChild(useTrue);
		mainGroup.appendChild(group);
		root.appendChild(mainGroup);
		return readDoc;

	}

	private Element makeAgentUse(Document readDoc, String id, Integer x,
			Integer y) {
		Element use = readDoc.createElement("use");
		use.setAttribute("xlink:href", "#" + id);
		double scale = dimBoxWidth / 10;
		double dbx = dimBoxWidth * x;
		double dby = dimBoxHeight * y;
		use.setAttribute("transform", "translate(" + dbx + "," + dby + ") "
				+ "scale(" + scale + ")");
		return use;
	}

	private Element makeAgentDefs(Document readDoc, String id, String color) {
		String fence1 = "M 9.6428571,4.4157519 A 4.0178571,3.8392856 0 1 1 1.6071429,4.4157519 A 4.0178571,3.8392856 0 1 1 9.6428571,4.4157519 z";
		String style = "fill:"
				+ color
				+ ";fill-rule:evenodd;stroke:#ffdd55;stroke-width:1.800731536px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1";

		Element defs = getElement(readDoc, "definitions", "defs");
		Element group = readDoc.createElement("g");
		group.setAttribute("id", id);
		Element path1 = funcPath(readDoc, fence1, style);
		group.appendChild(path1);
		defs.appendChild(group);
		return defs;
	}
}