package massim.visualization.svg;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

/**
 * main SVG function
 * 
 * for more information look at www.w3.org/TR/SVG/index.html
 * 
 * 
 * 
 * Copyright © [2006-05-08] World Wide Web Consortium, (Massachusetts Institute
 * of Technology, European Research Consortium for Informatics and Mathematics,
 * Keio University). All Rights Reserved.
 * http://www.w3.org/Consortium/Legal/2002/copyright-documents-20021231
 * 
 */

public class SvgFunction {

	/*
	 * toDo <defs> <linearGradient id="verlauf"> <stop offset="0%"
	 * style="stop-color:rgb(0,0,0);stop-opacity:1"/> <stop offset="100%"
	 * style="stop-color:rgb(255,255,255);stop-opacity:1"/> </linearGradient>
	 * </defs>
	 */
	// private void funcDefs() {
	// }
	/**
	 * function create the SVG element line
	 * 
	 * <svg> --> <line x1="..." y1="..." x2="..." y2="..." style="..." /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param x1
	 *            x-axis coordinate of the start of the line
	 * @param x2
	 *            x-axis coordinate of the end of the line
	 * @param y1
	 *            y-axis coordinate of the start of the line
	 * @param y2
	 *            y-axis coordinate of the end of the line
	 * @param rx
	 * @param ry
	 * @param style
	 *            style for the line NOTE: 'line' elements are never filled
	 * @return line svg element
	 */
	public Element funcLine(Document doc, double x1, double x2, double y1,
			double y2, double rx, double ry, String style) {
		Element line = doc.createElement("line");
		line.setAttribute("x1", Double.toString(x1));
		line.setAttribute("y1", Double.toString(x2));
		line.setAttribute("x2", Double.toString(y1));
		line.setAttribute("y2", Double.toString(y2));
		line.setAttribute("rx", Double.toString(rx));
		line.setAttribute("ry", Double.toString(ry));
		line.setAttribute("style", style);
		return line;
	}

	/**
	 * function create the SVG element line
	 * 
	 * <svg> --> <line x1="..." y1="..." x2="..." y2="..." style="..." /> </svg>
	 * 
	 * @param doc
	 *            document
	 * @param x1
	 *            x-axis coordinate of the start of the line
	 * @param x2
	 *            x-axis coordinate of the end of the line
	 * @param y1
	 *            y-axis coordinate of the start of the line
	 * @param y2
	 *            y-axis coordinate of the end of the line
	 * @param style
	 *            style for the line NOTE: 'line' elements are never filled
	 * @return line svg element
	 */
	public Element funcLine(Document doc, double x1, double x2, double y1,
			double y2, String style) {
		Element line = doc.createElement("line");
		line.setAttribute("x1", Double.toString(x1));
		line.setAttribute("y1", Double.toString(x2));
		line.setAttribute("x2", Double.toString(y1));
		line.setAttribute("y2", Double.toString(y2));
		line.setAttribute("style", style);
		return line;
	}

	/**
	 * function create the SVG element rectangle
	 * 
	 * <svg> --> <rect x =".." y =".." width =".." height =".." rx =".." ry
	 * =".." /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param x
	 *            x-axis coordinate of the rectangle
	 * @param y
	 *            y-axis coordinate of the rectangle
	 * @param width
	 *            width of the rectangle
	 * @param height
	 *            height of the rectangle
	 * @param rx
	 *            rounded rectangles (x-axis radius of ellipse used to round off
	 *            corners of rectangle) NOTE: negative value is an error
	 * @param ry
	 *            rounded rectangles (y-axis radius of ellipse used to round off
	 *            corners of rectangle) NOTE: negative value is an error
	 * @param style
	 * @return rectangle element
	 */
	public Element funcRect(Document doc, double x, double y, double width,
			double height, double rx, double ry, String style) {
		Element rect = doc.createElement("rect");
		rect.setAttribute("x", Double.toString(x));
		rect.setAttribute("y", Double.toString(y));
		rect.setAttribute("width", Double.toString(width));
		rect.setAttribute("height", Double.toString(height));
		rect.setAttribute("rx", Double.toString(rx));
		rect.setAttribute("ry", Double.toString(ry));
		rect.setAttribute("style", style);
		return rect;
	}

	/**
	 * function create the SVG element circle
	 * 
	 * <svg> --> <circle cx="..." cy="..." r="..." /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param cx
	 *            x-axis coordinate of the center of the circle
	 * @param cy
	 *            y-axis coordinate of the center of the circle
	 * @param r
	 *            radius of the circle NOTE: negative value is an error
	 * @param style
	 * @return circle element
	 */
	public Element funcCircle(Document doc, double cx, double cy, double r,
			String style) {
		Element circle = doc.createElement("circle");
		circle.setAttribute("cx", Double.toString(cx));
		circle.setAttribute("cy", Double.toString(cy));
		circle.setAttribute("r", Double.toString(r));
		circle.setAttribute("style", style);
		return circle;
	}

	/**
	 * function create the SVG element ellipse
	 * 
	 * <svg> --> <ellipse cx="..." cy="..." rx="..." ry="..." /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param cx
	 *            x-axis coordinate of the center of the ellipse
	 * @param cy
	 *            y-axis coordinate of the center of the ellipse
	 * @param rx
	 *            x-axis radius of the ellipse NOTE: negative value is an error
	 * @param ry
	 *            y-axis radius of the ellipse NOTE: negative value is an error
	 * @param style
	 * @return ellipse element
	 */
	public Element funcEllipse(Document doc, double cx, double cy, double rx,
			double ry, String style) {
		Element ellipse = doc.createElement("ellipse");
		ellipse.setAttribute("cx", Double.toString(cx));
		ellipse.setAttribute("cy", Double.toString(cy));
		ellipse.setAttribute("rx", Double.toString(rx));
		ellipse.setAttribute("ry", Double.toString(ry));
		ellipse.setAttribute("style", style);
		return ellipse;
	}

	/**
	 * function create the SVG element polyline
	 * 
	 * <svg> --> <polyline points="...,... ...,... ...,... ...,... /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param points
	 *            points that make up the polyline
	 * @param style
	 * @return polyline element
	 */
	public Element funcPolyline(Document doc, String points, String style) {
		Element polyline = doc.createElement("polyline");
		polyline.setAttribute("points", points);
		polyline.setAttribute("style", style);
		return polyline;
	}

	/**
	 * function create the SVG element polygone
	 * 
	 * <svg> --> <polygon points="...,... ...,... ...,... ...,... /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param points
	 *            points that make up the polygon
	 * @param style
	 * @return polygon element
	 */
	public Element funcPolygon(Document doc, String points, String style) {
		Element polygon = doc.createElement("polygon");
		polygon.setAttribute("points", points);
		polygon.setAttribute("style", style);
		return polygon;
	}

	/**
	 * function create the SVG elment path
	 * 
	 * <svg> --> <path d="M ... ... L ... ... L ... ... z" /> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param d
	 *            definition of the outline of a shape NOTE: > "name"
	 *            (parameters) //example Command //example > "moveto" (x y)+ M
	 *            (absolute) indicates that absolute coordinates m (relative)
	 *            indicates that relative coordinates > "closepath" (none) Z or
	 *            z close the current subpath (drawing a straight line from
	 *            current point to current subpath's initial point) > "lineto"
	 *            (x y)+ draw a line from current point to given coordinate
	 *            number of coordinates pairs may be specified to draw a
	 *            polyline L (absolute) indicates that absolute coordinates l
	 *            (relative) indicates that relative coordinates > "horizontal
	 *            lineto" x+ Draws a horizontal line from current point
	 *            (currentpoint(x), currentpoint(y)) to (x, currentpoint(y)) H
	 *            (absolute) indicates that absolute coordinates h (relative)
	 *            indicates that relative coordinates > "vertical lineto" y+
	 *            draws a vertical line from current point (currentpoint(x),
	 *            currentpoint(y)) to (currentpoint(x), y) V (absolute)
	 *            indicates that absolute coordinates v (relative) indicates
	 *            that relative coordinates > "curveto" (x1 y1 x2 y2 x y)+ draws
	 *            a cubic Bözier curve from current point to (x,y) using
	 *            (x1,y1) as control point at beginning of curve and (x2,y2) as
	 *            control point at end of curve C (absolute) indicates that
	 *            absolute coordinates c (relative) indicates that relative
	 *            coordinates > "shorthand/smooth curveto" (x2 y2 x y)+ draws a
	 *            cubic Bözier curve from current point to (x,y) (x2,y2) is
	 *            control point at the end of the curve S (absolute) indicates
	 *            that absolute coordinates s (relative) indicates that relative
	 *            coordinates > "quadratic Bözier curveto" (x1 y1 x y)+ Q
	 *            (absolute) indicates that absolute coordinates q (relative)
	 *            indicates that relative coordinates > "Shorthand/smooth
	 *            quadratic Bözier curveto" (x y)+ draws a quadratic Bözier
	 *            curve from current point to (x,y) T (absolute) indicates that
	 *            absolute coordinates t (relative) indicates that relative
	 *            coordinates > "elliptical arc" (rx ry x-axis-rotation
	 *            large-arc-flag sweep-flag x y)+ draws an elliptical arc from
	 *            current point to (x, y) size and orientation of ellipse are
	 *            defined by two radii (rx, ry) and an x-axis-rotation, which
	 *            indicates how ellipse as a whole is rotated relative to
	 *            current coordinate system the center (cx, cy) of ellipse is
	 *            calculated automatically to satisfy constraints imposed by
	 *            other parameters large-arc-flag and sweep-flag contribute to
	 *            automatic calculations and help determine how arc is drawn A
	 *            (absolute) indicates that absolute coordinates a (relative)
	 *            indicates that relative coordinates
	 * 
	 * Copyright � [2006-05-08] World Wide Web Consortium, (Massachusetts
	 * Institute of Technology, European Research Consortium for Informatics and
	 * Mathematics, Keio University). All Rights Reserved.
	 * http://www.w3.org/Consortium/Legal/2002/copyright-documents-20021231
	 * 
	 * @param pathLength
	 *            the total length of the path NOTE: negative value is an error
	 * @param style
	 * @return path element
	 */
	public Element funcPath(Document doc, String d, double pathLength,
			String style) {
		Element path = doc.createElement("path");
		path.setAttribute("d", d);
		path.setAttribute("pathLength", Double.toString(pathLength));
		path.setAttribute("style", style);
		return path;
	}

	public Element funcPath(Document doc, String d, String style) {
		Element path = doc.createElement("path");
		path.setAttribute("d", d);
		path.setAttribute("style", style);
		return path;
	}

	/**
	 * function create the SVG element text
	 * 
	 * <svg> --> <text x=".." y="..." font-family="Verdana" font-size="55"
	 * fill="blue" > Hello World! </text> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param x
	 *            start x-axis coordinate for text
	 * @param y
	 *            y-axis coordinate for the text
	 * @param text
	 * @param style
	 * @return
	 */
	public Element funcText(Document doc, String id, double x, double y,
			String text, String style) {
		Text theText = doc.createTextNode(text);
		Element elementText = doc.createElement("text");
		elementText.setAttribute("id", id);
		elementText.setAttribute("x", Double.toString(x));
		elementText.setAttribute("y", Double.toString(y));
		elementText.setAttribute("style", style);
		elementText.appendChild(theText);
		return elementText;
	}

	/**
	 * function create the SVG element text
	 * 
	 * <svg> --> <text x=".." y="..." font-family="Verdana" font-size="55"
	 * fill="blue" > Hello World! </text> </svg>
	 * 
	 * @param doc
	 *            svg document
	 * @param x
	 *            start x-axis coordinate for text
	 * @param y
	 *            y-axis coordinate for the text
	 * @param text
	 * @param style
	 * @return
	 */
	public Element funcText(Document doc, double x, double y, String text,
			String style) {
		Text theText = doc.createTextNode(text);
		Element elementText = doc.createElement("text");
		elementText.setAttribute("x", Double.toString(x));
		elementText.setAttribute("y", Double.toString(y));
		elementText.setAttribute("style", style);
		elementText.appendChild(theText);
		return elementText;
	}

	public Document funcAddAttribute(Document doc, String id, String attribute,
			String attributeValue) {
		Element toAdd = doc.getElementById(id);
		toAdd.setAttribute(attribute, attributeValue);
		return doc;
	}

	// ****** use in agent and obstacle ******//

	public String rotation(double x, double y, String angle) {
		String rotate = "";
		if (angle == "N" || angle == "n") {
			rotate = "rotate(0," + x + "," + y + ")";
		} else if (angle == "E" || angle == "e") {
			rotate = "rotate(90," + x + "," + y + ")";
		} else if (angle == "S" || angle == "s") {
			rotate = "rotate(180," + x + "," + y + ")";
		} else if (angle == "W" || angle == "w") {
			rotate = "rotate(-90," + x + "," + y + ")";
		} else {
			rotate = "rotate(" + angle + "," + x + "," + y + ")";
		}
		return rotate;
	}

}