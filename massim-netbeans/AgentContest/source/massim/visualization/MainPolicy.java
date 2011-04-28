package massim.visualization;

import java.io.IOException;

import massim.server.Server;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class MainPolicy extends massim.visualization.svg.SvgXmlFile {
	public static String svgFile="";
	public static String previewFile="";
	public boolean setTree = false;
	public boolean setDepot = false;
	public boolean setGold = false;

	public boolean setRedDepot = false;
	public boolean setBlueDepot = false;

	/* path and name of config File */
	private String configPath = Server.configurationFilenamePath + System.getProperty("file.separator") + "visualization" + System.getProperty("file.separator");
	private String configFile = configPath + "visualconfig.xml";
	

	/* counts svg's */
	private long numberOfSvgFiles = -1;
	// ** main config stuff **//
	public boolean readTheConfig = true;
	// ** grid stuff **//
	/* is the overhead title */
	private String headInformationFirstLevel = "";
	/* is the underline title */
	private String headInformationSecondLevel = "";

	/* path and name of the output */

	private String path = "./output" + System.getProperty("file.separator");
	private String outPut = path;
	private String nameOutputFile = "massim";
	private String namePreviewSvg = "SimulationPreview";
	private String imageHeight = "525";
	private String imageWidth = "1020";
	
	private double scaleFactor = 0;

	public int numberCellGridX = 0;
	public int numberCellGridY = 0;

	public  Document doc;
	public boolean setAgentblue;
	public boolean setAgentred;
	public boolean setFence;
	public boolean setSwitch;

	/*
	 * (non-Javadoc) only setters and getters
	 */

	public String getSvgEnding() {
		return svgEnding;
	}

	public String getConfigFile() {
		return configFile;
	}

	public String getNameOutputFile() {
		return nameOutputFile;
	}

	public String getNamePreviewSvg() {
		return namePreviewSvg;
	}

	public String getPath() {
		return path;
	}

	public void setOutPut(String newPath) {
		this.outPut = newPath;
	}

	public String getOutPut() {
		return this.outPut;
	}

	public void setPath(String newPath) {
		
		this.path = newPath;
	}

	public void setNameOutputFile(String newNameOutputFile) {
		this.nameOutputFile = newNameOutputFile;
	}

	public void setImageHeight(String newSvgImageHeight) {
		this.imageHeight = newSvgImageHeight;
	}

	public void setImageWidth(String newSvgImageWidth) {
		this.imageWidth = newSvgImageWidth;
	}

	/**
	 * set the first Informationline over the grid
	 * 
	 * @param headInfo
	 *            the String with the text
	 */
	public void setHeadInformationFirstLevel(String headInfo) {
		this.headInformationFirstLevel = headInfo;
	}

	/**
	 * set the second Informationline over the grid
	 * 
	 * @param headInfo
	 *            the String with the text
	 */
	public void setHeadInformationSecondLevel(String headInfo) {
		this.headInformationSecondLevel = headInfo;
	}

	/*
	 * end setters and getters
	 */
	
	public void setConfigPath(String string) {
		this.configPath = string;		
	}
	
	public void resetValues() {
		setDepot = false;
		setTree = false;
		setGold = false;
		setRedDepot = false;
		setBlueDepot = false;
		setAgentred = false;
		setAgentblue = false;
		setSwitch=false;
		setFence=false;
	}

	/**
	 * this method create the output folder if not exist.
	 */
	public void createPathFolder() {

		HandleFileFolder folder = new HandleFileFolder();
		folder.createFolder(path);
	}

	public void createFolder(String name) {
		HandleFileFolder folder = new HandleFileFolder();
		folder.createFolder(path + name);
		this.setOutPut(path + name + System.getProperty("file.separator"));
	}

	public void createFile(String part1, String part2) throws IOException {
		HandleFileFolder file = new HandleFileFolder();
		file.createFile(outPut + part1, part2);
	}

	/**
	 * ) This method create a new svg (xml) file
	 * 
	 * @return document is new SVG file
	 */
	public Document createXML() {
		/* call svg.SvgXmlFile.generateXML */
		Document document = generateXML();
		numberOfSvgFiles = numberOfSvgFiles + 1;
		return document;
	}

	/**
	 * create the preview / controll svg to play the simulation
	 */
	public void previewSvg() {
		PreviewSvg pre = new PreviewSvg();
		// String image = pre.readSvgConf(configFile);
		pre.setImageHeight(imageHeight);
		pre.setImageWidth(imageWidth);
		pre.createPreviewSvg(outPut, configPath, numberOfSvgFiles,
				headInformationFirstLevel, headInformationSecondLevel);
		//svgFile = outPut+namePreviewSvg;
		
	}
	

	/**
	 * method write the SVG file to disk.
	 * 
	 * @param doc
	 *            the document which will be write to disk
	 * @param internalHeight
	 *            the height of the hole SVG need to scale
	 * @param internalWidth
	 *            the width of the hole SVG need to scale
	 */
	public void save(Document doc, double internalHeight, double internalWidth) {
		/*
		 * this string merge the name of the outputfiles with a counter and the
		 * ending of the file
		 */
		String currentFile = outPut + nameOutputFile + "-" + numberOfSvgFiles + svgEnding;
		Element rootDoc = doc.getDocumentElement();

		/*
		 * here we add to the group "scaleSvg" the parameter transform="scale(x
		 * y)" because the image has to be scale to an dimension of height and
		 * width given by class fileAction
		 */
		Element scaleElement = doc.getElementById("scaleSvg");
		scaleElement.setAttribute("transform", "scale(" + Double.toString(this.getScaleFactor(imageHeight, imageWidth, internalHeight, internalWidth)) + ")");
		rootDoc.appendChild(scaleElement);
		saveXML(doc, currentFile);
		svgFile=currentFile;
		previewFile = outPut+namePreviewSvg+svgEnding;
	}

	// ****** Methode to draw ******//

	// ****** Methode to draw the grid ******//

	/**
	 * this method create a grid
	 * 
	 * @param id
	 *            is the id of the grid (String or Long)
	 * @param x
	 *            is the horizontal number of cells (if x=5 then there are cells
	 *            0 ... 4)
	 * @param y
	 *            is the vertical number of cells (if y=5 then there are cells 0
	 *            ... 4)
	 */
	/*
	 * public void drawGrid(String id, int x, int y) { MainGrid grid = new
	 * MainGrid(); /* generate the dimension of a box againts grid dimension
	 */
	/*
	 * widthGrid = cellWidth * x; heightGrid = cellHeight * y;
	 * grid.setWidthGrid(widthGrid); grid.setHeightGrid(heightGrid);
	 * setConfigStyle(grid); numberCellGridX = x; numberCellGridY = y; doc =
	 * grid.makeDrawGrid(doc, id, x, y); } public void drawGrid(long id, int x,
	 * int y) { String stringId = Long.toString(id); drawGrid(stringId, x, y); }
	 * 
	 * /** this method create a grid with text @param id is the id of the grid
	 * (String or Long) @param text text under the grid @param x is the
	 * horizontal number of cells (if x=5 then there are cells 0 ... 4) @param y
	 * is the vertical number of cells (if y=5 then there are cells 0 ... 4)
	 */
	/*
	 * public void drawGrid(String id, String text, int x, int y) { MainGrid
	 * grid = new MainGrid(); /* generate the dimension of a box againts grid
	 * dimension
	 */
	/*
	 * widthGrid = cellWidth * x; heightGrid = cellHeight * y + gridFontSize +
	 * gridFontSize/2; grid.setWidthGrid(cellWidth * x);
	 * grid.setHeightGrid(cellHeight * y); setConfigStyle(grid); numberCellGridX =
	 * x; numberCellGridY = y; this.doc = grid.makeDrawGrid(doc, id, text, x,
	 * y); } public void drawGrid(long id, String text, int x, int y) { String
	 * stringId = Long.toString(id); drawGrid(stringId, text, x, y); }
	 * 
	 * /** set the style read from config file @param gridStyle MainGrid object
	 */
	/*
	 * private void setConfigStyle(MainGrid gridStyle) {
	 * gridStyle.setWidthGridCell(cellWidth);
	 * gridStyle.setHeightGridCell(cellHeight); //
	 * gridStyle.setBackgroundColour(gridBackgroundColour);
	 * gridStyle.setStrokeWidth(gridLineStrength);
	 * gridStyle.setStrokeColour(gridStrokeColour);
	 * gridStyle.setFontFamily(gridFontFamily);
	 * gridStyle.setFontSize(gridFontSize); }
	 * 
	 * public void setGridStatistic(Map<String, String> myMap) { MainGrid grid =
	 * new MainGrid(); this.doc = grid.setGridStatistic(numberCellGridX,
	 * numberCellGridY, doc, myMap); /* return the width of the grid with the
	 * statistik on the right site
	 */
	/*
	 * double statisticWidth = grid.getStatisticWidth(); widthGrid =
	 * statisticWidth; //this.internalWidth = internalWidth + 1300; }
	 */
	
	private double getScaleFactor(String imageHeight, String imageWidth, double internalHeight, double internalWidth) {
		if (this.scaleFactor == 0) { 
			double scaleHeight = Double.parseDouble(imageHeight) / internalHeight;
			double scaleWidth = Double.parseDouble(imageWidth) / internalWidth;
			if (scaleHeight < scaleWidth) {
				this.scaleFactor = scaleHeight;
				return scaleHeight;
			} else {
				this.scaleFactor = scaleWidth;
				return scaleWidth;
			}
		} else {
			return this.scaleFactor;
		}
	}

}
