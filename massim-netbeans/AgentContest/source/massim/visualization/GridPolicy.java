package massim.visualization;

import java.util.ArrayList;
import java.util.Map;

import massim.visualization.svg.SvgXmlFile;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

public class GridPolicy extends MainPolicy {

	/* for parse config file */
	private Document configDocument;
	private double widthGrid;
	private double heightGrid;

	/* change this default values if define in config file */
	private String svgImageHeight = "525";
	private String svgImageWidth = "1020";
	public double cellHeight = 42;
	public double cellWidth = 42;

	/* optical style */
	public double gridFontSize = 30;
	public String gridFontFamily = "Arial Unicode";
	public String gridStrokeColour = "black";
	public String gridBackgroundColour0 = "#FEEFB8";
	public String gridBackgroundColour1 = "#FEEFB8";
	public double gridLineStrength = 3;

	public GridPolicy(){
		if(this.readTheConfig){
			this.readConfigFile();
		}
	}
	private double getHeightGrid() {
		return heightGrid;
	}

	private double getWidthGrid() {
		return widthGrid;
	}

	private void setSvgImageHeight(String value) {
		svgImageHeight = value;
	}

	private void setSvgImageWidth(String value) {
		svgImageWidth = value;
	}

	/**
	 * open the config file and work with it
	 * 
	 */

	private void readConfigFile() {
		String configPath = super.getConfigFile();
		SvgXmlFile config = new SvgXmlFile();
		configDocument = config.openFile(configDocument, configPath);
		/* set path and name of output (MainPolicy) */
		Node root = configDocument.getDocumentElement();
		int length = root.getChildNodes().getLength();
		for (int i = 0; i < length; i++) {
			Node configElement = root.getChildNodes().item(i);

			/* check NodeName from config file */

			// simulation stuff
			if ((configElement.getNodeName() == "simulationOutput")) {
				Element simulationConfig = (Element) configDocument
						.getElementsByTagName("simulationOutput").item(0);
				int numberAttributes = configElement.getAttributes()
						.getLength();
				for (int j = 0; j < numberAttributes; j++) {
					// get the path if define in config file
					if (configElement.getAttributes().item(j).getNodeName() == "path") {
						setPath(simulationConfig.getAttribute("path"));
					} else
					// get the name of the output files if define in config file
					if (configElement.getAttributes().item(j).getNodeName() == "nameOutputFile") {
						setNameOutputFile(simulationConfig
								.getAttribute("nameOutputFile"));
					} else {
						// do nothing
					}
				}
			}
			// read the width and height of the included SVG (e.g. the grid) in
			// the controll svg from config file
			if ((configElement.getNodeName() == "imageSize")) {
				Element simulationConfig = (Element) configDocument
						.getElementsByTagName("imageSize").item(0);
				int numberAttributes = configElement.getAttributes()
						.getLength();
				for (int j = 0; j < numberAttributes; j++) {
					if (configElement.getAttributes().item(j).getNodeName() == "height") {
						setSvgImageHeight(simulationConfig
								.getAttribute("height"));
					} else if (configElement.getAttributes().item(j)
							.getNodeName() == "width") {
						setSvgImageWidth(simulationConfig.getAttribute("width"));
					} else {
						// to ignore wrong definitions in config file or if
						// nothing is define
					}
				}
			}
			// read the configuration for the grid (background colour, colour of
			// stroke, etc.) if define in config file
			if ((configElement.getNodeName() == "gridConfig")) {
				Element simulationConfig = (Element) configDocument
						.getElementsByTagName("gridConfig").item(0);
				int numberAttributes = simulationConfig.getChildNodes()
						.getLength();
				for (int j = 0; j < numberAttributes; j++) {
					if (simulationConfig.getChildNodes().item(j).getNodeName() == "mainStyle") {
						Element mainStyle = (Element) configDocument
								.getElementsByTagName("mainStyle").item(0);
						int lengthMainStyle = mainStyle.getAttributes()
								.getLength();
						for (int a = 0; a < lengthMainStyle; a++) {
							if (mainStyle.getAttributes().item(a).getNodeName() == "strokeColour") {
								gridStrokeColour = mainStyle
										.getAttribute("strokeColour");
							} else if (mainStyle.getAttributes().item(a)
									.getNodeName() == "backgroundColour0") {
								gridBackgroundColour0 = mainStyle
										.getAttribute("backgroundColour0");
							} else if (mainStyle.getAttributes().item(a)
									.getNodeName() == "backgroundColour1") {
								gridBackgroundColour1 = mainStyle
										.getAttribute("backgroundColour1");
							}
							else if (mainStyle.getAttributes().item(a)
									.getNodeName() == "strokeWidth") {
								gridLineStrength = Double.parseDouble(mainStyle
										.getAttribute("strokeWidth"));
							} else if (mainStyle.getAttributes().item(a)
									.getNodeName() == "cellHeight") {
								cellHeight = Double.parseDouble(mainStyle
										.getAttribute("cellHeight"));
							} else if (mainStyle.getAttributes().item(a)
									.getNodeName() == "cellWidth") {
								cellWidth = Double.parseDouble(mainStyle
										.getAttribute("cellWidth"));
							} else {
								// do nothing
							}
						}
					} else if (simulationConfig.getChildNodes().item(j)
							.getNodeName() == "textStyle") {
						Element textStyle = (Element) configDocument
								.getElementsByTagName("textStyle").item(0);
						int lengthTextStyle = textStyle.getAttributes()
								.getLength();
						for (int a = 0; a < lengthTextStyle; a++) {
							if (textStyle.getAttributes().item(a).getNodeName() == "font-size") {
								gridFontSize = Double.parseDouble(textStyle
										.getAttribute("font-size"));
							} else if (textStyle.getAttributes().item(a)
									.getNodeName() == "font-family") {
								gridFontFamily = textStyle
										.getAttribute("font-family");
							} else {

							}
						}
					} else {
						// to ignore wrong definitions in config file
					}
					// configDocument.getElementsByTagName("gridConfig").item(0).getChildNodes().item(j).getNodeName()
				}
			}
			if ((configElement.getNodeName() == "agentConfig")) {
				Element agentConfig = (Element) configDocument
						.getElementsByTagName("agentConfig").item(0);
				int numberAttributes = agentConfig.getChildNodes().getLength();
				for (int j = 0; j < numberAttributes; j++) {
					if (agentConfig.getChildNodes().item(j).getNodeName() == "labelStyle") {
						Element agentStyle = (Element) configDocument
								.getElementsByTagName("labelStyle").item(0);
						int lengthAgentStyle = agentStyle.getAttributes()
								.getLength();
						// FIXME override the default value with value from
						// conf-file
						for (int a = 0; a < lengthAgentStyle; a++) {
							if (agentStyle.getAttributes().item(a)
									.getNodeName() == "colour") {
								// = agentStyle.getAttribute("colour");
							} else if (agentStyle.getAttributes().item(a)
									.getNodeName() == "textColour") {
								// = agentStyle.getAttribute("textColour");
							} else {
								// do nothing
							}
						}
					}
					if (agentConfig.getChildNodes().item(j).getNodeName() == "headStyle") {
						Element agentStyle = (Element) configDocument
								.getElementsByTagName("headStyle").item(0);
						int lengthAgentStyle = agentStyle.getAttributes()
								.getLength();
						// FIXME override the default value with value from
						// conf-file
						for (int a = 0; a < lengthAgentStyle; a++) {
							if (agentStyle.getAttributes().item(a)
									.getNodeName() == "colour") {
								// = agentStyle.getAttribute("colour");
							} else if (agentStyle.getAttributes().item(a)
									.getNodeName() == "textColour") {
								// = agentStyle.getAttribute("textColour");
							} else {
								// do nothing
							}
						}
					}
				}

			}
		}
		createPathFolder();
		readTheConfig = true;
	}

	/**
	 * for the previewSvg create it and set the head NOTE: run createPreviewSvg
	 * at the end !!! (need the number of svg's)
	 */
	public void createPreviewSvg() {
		// if (readTheConfig == false) readConfigFile();
		setImageHeight(svgImageHeight);
		setImageWidth(svgImageWidth);
		previewSvg();
	}

	public void create() {
		if (readTheConfig == false)
			readConfigFile();
		doc = createXML();
		resetValues();
	}

	public void save() {
		if (readTheConfig == false)
			readConfigFile();
		double height = getHeightGrid();
		double width = getWidthGrid();
		save(doc, height, width);
	}

	// ****** Methode to draw the grid ******//

	/**
	 * set the style read from config file
	 * 
	 * @param gridStyle
	 *            MainGrid object
	 */
	private void setConfigStyle(MainGrid gridStyle) {
		gridStyle.setWidthGridCell(cellWidth);
		gridStyle.setHeightGridCell(cellHeight);
		gridStyle.setBackgroundColour0(gridBackgroundColour0);
		gridStyle.setBackgroundColour1(gridBackgroundColour1);
		gridStyle.setStrokeWidth(gridLineStrength);
		gridStyle.setStrokeColour(gridStrokeColour);
		gridStyle.setFontFamily(gridFontFamily);
		gridStyle.setFontSize(gridFontSize);
	}

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
	public void drawGrid(String id, int x, int y) {
		MainGrid grid = new MainGrid();
		/* generate the dimension of a box againts grid dimension */
		widthGrid = cellWidth * x;
		heightGrid = cellHeight * y;
		grid.setWidthGrid(widthGrid);
		grid.setHeightGrid(heightGrid);
		setConfigStyle(grid);
		numberCellGridX = x;
		numberCellGridY = y;
		doc = grid.makeDrawGrid(doc, id, x, y);
	}

	public void drawGrid(long id, int x, int y) {
		String stringId = Long.toString(id);
		drawGrid(stringId, x, y);
	}

	/**
	 * this method create a grid with text
	 * 
	 * @param id
	 *            is the id of the grid (String or Long)
	 * @param text
	 *            text under the grid
	 * @param x
	 *            is the horizontal number of cells (if x=5 then there are cells
	 *            0 ... 4)
	 * @param y
	 *            is the vertical number of cells (if y=5 then there are cells 0
	 *            ... 4)
	 */
	public void drawGrid(String id, String text, int x, int y) {
		MainGrid grid = new MainGrid();
		/* generate the dimension of a box againts grid dimension */
		widthGrid = cellWidth * x;
		heightGrid = cellHeight * y + gridFontSize + gridFontSize / 2;
		grid.setWidthGrid(cellWidth * x);
		grid.setHeightGrid(cellHeight * y);
		setConfigStyle(grid);
		numberCellGridX = x;
		numberCellGridY = y;
		this.doc = grid.makeDrawGrid(doc, id, text, x, y);
	}

	public void drawGrid(long id, String text, int x, int y) {
		String stringId = Long.toString(id);
		drawGrid(stringId, text, x, y);
	}

	/**
	 * set the style read from config file
	 * 
	 * @param gridStyle
	 *            MainGrid object
	 */

	public void setGridStatistic(Map<String, String> myMap) {
		MainGrid grid = new MainGrid();
		grid.setWidthGridCell(cellWidth);
		grid.setHeightGrid(cellHeight);
		this.doc = grid.setGridStatistic(numberCellGridX, numberCellGridY, doc,
				myMap);
		/* return the width of the grid with the statistic on the right site */
		double statisticWidth = grid.getStatisticWidth();
		widthGrid = statisticWidth;
		// this.internalWidth = internalWidth + 1300;
	}

	// ****** Methode to draw the agents ******//

	public void drawGoldDigger(long id, long teamId, int x, int y,
			String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDigger(doc, id, teamId, x, y, rotation);
	}

	public void drawGoldDigger(long id, long teamId, int x, int y) {
		drawGoldDigger(id, teamId, x, y, "_");
	}

	public void drawGoldDigger(long id, long teamId, String text, int x, int y,
			String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDigger(doc, id, teamId, text, x, y, rotation);
	}

	public void drawGoldDigger(long id, long teamId, String text, int x, int y) {
		drawGoldDigger(id, teamId, text, x, y, "_");
	}

	public void drawGoldDigger(long id, String teamId, int x, int y,
			String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDigger(doc, id, teamId, x, y, rotation);
	}

	public void drawGoldDigger(long id, String teamId, int x, int y) {
		drawGoldDigger(id, teamId, x, y, "_");
	}

	public void drawGoldDigger(long id, String teamId, String text, int x,
			int y, String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDigger(doc, id, teamId, text, x, y, rotation);
	}

	public void drawGoldDigger(long id, String teamId, String text, int x, int y) {
		drawGoldDigger(id, teamId, text, x, y, "_");
	}

	// ****** gold digger with gold bag ******//

	public void drawGoldDiggerWithGold(long id, long teamId, int x, int y,
			String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDiggerWithGold(doc, id, teamId, x, y, rotation);
	}

	public void drawGoldDiggerWithGold(long id, long teamId, int x, int y) {
		drawGoldDiggerWithGold(id, teamId, x, y, "_");
	}

	public void drawGoldDiggerWithGold(long id, long teamId, String text,
			int x, int y, String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDiggerWithGold(doc, id, teamId, text, x, y, rotation);
	}

	public void drawGoldDiggerWithGold(long id, long teamId, String text,
			int x, int y) {
		drawGoldDiggerWithGold(id, teamId, text, x, y, "_");
	}

	public void drawGoldDiggerWithGold(long id, String teamId, int x, int y,
			String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDiggerWithGold(doc, id, teamId, x, y, rotation);
	}

	public void drawGoldDiggerWithGold(long id, String teamId, int x, int y) {
		drawGoldDiggerWithGold(id, teamId, x, y, "_");
	}

	public void drawGoldDiggerWithGold(long id, String teamId, String text,
			int x, int y, String rotation) {
		Agent agent = new Agent();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		agent.setDimLine();
		doc = agent.goldDiggerWithGold(doc, id, teamId, text, x, y, rotation);
	}

	public void drawGoldDiggerWithGold(long id, String teamId, String text,
			int x, int y) {
		drawGoldDiggerWithGold(id, teamId, text, x, y, "_");
	}

	// ****** objects ******//

	//****** gold depot ******//
	public void drawGoldDepot(String id, int x, int y, String rotation) {
		ItemsAndObstacles depot = new ItemsAndObstacles();
		depot.setDimBoxHeight(cellHeight);
		depot.setDimBoxWidth(cellWidth);
		doc = depot.makeGoldDepot(doc, "depot_"+id, x, y, setDepot, rotation);
		setDepot = true;
	}
	public void drawGoldDepot(long id, int x, int y, String rotation) {
		drawGoldDepot(Long.toString(id), x, y, rotation);
	}
	public void drawGoldDepot(String id, int x, int y) {
		drawGoldDepot(id, x, y, "_");
	}
	public void drawGoldDepot(long id, int x, int y) {
		drawGoldDepot(Long.toString(id), x, y, "_");
	}
	
	public void drawGoldDepot(String id, String text, int x, int y, String rotation) {
		ItemsAndObstacles depot = new ItemsAndObstacles();
		depot.setDimBoxHeight(cellHeight);
		depot.setDimBoxWidth(cellWidth);
		doc = depot.makeGoldDepot(doc, "depot_"+id, text, x, y, setDepot, rotation);
		setDepot = true;
	}
	public void drawGoldDepot(long id, String text, int x, int y, String rotation) {
		drawGoldDepot(Long.toString(id), text, x, y, rotation);
	}
	public void drawGoldDepot(String id, String text, int x, int y) {
		drawGoldDepot(id, text, x, y, "_");
	}
	public void drawGoldDepot(long id, String text, int x, int y) {
		drawGoldDepot(Long.toString(id), text, x, y, "_");
	}
	//**draw Gold**/
	//****** gold object ******//
	public void drawGold(String id, int x, int y, String rotation) {
		ItemsAndObstacles gold = new ItemsAndObstacles();
		gold.setDimBoxHeight(cellHeight);
		gold.setDimBoxWidth(cellWidth);
		doc = gold.makeGold(doc, "gold_"+id, x, y, setGold, rotation);
		setGold = true;
	}
	public void drawGold(long id, int x, int y, String rotation) {
		drawGold(Long.toString(id), x, y, rotation);
	}
	public void drawGold(String id, int x, int y) {
		drawGold(id, x, y, "_");
	}
	public void drawGold(long id, int x, int y) {
		drawGold(Long.toString(id), x, y, "_");
	}

	public void drawGold(String id, String text, int x, int y, String rotation) {
		ItemsAndObstacles gold = new ItemsAndObstacles();
		gold.setDimBoxHeight(cellHeight);
		gold.setDimBoxWidth(cellWidth);
		doc = gold.makeGold(doc, "gold_"+id, text, x, y, setGold, rotation);
		setGold = true;
	}
	public void drawGold(long id, String text, int x, int y, String rotation) {
		drawGold(Long.toString(id), text, x, y, rotation);
	}
	public void drawGold(String id, String text, int x, int y) {
		drawGold(id, text, x, y, "_");
	}
	public void drawGold(long id, String text, int x, int y) {
		drawGold(Long.toString(id), text, x, y, "_");
	}
//**draw stable**/
	public void drawStable(String id, int x, int y, String rotation,
			boolean team) {
		ItemsAndObstacles depot = new ItemsAndObstacles();
		depot.setDimBoxHeight(cellHeight);
		depot.setDimBoxWidth(cellWidth);
		if (!team) {
			doc = depot.makeRedStable(doc, "depot_0", x, y, setRedDepot, team,
					rotation);
			setRedDepot = true;
		} else {
			doc = depot.makeBlueStable(doc, "depot_1", x, y, setBlueDepot,
					team, rotation);
			setBlueDepot = true;
		}
	}

	public void drawStable(long id, int x, int y, String rotation,
			boolean team) {
		drawStable(Long.toString(id), x, y, rotation, team);
	}

	public void drawStable(String id, int x, int y, boolean team) {
		drawStable(id, x, y, "_", team);
	}

	public void drawStable(long id, int x, int y, boolean team) {
		drawStable(Long.toString(id), x, y, "_", team);
	}

	public void drawStable(String id, String text, int x, int y,
			String rotation, boolean team) {
		ItemsAndObstacles depot = new ItemsAndObstacles();
		depot.setDimBoxHeight(cellHeight);
		depot.setDimBoxWidth(cellWidth);
		if (!team) {
			doc = depot.makeRedStable(doc, "depot_0", x, y, setRedDepot, team,
					rotation);
			setRedDepot = true;
		} else {
			doc = depot.makeBlueStable(doc, "depot_1", x, y, setRedDepot, team,
					rotation);
			setBlueDepot = true;
		}
	}

	public void drawStable(long id, String text, int x, int y,
			String rotation, boolean team) {
		drawStable(Long.toString(id), text, x, y, rotation, team);
	}

	public void drawStable(String id, String text, int x, int y, boolean team) {
		drawStable(id, text, x, y, "_", team);
	}

	public void drawStable(long id, String text, int x, int y, boolean team) {
		drawStable(Long.toString(id), text, x, y, "_", team);
	
	}
	
	boolean setCircle = false;
	ArrayList<String> list = new ArrayList<String>();
	public void drawCircle(String id, int x, int y, String color){
		ItemsAndObstacles iao = new ItemsAndObstacles();
		iao.setDimBoxHeight(cellHeight);
		iao.setDimBoxWidth(cellWidth);
		if(!id.contains(id)){
			setCircle = false;
			doc = iao.makeCirlce(doc, id, x, y,setCircle, color);
			list.add(id);
		}
		else {
			setCircle = true;
			doc = iao.makeCirlce(doc, id, x, y,setCircle, color);
		}	
	}
	// ****** tree as obstacle ******//
	public void drawTrees(String id, int x, int y, String rotation) {
		ItemsAndObstacles itemOrObstacle = new ItemsAndObstacles();
		itemOrObstacle.setDimBoxHeight(cellHeight);
		itemOrObstacle.setDimBoxWidth(cellWidth);
		doc = itemOrObstacle.makeTrees(doc, "tree_" + id, x, y, setTree, rotation);
		setTree = true;
	}

	public void drawTrees(long id, int x, int y, String rotation) {
		drawTrees(Long.toString(id), x, y, rotation);
	}

	public void drawTrees(String id, int x, int y) {
		drawTrees(id, x, y, "_");
	}
//draw 1
	public void drawTrees(long id, int x, int y) {
		drawTrees(Long.toString(id), x, y, "_");
	}

	public void drawTrees(String id, String text, int x, int y, String rotation) {
		ItemsAndObstacles depot = new ItemsAndObstacles();
		depot.setDimBoxHeight(cellHeight);
		depot.setDimBoxWidth(cellWidth);
		doc = depot.makeTrees(doc, "tree_" + id, text, x, y, setTree, rotation);
		setTree = true;
	}

	public void drawTrees(long id, String text, int x, int y, String rotation) {
		drawTrees(Long.toString(id), text, x, y, rotation);
	}

	public void drawTrees(String id, String text, int x, int y) {
		drawTrees(id, text, x, y, "_");
	}

	public void drawTrees(long id, String text, int x, int y) {
		drawTrees(Long.toString(id), text, x, y, "_");
	}

	// ****** cow object ******//
	public void drawCow(String id, int x, int y, String rotation) {
		ItemsAndObstacles cow = new ItemsAndObstacles();
		cow.setDimBoxHeight(cellHeight);
		cow.setDimBoxWidth(cellWidth);
		doc = cow.makeCow(doc, "cow_" + id, x, y, setGold, rotation);
		setGold = true;
	}

	public void drawCow(long id, int x, int y, String rotation) {
		drawCow(Long.toString(id), x, y, rotation);
	}

	public void drawCow(String id, int x, int y) {
		drawCow(id, x, y, "_");
	}

	public void drawCow(long id, int x, int y) {
		drawCow(Long.toString(id), x, y, "_");
	}

	public void drawCow(String id, String text, int x, int y, String rotation) {
		ItemsAndObstacles cow = new ItemsAndObstacles();
		cow.setDimBoxHeight(cellHeight);
		cow.setDimBoxWidth(cellWidth);
		doc = cow.makeCow(doc, "gold_" + id, text, x, y, setGold, rotation);
		setGold = true;
	}

	public void drawCow(long id, String text, int x, int y, String rotation) {
		drawCow(Long.toString(id), text, x, y, rotation);
	}

	public void drawCow(String id, String text, int x, int y) {
		drawCow(id, text, x, y, "_");
	}

	public void drawCow(long id, String text, int x, int y) {
		drawCow(Long.toString(id), text, x, y, "_");
	}
	//*********draw fences**********/

public void drawFences(int x, int y){
	ItemsAndObstacles fence = new ItemsAndObstacles();
	fence.setDimBoxHeight(cellHeight);
	fence.setDimBoxWidth(cellWidth);
	doc = fence.makeFence(doc, "fence", x, y, setFence, "_");
	setFence = true;	
}

//*********draw switch**********/

public void drawSwitch(int x, int y){
ItemsAndObstacles fence = new ItemsAndObstacles();
fence.setDimBoxHeight(cellHeight);
fence.setDimBoxWidth(cellWidth);
doc = fence.makeSwitch(doc, "switch", x, y, setSwitch, "_");
setSwitch = true;	
}
	// ****** text object (marker) ******//
	public void drawText(String id, String text, int x, int y, String rotation) {
		ItemsAndObstacles label = new ItemsAndObstacles();
		/* set the informations about the boxes and the hole grid */
		label.setDimBoxHeight(cellHeight);
		label.setDimBoxWidth(cellWidth);
		this.doc = label.label(doc, id, x, y, text, rotation);
	}

	public void drawText(long id, String text, int x, int y, String rotation) {
		drawText(Long.toString(id), text, x, y, rotation);
	}

	public void drawText(String id, String text, int x, int y) {
		drawText(id, text, x, y, "_");
	}

	public void drawText(long id, String text, int x, int y) {
		drawText(Long.toString(id), text, x, y, "_");
	}

	// ****** ******//
	public void addImage(long id, int x, int y, String image) {
		ItemsAndObstacles t = new ItemsAndObstacles();
		t.addImage(doc, x, y, image);

	}


	public void drawAgent(String color, Integer posx, Integer posy) {
		
		ItemsAndObstacles agent = new ItemsAndObstacles();
		agent.setDimBoxHeight(cellHeight);
		agent.setDimBoxWidth(cellWidth);
		if(color.equalsIgnoreCase("red")){
			doc = agent.makeAgent(doc, "agentred", posx, posy, setAgentred, color);
			setAgentred = true;
		}
		else{
			doc = agent.makeAgent(doc, "agentblue", posx, posy, setAgentblue, color);
			setAgentblue = true;
		}
		
	}
	public void setConfigPath(String string) {
		
		
	}
}
