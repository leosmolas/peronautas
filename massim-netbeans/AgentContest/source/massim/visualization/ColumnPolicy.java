package massim.visualization;

//import org.w3c.dom.Document;

public class ColumnPolicy extends MainPolicy {

	/* for parse config file */
	// private Document configDocument;
	// private boolean setDepot = false;
	// private boolean setTree = false;
	// private boolean setGold = false;
	// private double widthGrid;
	// private double heightGrid;
	/* change this default values if define in config file */
	// private String svgImageHeight = "525";
	// private String svgImageWidth = "1020";
	public double cellHeight = 42;
	public double cellWidth = 42;

	public void readConfigFile() {

	}

	public void create() {
		if (readTheConfig == false)
			readConfigFile();
		doc = createXML();
		resetValues();
	}

	/*
	 * for the previewSvg create it and set the head NOTE: run createPreviewSvg
	 * at the end !!! (need the number of svg's)
	 */
	public void createPreviewSvg() {
		// if (readTheConfig == false) readConfigFile();
		// setImageHeight(svgImageHeight);
		// setImageWidth(svgImageWidth);
		previewSvg();
	}

	/*
	 * end previewSvg
	 */

	/**
	 * 
	 */
	public void save() {
		if (readTheConfig == false)
			readConfigFile();
		// double height = getHeightGrid();
		// double width = getWidthGrid();
		// save(doc, height, width);
		save(doc, 50, 50);
	}

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

}
