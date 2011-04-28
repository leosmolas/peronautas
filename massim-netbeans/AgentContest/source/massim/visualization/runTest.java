package massim.visualization;

import java.util.HashMap;
import java.util.Map;

public class runTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		// PreviewSvg t = new PreviewSvg();
		// t.readSvgConf("SimulationOutputConfig.xml");
		Map<String, String> myMap = new HashMap<String, String>();
		/*
		 * myMap.put("QQ","QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ45");
		 * myMap.put("QQ","WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW");
		 * myMap.put("aQQQQQQQQQQQQQQQ","[");
		 * myMap.put("sQQQQQQQQQQQQQQQ","QQQQQQ"); myMap.put("QQefc","QQer");
		 */
		myMap.put("QQ", "QQ");
		myMap.put("QQ", "WW");
		myMap.put("aQQQQQQQQQQQQQQQ", "[");
		myMap.put("sQQQQQQQQQQQQQQQ", "QQQQQQ");
		myMap.put("QQefc", "QQer");

		GridPolicy te = new GridPolicy();
		te.create();
		te.drawGrid(12, 2, 2);
		te.drawGoldDigger(1, 1, "sdchu viet hung", 1, 1);
		te.drawGoldDiggerWithGold(1, 5, 0, 2);
		te.save();

		te.create();
		te.drawGrid(12, "gsdsd", 100, 100);
		te.drawGoldDiggerWithGold(88, 3, "DDDDD", 0, 0);
		te.drawGoldDiggerWithGold(1, 5, "DDDDD", 0, 1);
		te.drawGoldDiggerWithGold(1, 5, 0, 2);
		te.drawGoldDiggerWithGold(1, "gold", "defedf", 0, 3, "12");
		te.drawGoldDiggerWithGold(1, "RGB(12,24,45)", 0, 4);
		te.drawGoldDiggerWithGold(1, "RGB(12,24,45)", "sfd", 3, 4, "s");
		te.drawGoldDigger(88, 3, "DDDDD", 1, 0);
		te.drawGoldDigger(1, 5, "DDDDD", 1, 1);
		te.drawGoldDigger(1, 5, 1, 2);
		te.drawGoldDigger(1, "gold", "dgdg", 1, 3);
		te.drawGoldDigger(1, "RGB(12,24,45)", 1, 4);
		// te.drawGoldDepot(1, 4, 4);
		te.addImage(99, 2, 4, "output\\images\\previmage.gif");
		te.save();

		te.create();
		te.drawGrid(12, "gsdsd", 5, 5);
		te.drawTrees(23, 0, 2, "N");
		// te.drawGoldDepot(1, 4, 4, "E");
		te.setGridStatistic(myMap);
		te.save();

		te.create();
		te.drawGrid(12, "gsdsd", 25, 25);
		te.drawTrees(23, 0, 0, "N");
		te.drawTrees(23, 0, 1, "E");
		te.drawTrees(23, 0, 2, "S");
		te.drawTrees(23, "s", 0, 0, "N");
		te.drawTrees(23, "fd", 0, 1, "E");
		te.drawTrees(23, "df", 0, 2, "S");

		// te.drawGoldDepot(1, 4, 4, "E");
		// te.drawGoldDepot(1, "lala", 4, 12, "E");
		// for(int i=0;i<30;i++) {
		te.drawCow(21, 1, 3, "E");
		// }
		te.setHeadInformationFirstLevel("servus LOL");
		te.setHeadInformationSecondLevel("h3h3");
		te.setGridStatistic(myMap);
		te
				.addImage(
						13,
						23,
						3,
						"C:\\Dokumente und Einstellungen\\dm\\Eigene Dateien\\workspace\\X_NewVisualization\\output\\images\\previmage.gif");
		te.save();

		te.createPreviewSvg();
		System.out.println("ok");
	}

}
