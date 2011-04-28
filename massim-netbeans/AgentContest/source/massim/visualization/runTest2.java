package massim.visualization;

public class runTest2 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		ColumnPolicy te = new ColumnPolicy();
		te.create();
		// te.drawGrid(12,2,2);
		te.drawGoldDigger(88, 3, 0, 0);
		te.save();
	}

}
