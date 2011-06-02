package massim.goldsimulations;

import massim.gridsimulations.AbstractGridSimulationCell;

/**
 * This class describes a single GridCell.
 * 
 */
public class GridSimulationCell extends AbstractGridSimulationCell {

	private static final long serialVersionUID = -4580023204848715935L;
	public boolean mark;
	public boolean gold;
	public boolean depot;
	public String markText;

	/**
	 * This method checks whether the cell contains an object or not.
	 * 
	 * @return Returns false, if gold, depot, obstacle or agent is in the cell,
	 *         otherwise true.
	 */
	public boolean noObject() {
		if (!gold && !depot && !obstacle && !agent) {
			return true;
		} else {
			return false;
		}
	}


	/**
	 * This method checks, if a cell is free for pushing
	 * 
	 * @param x
	 *            Position x on the grid
	 * @param y
	 *            Position y on the grid
	 * @return True in such a case, else false
	 */
	public boolean freeCell() {
		if (!agent && !obstacle && !depot) {
			return true;
		}
		else {
			return false;
		}
	}

}
