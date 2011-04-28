package massim.cowsimulations;

import massim.gridsimulations.AbstractGridSimulationCell;

/**
 * This class describes a single GridCell.
 * 
 */
public class GridSimulationCell extends AbstractGridSimulationCell {

	private static final long serialVersionUID = -4580023204848715935L;
	public boolean cow = false;
	public boolean fence = false;
	public boolean open = false;
	public boolean switcher = false;
	public boolean canOpen = false;

	public int cowturn;
	public String agentTeam;
	private boolean stable1 = false;
	private boolean stable2 = false;
	public String cowID;

	/**
	 * This method checks whether the cell contains an object or not.
	 * 
	 * @return Returns false, if cow, obstacle or agent is in the cell,
	 *         otherwise true.
	 */
	public boolean noObject() {

		return (!cow && !obstacle && !agent && !fence && !switcher);

	}

	/**
	 * This method checks, if a cell is free to get in
	 * 
	 * @param x
	 *            Position x on the grid
	 * @param y
	 *            Position y on the grid
	 * @return True in a such case, else false
	 */
	public boolean freeCell() {

		if (!agent && !obstacle && !cow && !switcher
				&& (!fence || (fence && open))) {

			return true;

		}

		else {
			return false;
		}
	}

	/**
	 * This method checks, if a cell is free to get in
	 * 
	 * @param x
	 *            Position x on the grid
	 * @param y
	 *            Position y on the grid
	 * @return True in a such case, else false
	 */
	public boolean freeCellforCow() {

		if (!agent && !obstacle && !cow && !switcher
				&& (!fence || (fence && open))) {

			return true;

		}

		else {
			return false;
		}
	}

	public void setStable1(boolean stable1) {
		this.stable1 = stable1;
	}

	public boolean isStable1() {
		return stable1;
	}

	public void setStable2(boolean stable2) {
		this.stable2 = stable2;
	}

	public boolean isStable2() {
		return stable2;
	}
}