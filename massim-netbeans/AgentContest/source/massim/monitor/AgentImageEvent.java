package massim.monitor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;

import javax.swing.ImageIcon;
import javax.swing.JPanel;


public class AgentImageEvent extends JPanel {

	
	int x , y;
		
	public AgentImageEvent(int x, int y){
		this.x = x;
		this.y = y;
		
	}
	
	public void setSize(int x, int y){
			this.x = x; 
			this.y = y;
		}
		
	   public Dimension getPreferredSize() {
	        return new Dimension(x,y);
	    }

	 protected void paintComponent(Graphics g) {
	        super.paintComponent(g);       
	        Image img = new ImageIcon("./cow.png").getImage();
	        g.drawImage(img, 20, 20,14, 20, null,null);
	    }  

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
