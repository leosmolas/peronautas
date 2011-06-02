package massim.monitor;

import java.awt.Graphics;
import java.awt.Image;

import javax.swing.JFrame;

import org.w3c.dom.Document;

public abstract class AbstractSimulationOutput extends JFrame{
	
	private Image [] images = null;
	private String configPath = null;
	
	private int sizeX = 0;
	private int sizeY = 0;
	
	//Configuration of components
	public abstract void configure();
	
	//Update the simulation field (after one step)
	public abstract void updateOutput(Document doc);
	
	//Painting of components
	public abstract void paint(Graphics g);
	
	//Set images for components of simulation
	public void setImages(Image[] newImages){
		
		this.images = newImages;
	}
	
	//Get images for components of simulation
	public  Image[] getImages(){
		
		return this.images;
	}
	
	//Set path
	public void setConfigPath(String newPath){
		
		this.configPath = newPath;
	}
	
	//Get path
	public String getConfigPath(){
		
		return this.configPath;
	}
	
	//Set size
	public void setOutputSize(int newX, int newY){
		
		this.sizeX = newX;
		this.sizeY = newY;
	}
	
	//Get sizeX
	public int getOutputSizeX(){
		
		return this.sizeX;
	}
	
	//Get sizeY
	public int getOutputSizeY(){
		
		return this.sizeY;
	}

}