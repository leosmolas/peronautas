package massim.competition2011.monitor;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * This class is a file-based version of the 2011 Mars Scenario's monitor application for off-line
 * review of matches. To start this application you must specify the directory containing the XMLs files
 * of a match:<br/>
 * <br/>
 * <code>GraphFileViewer -dir &lt;directrory&gt;</code><br/>
 * <br/>
 * The required XMLs can be obtained running the RMI XML monitor application with the <code>-savexmls</code>
 * flag during a simulation.
 * (Note that The folder to use here should be one of the automatically-created sub-folders of the folder that
 * you gave as parameter to the RMI XML monitor application) 
 */
public class GraphFileViewer extends GraphMonitor {

	private static final long serialVersionUID = -573692905809930002L;
	
	//File managment
	private String dirName;
	private Vector<Integer> fileNumbers;
	private int fileIdx;
	private File currFile;
	private File dir;
	// XML parsing
	DocumentBuilderFactory factory;
	
	// Buttons
	private JButton buttonFirst;	
	private JButton buttonPrev;
	private JButton buttonNext;
	private JButton buttonLast;	

	private JButton buttonPlay;
	private JButton buttonPlayFs;
	private JButton buttonPlaySl;

	// Play-related
	private Object playSync = new Object();
	private boolean playing;	
	private long[] playSpeeds = {10000, 5000, 2500, 1000, 500, 400, 300, 200, 100, 50, 10};
	private int playSpeedIdx = 4;

	// Constructor
	public GraphFileViewer(String directory) throws FileNotFoundException {
		super();
		
		initFile(directory);
		fileIdx = 0;		
		if (fileNumbers.size() == 0){
			return;
		}
				
		factory = DocumentBuilderFactory.newInstance();
		updateFile();
		if (fileNumbers.size() > 1){
			buttonNext.setEnabled(true);
			buttonLast.setEnabled(true);
			beginPlay();
		}		
	}

	private void initFile(String directory) throws FileNotFoundException {
		dir = new File(directory);
		if (!dir.isDirectory()){
			throw new FileNotFoundException();
		}
		this.dirName = dir.getName();
		
		FileFilter filter = new FileFilter() {

			@Override
			public boolean accept(File f) {
				if (f.isDirectory()){
					return false;
				}
				try {
					String name = f.getName();
					String firstPart = name.substring(0, dirName.length());
					String extension = name.substring(name.lastIndexOf("."));
					if (firstPart.equalsIgnoreCase(dirName)
							&& extension.equalsIgnoreCase(".xml")) {
						return true;
					}
					return false;
				} catch (IndexOutOfBoundsException e) {
					return false;
				}
			}
			
		};
		File [] files = dir.listFiles(filter);
		fileNumbers = new Vector<Integer>(files.length);
		for (int i = 0; i < files.length; i++){
			try {
				String name = files[i].getName();
				String nr = name.substring(dirName.length() + 1, name.lastIndexOf("."));
				fileNumbers.add(new Integer(nr));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		Collections.sort(fileNumbers);		
	}
	
	private void beginPlay() {
		synchronized (playSync) {
			playing = false;
			buttonPlay.setVisible(true);
			buttonPlayFs.setVisible(true);
			buttonPlaySl.setVisible(true);
			buttonPlayFs.setEnabled(true);
			buttonPlaySl.setEnabled(true);
			if (fileNumbers.size() > 1){
				buttonPlay.setEnabled(true);
			}
			while (true){
				
				try {
					playSync.wait();
				} catch (InterruptedException e1) {
					e1.printStackTrace();
				}
				
				while (playing){
					if (fileIdx < fileNumbers.size()-1){
						fileIdx ++;
						buttonPrev.setEnabled(true);
						buttonFirst.setEnabled(true);
						updateFile();
					}
					if (fileIdx >= fileNumbers.size()-1){
						fileIdx = fileNumbers.size()-1;
						buttonPlay.setEnabled(false);
						buttonPlay.setText("Play");
						buttonNext.setEnabled(false);
						buttonLast.setEnabled(false);
						playing = false;
					}	
					try {
						long speed;
						synchronized (playSpeeds) {
							speed = playSpeeds[playSpeedIdx];
						}						
						playSync.wait(speed);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
					
				}
			}
		}
		
	}

	private void updateFile() {
		try {
			currFile = new File(dir, dirName + "_" + fileNumbers.get(fileIdx).toString()+".xml");
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document document = builder.parse(currFile);
			parseXML(document);
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		updateView();		
	}

	public void actionPerformed(ActionEvent e) {
		if ("last".equals(e.getActionCommand())){
			synchronized (playSync) {
				buttonPrev.setEnabled(true);
				buttonFirst.setEnabled(true);
				fileIdx++;
				fileIdx = fileNumbers.size() - 1;
				buttonNext.setEnabled(false);
				buttonPlay.setEnabled(false);
				buttonLast.setEnabled(false);
				updateFile();
			}
		}
		else if ("next".equals(e.getActionCommand())){
			synchronized (playSync) {
				buttonPrev.setEnabled(true);
				buttonFirst.setEnabled(true);
				fileIdx++;
				if (fileIdx == fileNumbers.size() - 1) {
					buttonNext.setEnabled(false);
					buttonPlay.setEnabled(false);
					buttonLast.setEnabled(false);
				}
				updateFile();
			}
		}
		else if ("previous".equals(e.getActionCommand())){
			synchronized (playSync) {
				buttonNext.setEnabled(true);
				buttonPlay.setEnabled(true);
				buttonLast.setEnabled(true);
				fileIdx--;
				if (fileIdx == 0) {
					buttonPrev.setEnabled(false);
					buttonFirst.setEnabled(false);
				}
				updateFile();
			}
		}
		else if ("first".equals(e.getActionCommand())){
			synchronized (playSync) {
				buttonNext.setEnabled(true);
				buttonPlay.setEnabled(true);
				buttonLast.setEnabled(true);
				fileIdx = 0;
				buttonPrev.setEnabled(false);
				buttonFirst.setEnabled(false);
				updateFile();
			}
		}
		else if ("play".equals(e.getActionCommand())){
			synchronized (playSync) {
				playing = !playing;
				if (playing) {
					buttonPlay.setText("Stop");
					playSync.notifyAll();
				} else {
					buttonPlay.setText("Play");
				}				
			}			
		}
		else if ("faster".equals(e.getActionCommand())){
			synchronized (playSpeeds) {
				buttonPlaySl.setEnabled(true);
				playSpeedIdx++;
				if (playSpeedIdx == playSpeeds.length - 1){
					buttonPlayFs.setEnabled(false);
				}
			}			
		}
		else if ("slower".equals(e.getActionCommand())){
			synchronized (playSpeeds) {
				buttonPlayFs.setEnabled(true);
				playSpeedIdx--;
				if (playSpeedIdx == 0){
					buttonPlaySl.setEnabled(false);
				}
			}			
		}
	}
	
	@Override
	protected JPanel createUpperMenu() {
		JPanel menuPanel = new JPanel();
		menuPanel.setLayout(new BoxLayout(menuPanel,BoxLayout.X_AXIS));
		menuPanel.add(Box.createRigidArea(new Dimension(10, 0)));
		
		buttonFirst = new JButton("<<");
		buttonFirst.addActionListener(this);
		buttonFirst.setActionCommand("first");
		buttonFirst.setEnabled(false);
		menuPanel.add(buttonFirst);
		
		buttonPrev = new JButton("Previous");
		buttonPrev.addActionListener(this);
		buttonPrev.setActionCommand("previous");
		buttonPrev.setEnabled(false);
		menuPanel.add(buttonPrev);
		
		buttonNext = new JButton("Next");
		buttonNext.addActionListener(this);
		buttonNext.setEnabled(false);
		buttonNext.setActionCommand("next");
		menuPanel.add(buttonNext);
		
		buttonLast = new JButton(">>");
		buttonLast.addActionListener(this);
		buttonLast.setActionCommand("last");
		buttonLast.setEnabled(false);
		menuPanel.add(buttonLast);

		
		
		menuPanel.add(Box.createRigidArea(new Dimension(10, 0)));
		
		buttonPlay = new JButton("Play");
		buttonPlay.addActionListener(this);
		buttonPlay.setEnabled(false);
		buttonPlay.setVisible(false);
		buttonPlay.setActionCommand("play");		
		menuPanel.add(buttonPlay);
		
		buttonPlayFs = new JButton("+");
		buttonPlayFs.addActionListener(this);
		buttonPlayFs.setEnabled(false);
		buttonPlayFs.setVisible(false);
		buttonPlayFs.setActionCommand("faster");		
		menuPanel.add(buttonPlayFs);
		
		buttonPlaySl = new JButton("-");
		buttonPlaySl.addActionListener(this);
		buttonPlaySl.setEnabled(false);
		buttonPlaySl.setVisible(false);
		buttonPlaySl.setActionCommand("slower");		
		menuPanel.add(buttonPlaySl);
		
		menuPanel.add(Box.createRigidArea(new Dimension(25, 0)));
		
		addZoomButtons(menuPanel);
		
		return menuPanel;
	}
	
	/**
	 * 
	 * @param args
	 */
	public static void main(String args[]) {
		try {
			
			String arg = null;
			for(int i = 0; i<args.length;i++){
				if(args[i].equalsIgnoreCase("-dir")){
					arg = args[i+1];
					break;
				}
					
			}
			if (arg != null){
				new GraphFileViewer(arg);
			} else {
				System.out.println("GraphMonitor -dir <directrory containing the xmls>");
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

}
