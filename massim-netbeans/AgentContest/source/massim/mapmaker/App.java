package massim.mapmaker;

import java.io.File;
import java.io.IOException;

public class App {

	public static void main(String[] args) {
		
		if( args.length != 3) {
			System.out.println("Parameters: pngfile xmlfile destfile");
			System.exit(0);
		}
		
		// get the PNG-file
		File pngFile = new File(args[0]);
		if( pngFile.exists() == false ) {
			System.out.println("File \"" + pngFile + "\" does not exist!");
			System.exit(0);
		}
		else if( pngFile.getAbsolutePath().endsWith(".png") == false) {
			System.out.println("File \"" + pngFile + "\" is not a PNG-file!");
			System.exit(0);
		}

		// get the XML-file
		File xmlFile = new File(args[1]);
		if( xmlFile.exists() == false ) {
			System.out.println("File \"" + xmlFile + "\" does not exist!");
			System.exit(0);
		}
		else if( xmlFile.getAbsolutePath().endsWith(".xml") == false) {
			System.out.println("File \"" + xmlFile + "\" is not an XML-file!");
			System.exit(0);
		}

		File destFile = new File(args[2]);
		if( destFile.exists() ) {
		
			System.out.println("File \"" + destFile + "\" exists. Overwrite? y/n");

			char c = 0;
			
			try {
				c = (char) System.in.read();
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(0);
			}

			if( c != 'y') {
				System.out.println("Will not overwrite");
				System.exit(0);
			}
				
			
		}
		
		new Converter(pngFile,xmlFile,destFile);
		
	}
	
}
