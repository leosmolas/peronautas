package massim.visualization;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class HandleFileFolder {

	/**
	 * this method create a file
	 * 
	 * @param fileName
	 *            name / path for file
	 * @param contents
	 *            contents of the file
	 * @throws IOException
	 *             write problems
	 */
	public void createFile(String fileName, String contents) throws IOException {
		FileOutputStream writeStream = new FileOutputStream(fileName);
		for (int i = 0; i < contents.length(); i++) {
			writeStream.write((byte) contents.charAt(i));
		}
		writeStream.close();
	}

	/**
	 * create folder if not exist
	 * 
	 * @param folderPath
	 *            give the path to the folder
	 */
	public void createFolder(String folderPath) {
		File newDir = new File(folderPath);
		newDir.mkdirs();
		// if (newDir.mkdir()==true) {
		// System.out.println("Directory was created");
		// }
		// else {
		// System.out.println("Directory already existed");
		// }
	}
}
