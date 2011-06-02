package massim.framework.backup;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Test {

	private BackupReader reader = null;

	/**
	 * Constructor
	 */
	public Test() {

		String path = "";

		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

		System.out
				.print("Bitte den absoluten Pfad des Backup-Verzeichnisses eingeben: ");

		try {
			path = br.readLine();
		} catch (IOException e) {
		}
		;

		reader = new BackupReader(path);
		reader.restore();
		reader.printTable();
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

		new Test();

	}
}
