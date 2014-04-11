package abscon.instance.tools;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.PrintWriter;

import org.w3c.dom.Document;

import abscon.instance.InstanceTokens;
import abscon.instance.XMLManager;

public class InstanceShuffler {

	private File file;

	private int mode;

	private int seed;

	public InstanceShuffler(File file, int seed, int mode) {
		this.file = file;
		this.mode = mode;
		if (mode < 1 || mode > 3)
			throw new IllegalArgumentException();
		this.seed = seed;
	}

	private PrintWriter buildPrintWriter() throws Exception {
		String s = file.getName();
		int position = s.lastIndexOf(".xml");
		String fileName = s.substring(0, position) + "_shf" + seed + ".xml";
		File file = new File(fileName);
		if (file.exists())
			System.out.println(fileName + " exists ");
		return new PrintWriter(new FileOutputStream(file)); // absoluteFileName));
	}

	public void treat() throws Exception {
		Document document = XMLManager.load(new FileInputStream(file), null);
		DocumentShuffler documentShuffler = new DocumentShuffler();
		document = documentShuffler.shuffle(document, seed,mode);
		XMLManager.save(document, buildPrintWriter(), XMLManager.class.getResourceAsStream(InstanceTokens.INSTANCE_STYLESHEET_2_0));
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 3) {
			System.out.println("InstanceShuffler " + InstanceParser.VERSION);
			System.out.println("Usage: java ... InstanceShuffler <instanceFileName> <seed> <mode>");
			System.out.println();
			System.out.println("  <instanceFileName> must be the name of a file which contains the representation of a CSP instance in format XCSP 2.0");
			System.out.println("  <seed> must be an integer that is used to shuffle variables and constraints");
			System.out.println("  <mode> must be equel to 1 (only variables shuffled), 2 (only constraints shuffled) and 3 (both variables and constraints shuffled)");
			System.out.println();
			System.out.println("With this usage, InstanceShuffler shuffles the given instance and saves the result in a new file (by appending _shf<seed> to the prefix of the file name)");
			System.out.println();
			System.out.println("Exit code of instanceShuffler is as follows:");
			System.out.println("  0 : no problem occurs and the new shuffled instance has been saved");
			System.out.println("  2 : a problem occurs (file not found, ...)");
			System.exit(0);
		}
		try {
			File file = new File(args[0]);
			if (file.isDirectory()) {
				System.err.println("PROBLEM \t you must give the name of a file (and not the name of a directory)");
				System.exit(2);
			}
			if (!file.exists()) {
				System.err.println("PROBLEM \t the file has not been found");
				System.exit(2);
			}
			InstanceShuffler instanceShuffler = new InstanceShuffler(file, Integer.parseInt(args[1]), Integer.parseInt(args[2]));
			instanceShuffler.treat();
			System.exit(0);

		} catch (Throwable e) {
			System.err.println("PROBLEM \t " + e.getMessage());
			System.exit(2);
		}
	}
}
