package abscon.instance.tools;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.PrintWriter;

import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import abscon.instance.InstanceTokens;
import abscon.instance.XMLManager;
import abscon.instance.tools.InstanceChecker.Indicator;

public class InstanceCheckerEngine extends Thread {
	public static final String DEFAULT_PREFIX = "inst";

	private InstanceChecker checkerGraphic;

	private Indicator indicator;

	private File srcDirectory;

	private File dstDirectory;

	private boolean defaultFileName;

	private InstanceChecker.CHECKING_MODE mode;

	private int counter1 = 0;

	private int counter2 = 0;

	private int counter3 = 0;

	private boolean finished;

	private boolean overwrite;

	private boolean overwriteDecided;

	private boolean competitionControl;

	public void setOverwrite(boolean overwrite) {
		this.overwrite = overwrite;
	}

	public void setOverwriteDecided(boolean overwriteDecided) {
		this.overwriteDecided = overwriteDecided;
	}

	public InstanceCheckerEngine(InstanceChecker checkerGraphic, Indicator indicator, File srcDirectory, File dstDirectory, boolean defaultFileName, boolean competitionControl,
			InstanceChecker.CHECKING_MODE mode) {
		this.checkerGraphic = checkerGraphic;
		this.indicator = indicator;
		this.srcDirectory = srcDirectory;
		this.dstDirectory = dstDirectory;
		if (dstDirectory != null && !dstDirectory.exists())
			dstDirectory.mkdirs();
		this.defaultFileName = defaultFileName;
		this.competitionControl = competitionControl;
		this.mode = mode;
	}

	public void setFinished(boolean b) {
		finished = b;
	}

	private boolean mustBeTreated(String fileName) {
		fileName = fileName.toLowerCase();
		return fileName.endsWith("xml") || fileName.endsWith("bz2");
	}

	private String valueOf(int cpt) {
		return (cpt < 10 ? "000" : cpt < 100 ? "00" : cpt < 1000 ? "0" : "") + cpt;
	}

	private String getNameOfFileToSave(File srcFile, boolean containsPredicatesBefore) {
		String basename = (defaultFileName ? DEFAULT_PREFIX + valueOf(counter1) : srcFile.getName().substring(0, srcFile.getName().lastIndexOf(".")));
		// basename = basename.replaceFirst(".int", "");
		// basename = basename.replaceFirst(".ext", "");
		if (containsPredicatesBefore && mode == InstanceChecker.CHECKING_MODE.EXTENSIONAL)
			return basename + "_ext.xml";

		String suffix = ".xml"; // (containsPredicates && mode != CheckerConstants.EXTENSIONAL ? ".int" : "_ext") + ".xml";
		return basename + suffix;
	}

	private String getPathOfFileToSave(File srcFile, boolean containsPredicates) {
		if (srcDirectory.equals(dstDirectory))
			return srcFile.getParent();
		if (defaultFileName)
			return dstDirectory.getAbsolutePath();
		return dstDirectory.getAbsolutePath() + File.separator + srcFile.getParent().replaceFirst(srcDirectory.getAbsolutePath(), "");
	}

	private PrintWriter buildPrintWriterFor(File srcFile, boolean containsPredicates) throws Exception {
		String fileName = getNameOfFileToSave(srcFile, containsPredicates);
		String pathName = getPathOfFileToSave(srcFile, containsPredicates);
		String absoluteFileName = pathName + File.separator + fileName;

		File f = new File(pathName);
		f.mkdirs();
		f = new File(f, fileName);
		if (f.exists()) {
			if (!overwriteDecided) {
				int result = JOptionPane.showConfirmDialog(null, "Do you want to overwrite ALL files (including " + fileName + ") ?", "", JOptionPane.YES_NO_OPTION);
				overwrite = (result == JOptionPane.YES_OPTION);
				overwriteDecided = true;
			}
			if (!overwrite) {
				fileName = fileName + ".new";
				absoluteFileName = pathName + File.separator + fileName;
			}
		}
		return new PrintWriter(new FileOutputStream(absoluteFileName));
	}

	public void treat(File srcFile) throws Exception {
		assert srcFile.getName().toLowerCase().endsWith("xml") || srcFile.getName().toLowerCase().endsWith("xml.bz2");

		DocumentModifier translator = new DocumentModifier();
		Document document = null;
		InstanceCheckerParser problem = null;

		indicator.write("    loading XML document " + srcFile.getName() + "...");
		if (srcFile.getName().toLowerCase().endsWith("xml.bz2")) {
			document = XMLManager.load(srcFile.getAbsolutePath());
		} else {
			FileInputStream in = new FileInputStream(srcFile);
			document = XMLManager.load(in, null); // Nada.class.getResource(XMLInstanceRepresentation.INSTANCE_SCHEMA_2_0));
			in.close();
		}
		indicator.write("ok\n");
		indicator.write("    building problem...");
		problem = new InstanceCheckerParser(this, document, competitionControl);
		indicator.write("ok\n");
		indicator.write("    checking validity...");
		// validityProblem = true;
		problem.checkValidity();
		// validityProblem = false;
		indicator.write("ok\n");

		if (mode == InstanceChecker.CHECKING_MODE.VALIDATION)
			return;

		boolean containsPredicateBefore = problem.getPredicatesMap().size() > 0 || problem.getFunctionsMap().size() > 0;
		PrintWriter out = buildPrintWriterFor(srcFile, containsPredicateBefore);

		// TODO mixer ce qui suit avec la gestion forme canonique
		if (containsPredicateBefore && mode == InstanceChecker.CHECKING_MODE.EXTENSIONAL) {
			indicator.write("    converting predicates...");
			problem.convertToExtension();
			indicator.write("ok\n");
			indicator.write("    modifying XML document...");
			document = translator.modifyDocumentFrom(this, document, problem);
			problem.updateStructures();
			indicator.write("ok\n");
		}

		indicator.write("    setting to canonical form...");
		document = translator.setCanonicalFormOf(this, document, problem.hasCanonicalNames(),problem.getMaxConstraintRAity());
		indicator.write("ok\n");
		problem = null;

		indicator.write("    saving " + getNameOfFileToSave(srcFile, containsPredicateBefore) + "...");
		XMLManager.save(document, out, XMLManager.class.getResourceAsStream(InstanceTokens.INSTANCE_STYLESHEET_2_1));
		indicator.write("ok\n");

		out.close();
		// return fileName;
	}

	private void operateFile(File srcFile) {
		// assert !srcFile.isDirectory();

		if (!mustBeTreated(srcFile.getName())) {
			System.out.println(srcFile.getAbsolutePath() + " ignored");
			counter3++;
		} else {
			indicator.write(srcFile.getName() + "\n");
			try {
				treat(srcFile);
				counter1++;
			} catch (Exception e) {
				counter2++;
				// e.printStackTrace();
				indicator.write("  ERROR as " + e.getMessage() + " " + "\n");
			}
		}
		if (checkerGraphic != null)
			checkerGraphic.updateCounters(counter1, counter2, counter3);
	}

	private void operateDirectory(File dir) {
		String[] list = dir.list();
		for (int i = 0; i < list.length; i++)
			operate(new File(dir, list[i]));
	}

	private void operate(File file) {
		if (finished)
			return;
		if (file.isDirectory() == false)
			operateFile(file);
		else
			operateDirectory(file);
	}

	public void run() {
		long start = System.currentTimeMillis();
		try {
			overwriteDecided = false;
			operate(srcDirectory);
		} catch (OutOfMemoryError e) {
			indicator.write(" OUT OF MEMORY ERROR - the program is going to be stopped");
			System.out.println("OUT OF MEMORY ERROR - the program has been stopped");
			System.exit(1);
		}

		long stop = System.currentTimeMillis();
		if (checkerGraphic != null)
			checkerGraphic.endOfCoder(counter1, counter2, counter3, stop - start);

	}

	public void spot() {
		indicator.write(".");
	}

	public void write(String s) {
		indicator.write(s);
	}
}
