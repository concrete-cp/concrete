package result;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import cspfj.util.SAXErrorHandler;

import ml.options.OptionSet;
import ml.options.Options;
import ml.options.Options.Multiplicity;
import ml.options.Options.Separator;

public class ResolvedTime {

	private final static Logger logger = Logger
			.getLogger("result.ResolvedTime");

	private ResolvedTime() {
		super();
	}

	public static void main(final String[] args) {
		final Options opt = new Options(args, 80);
		opt.addSet("standard", 1, 80);

		opt.addOptionAllSets("w", Separator.BLANK, Multiplicity.ZERO_OR_ONE);
		opt.addOptionAllSets("d", Separator.BLANK, Multiplicity.ZERO_OR_ONE);
		
		final OptionSet set = opt.getMatchingSet();

		if (set == null) {
			logger
					.severe("Usage : java cspfj.Cspfj [-e maxTime] [-d level] instance");
			logger.severe(opt.getCheckErrors());
			System.exit(1);
		}

		Logger.getLogger("").getHandlers()[0].setLevel(Level.ALL);
//		Logger.getLogger("").getHandlers()[0].setFilter(new Filter() {
//			public boolean isLoggable(LogRecord record) {
//				// return true if the record should be logged;
//				// false otherwise.
//				return record.getSourceClassName().contains("AC3")
//						|| record.getSourceMethodName().contains("revise");
//			}
//		});

		if (set.isSet("d")) {
			Logger.getLogger("").setLevel(
					Level.parse((set.getOption("d").getResultValue(0))));
		} else {
			Logger.getLogger("").setLevel(Level.WARNING);
		}
		
		final List<Float[]> times = new ArrayList<Float[]>();

		int size = -1;

		for (String fileName : set.getData()) {
			final File dir = new File(fileName);
			final List<Float> timesList = getTimes(dir);
			final Float[] timesArray = timesList.toArray(new Float[timesList
					.size()]);
			if (size < 0) {
				size = timesArray.length;
			} else if (size != timesArray.length) {
				logger.warning("Not the same number of instances !");
				size = Math.max(size, timesArray.length);

			}
			Arrays.sort(timesArray);
			times.add(timesArray);
		}
		try {
			OutputStreamWriter out;
			if (set.isSet("w")) {

				out = new FileWriter(set.getOption("w").getResultValue(0));

			} else {
				out = new OutputStreamWriter(System.out);
			}

			for (int i = 0; i < size; i++) {
				out.write(Integer.toString(size - i));
				for (Float[] column : times) {
					if (i >= column.length) {
						out.write("\t"+Float.POSITIVE_INFINITY);
					} else {
						out.write("\t" + column[i]);
					}
				}
				out.write("\n");
			}

			out.close();
		} catch (IOException e) {
			logger.severe(e.toString());
		}
	}

	public static List<Float> getTimes(final File file) {

		if (file.isDirectory()) {

			logger.info("dir : " + file.getName());

			final List<Float> times = new ArrayList<Float>();

			for (String fileName : file.list()) {
				final File inFile = new File(file.getName() + File.separator
						+ fileName);
				if (inFile.isDirectory()) {
					continue;
				}

				try {
					logger.info("File " + inFile.getName());
					times.addAll(getTimes(new FileReader(inFile)));
				} catch (FileNotFoundException e) {
					// logger.warning("File not found : " + fileName) ;
					continue;
				}
			}
			return times;
		}

		try {
			return getTimes(new FileReader(file));
		} catch (FileNotFoundException e) {
			return null;
		}
	}

	public static List<Float> getTimes(final InputStreamReader file) {

		final DocumentBuilderFactory factory = DocumentBuilderFactory
				.newInstance();

		// factory.setNamespaceAware(true);
		// factory.setValidating(true);
		// factory.setAttribute(SCHEMA_LANGUAGE, XML_SCHEMA);
		// factory.setAttribute(SCHEMA_SOURCE, schema);

		Document doc = null;

		final List<Float> times = new ArrayList<Float>();

		try {
			final DocumentBuilder parser = factory.newDocumentBuilder();
			parser.setErrorHandler(new SAXErrorHandler());
			doc = parser.parse(new InputSource(file));
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			logger.warning("Incorrect file");
			return times;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		NodeList list = doc.getElementsByTagName("problem");

		for (int i = 0; i < list.getLength(); i++) {
			NodeList problemElements = list.item(i).getChildNodes();
			for (int j = 0 ; j < problemElements.getLength() ; j++) {
				Node problemElement = problemElements.item(j) ;
				if (problemElement.getNodeType() == Node.ELEMENT_NODE) {
					logger.info(problemElement.getNodeName()) ;
				}
				if ("result".equals(problemElement.getNodeName())) {
					if ("UNKNOWN".equals(problemElement.getTextContent())) {
						times.add(Float.POSITIVE_INFINITY) ;
						break ;
					}
					
				} else if ("statistics".equals(problemElement.getNodeName())) {
					logger.info(problemElement.getChildNodes().toString());
					times.add(Float.parseFloat(problemElement.getChildNodes().item(1)
							.getTextContent()));
				}
			}
			
		}

		return times;
	}

}
