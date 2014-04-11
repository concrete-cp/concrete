package abscon.instance;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.text.MessageFormat;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

public class XMLManager {
	private static boolean useStyleSheet = true; // true; // TODO //true; //false; // true // a style sheet can seriously degrade performances

	static class ErrorHandler extends DefaultHandler {
		private MessageFormat message = new MessageFormat("({0}: {1}, {2}): {3}");

		private void print(SAXParseException x) {
			String msg = message.format(new Object[] { x.getSystemId(), new Integer(x.getLineNumber()), new Integer(x.getColumnNumber()), x.getMessage() });
			System.out.println(msg);
		}

		public void warning(SAXParseException x) {
			print(x);
		}

		public void error(SAXParseException x) {
			print(x);
		}

		public void fatalError(SAXParseException x) throws SAXParseException {
			print(x);
			throw x;
		}
	}

	private static void dealWithException(Exception e) {
		if (e instanceof SAXParseException) {
			SAXParseException ee = (SAXParseException) e;
			System.out.println("\n** Parsing error" + ", line " + ee.getLineNumber() + ", uri " + ee.getSystemId());
			System.out.println("   " + ee.getMessage());
			Exception x = (ee.getException() == null ? ee : ee.getException());
			x.printStackTrace();
		} else if (e instanceof SAXException) {
			SAXException ee = (SAXException) e;
			Exception x = (ee.getException() == null ? ee : ee.getException());
			x.printStackTrace();
		} else if (e instanceof TransformerConfigurationException) {
			TransformerConfigurationException ee = (TransformerConfigurationException) e;
			System.out.println("\n** Transformer Factory error\n" + e.getMessage());
			Throwable x = (ee.getException() == null ? ee : ee.getException());
			x.printStackTrace();
		} else if (e instanceof TransformerException) {
			TransformerException ee = (TransformerException) e;
			System.out.println("\n** Transformation error" + e.getMessage());
			Throwable x = (ee.getException() == null ? ee : ee.getException());
			x.printStackTrace();
		} else {
			System.out.println(e);
			e.printStackTrace();
		}
		System.exit(1);
	}

	public static Document createNewDocument() {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try {
			DocumentBuilder builder = factory.newDocumentBuilder();
			return builder.newDocument();
		} catch (ParserConfigurationException e) {
			dealWithException(e);
			return null;
		}
	}

	/**
	 * Build a DOM object that corresponds to the given input stream.
	 * 
	 * @param inputStream the input stream that denotes the XML document to be loaded.
	 * @param schemaUrl the schema to be used (<code> null </code> if not used) to validate the document
	 * @return a DOM object
	 */
	public static Document load(InputStream is, URL schemaUrl) {
		try {
			DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
			documentBuilderFactory.setNamespaceAware(true);
			if (schemaUrl != null) {
				SchemaFactory schemaFactory = SchemaFactory.newInstance(InstanceTokens.W3C_XML_SCHEMA);
				Schema schema = schemaFactory.newSchema(schemaUrl); // new File(schemaFileName));
				documentBuilderFactory.setSchema(schema);
			}
			DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
			documentBuilder.setErrorHandler(new ErrorHandler());
			Document document = documentBuilder.parse(is);
			return document;
		} catch (Exception e) {
			dealWithException(e);
			return null;
		}
	}

	public static Document load(File file, URL schemaUrl) {
		try {
			return load(new FileInputStream(file), schemaUrl);
		} catch (FileNotFoundException e) {
			System.out.println("File " + file.getName() + " does not exist");
			System.exit(1);
			return null;
		}
	}

	public static Document load(InputStream is) {
		return load(is, null);
	}

	public static Document load(File file) {
		return load(file, null);
	}

	public static Document load(String fileName) {
		if (fileName.endsWith("xml.bz2")) {
			try {
				Process p = Runtime.getRuntime().exec("bunzip2 -c " + fileName);
				Document document = load(p.getInputStream());
				p.waitFor();
				p.exitValue();
				p.destroy();
				return document;

			} catch (Exception e) {
				System.out.println("Problem with " + fileName);
				System.exit(1);
				return null;
			}
		}
		return load(new File(fileName), null);
	}

	private static Transformer buildTransformer(InputStream styleSheetInputStream) {
		try {
			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			if (styleSheetInputStream == null || !useStyleSheet)
				return transformerFactory.newTransformer();
			Document document = load(styleSheetInputStream, null);
			DOMSource source = new DOMSource(document);
			return transformerFactory.newTransformer(source);
		} catch (TransformerConfigurationException e) {
			dealWithException(e);
			return null;
		}
	}

	public static void save(Document document, PrintWriter writer, InputStream styleSheetInputStream) {
		try {
			Transformer transformer = buildTransformer(styleSheetInputStream);
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			DOMSource source = new DOMSource(document);
			StreamResult result = new StreamResult(writer);
			transformer.transform(source, result);
		} catch (TransformerException e) {
			dealWithException(e);
		}
	}

	public static void save(Document document, String fileName, InputStream styleSheetInputStream) {
		try {
			PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)));
			save(document, out, styleSheetInputStream);
			out.close();
		} catch (IOException e) {
			dealWithException(e);
		}
	}

	public static void save(Document document, String fileName) {
		save(document, fileName, (InputStream)null);
	}

	public static void save(Document document, String fileName, String styleSheetFileName) {
		save(document, fileName, styleSheetFileName == null ? null : XMLManager.class.getResourceAsStream(styleSheetFileName));
	}

	public static Element getElementByTagNameFrom(Element element, String tagName, int i) {
		NodeList nodeList = element.getElementsByTagName(tagName);
		if (nodeList == null || nodeList.getLength() <= i)
			return null;
		return (Element) (nodeList.item(i));
	}

	public static Element getFirstElementByTagNameFromRoot(Document document, String tagName) {
		return getElementByTagNameFrom(document.getDocumentElement(), tagName, 0);
	}

	public static void deleteElementByTagNameFrom(Element element, String tagName, int i) {
		NodeList list = element.getElementsByTagName(tagName);
		if (list.getLength() <= i)
			throw new IllegalArgumentException();
		element.removeChild(list.item(i));
	}

	public static Node getChildElement(Document document, String elementName, int index) {
		NodeList list = (NodeList) document.getElementsByTagName(elementName);
		return list.item(index);
	}

	public static void deleteChildElement(Document document, String elementName, int index) {
		Element element = (Element) document.getElementsByTagName(elementName).item(0);
		element.removeChild(getChildElement(document, elementName, index));
	}

	public static String[] displayAttributes(Element element) {
		NamedNodeMap map = element.getAttributes();
		String[] values = new String[map.getLength()];
		for (int i = 0; i < map.getLength(); i++) {
			Attr attribut = (Attr) map.item(i);
			values[i] = attribut.getValue();
			System.out.println(attribut.getName() + " = " + values[i]);
		}
		return values;
	}

	public static void displayElement(Document document, String elementName) {
		Node node = document.getElementsByTagName(elementName).item(0);
		NodeList list = (NodeList) node.getChildNodes();
		for (int i = 0; i < list.getLength(); i++) {
			Element element = (Element) list.item(i);
			displayAttributes(element);
		}
	}

}
