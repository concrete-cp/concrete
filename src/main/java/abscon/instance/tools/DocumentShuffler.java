package abscon.instance.tools;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.StringTokenizer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import abscon.instance.InstanceTokens;
import abscon.instance.XMLManager;

public class DocumentShuffler {

	private String changeNames(String s, Map<String, String> map) {
		StringBuffer sb = new StringBuffer();
		StringTokenizer st = new StringTokenizer(s);
		String name = st.nextToken();
		sb.append(map.get(name));
		while (st.hasMoreTokens()) {
			name = st.nextToken();
			sb.append(" " + map.get(name));
		}
		return sb.toString();
	}

	Map<String, String> variableNamesMap = new HashMap<String, String>();

	private int[] buildPermutation(Random random, int size) {
		int[] values = new int[size];
		for (int i = 0; i < values.length; i++)
			values[i] = i;
		int nbValues = values.length;
		int[] t = new int[size];
		for (int i = 0; i < size; i++) {
			int j = random.nextInt(size - i);
			t[i] = values[j];
			values[j] = values[size - i - 1];
			nbValues--;
		}
		// System.out.print("Permutation = ");
		// for (int i = 0; i < t.length; i++)
		// System.out.print(t[i] + " ");
		// System.out.println();
		return t;
	}

	private Map<String, String> modifyOrder(Random random, Element parent, NodeList nodeList, boolean variables, int mode) {
		Map<String, String> variablesMap = new HashMap<String, String>();
		int[] permutation = buildPermutation(random, nodeList.getLength());
		Element[] elements = new Element[nodeList.getLength()];
		for (int i = 0; i < nodeList.getLength(); i++) {
			Element element = (Element) nodeList.item(i);
			String name = element.getAttribute(InstanceTokens.NAME);
			String canonicalName = null;
			if (variables)
				canonicalName = mode == 2 ? name : InstanceTokens.getVariableNameFor(permutation[i]);
			else
				canonicalName = mode == 1 ? name : InstanceTokens.getConstraintNameFor(permutation[i]);
			// String canonicalName = (variables ? InstanceTokens.getVariableNameFor(permutation[i]) : InstanceTokens.getConstraintNameFor(permutation[i]));
			element.setAttribute(InstanceTokens.NAME, canonicalName);
			variablesMap.put(name, canonicalName);
			if (variables)
				elements[mode == 2 ? i : permutation[i]] = element;
			else
				elements[mode == 1 ? i : permutation[i]] = element;
			//elements[permutation[i]] = element;
		}
		for (int i = 0; i < nodeList.getLength(); i++)
			parent.removeChild(nodeList.item(i));
		for (int i = 0; i < elements.length; i++)
			parent.appendChild(elements[i]);
		return variablesMap;
	}

	public Document shuffle(Document document, int seed, int mode) {
		Random random = new Random(seed);

		Element variablesElement = XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.VARIABLES);
		NodeList nodeList = variablesElement.getElementsByTagName(InstanceTokens.VARIABLE);
		Map<String, String> variablesMap = modifyOrder(random, variablesElement, nodeList, true,mode);

		// Map<String, String> relationsMap = new HashMap<String, String>();
		// Element rels = getFirstElementByTagNameFromRoot(document, XMLInstanceRepresentation.RELATIONS);
		// if (rels != null) {
		// nodeList = rels.getElementsByTagName(XMLInstanceRepresentation.RELATION);
		//
		// for (int i = 0; i < nodeList.getLength(); i++) {
		// Element element = (Element) nodeList.item(i);
		// String name = element.getAttribute(XMLInstanceRepresentation.NAME);
		// String canonicalName = XMLInstanceRepresentation.getRelationNameFor(i);
		// element.setAttribute(XMLInstanceRepresentation.NAME, canonicalName);
		// relationsMap.put(name, canonicalName);
		// }
		// }

		Element constraintsElement = XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.CONSTRAINTS);
		nodeList = constraintsElement.getElementsByTagName(InstanceTokens.CONSTRAINT);
		for (int i = 0; i < nodeList.getLength(); i++) {
			Element element = (Element) nodeList.item(i);
			// String name = element.getAttribute("name");
			String canonicalName = InstanceTokens.getConstraintNameFor(i);
			element.setAttribute(InstanceTokens.NAME, canonicalName);

			String scope = element.getAttribute(InstanceTokens.SCOPE);
			element.setAttribute(InstanceTokens.SCOPE, changeNames(scope, variablesMap));
			// String reference = element.getAttribute(XMLInstanceRepresentation.REFERENCE);
			Element parameters = XMLManager.getElementByTagNameFrom(element, InstanceTokens.PARAMETERS, 0);
			if (parameters != null) {
				String canonicalExpression = "";
				//System.out.println("before " + parameters.getTextContent());
				StringTokenizer st = new StringTokenizer(parameters.getTextContent());
				while (st.hasMoreTokens()) {
					String token = st.nextToken();
					if (variablesMap.containsKey(token))
						canonicalExpression += " " + variablesMap.get(token);
					else
						canonicalExpression += " " + token;
				}
				//System.out.println("after " + canonicalExpression);
				
				parameters.setTextContent(canonicalExpression.trim());
			}
		}

		modifyOrder(random, constraintsElement, nodeList, false,mode);
		return document;
	}
}
