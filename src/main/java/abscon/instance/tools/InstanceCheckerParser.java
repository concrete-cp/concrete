package abscon.instance.tools;

import java.util.ArrayList;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import abscon.instance.InstanceTokens;
import abscon.instance.Toolkit;
import abscon.instance.XMLManager;
import abscon.instance.PredicateTokens.RelationalOperator;
import abscon.instance.components.PAllDifferent;
import abscon.instance.components.PConstraint;
import abscon.instance.components.PCumulative;
import abscon.instance.components.PDomain;
import abscon.instance.components.PElement;
import abscon.instance.components.PExtensionConstraint;
import abscon.instance.components.PFunction;
import abscon.instance.components.PIntensionConstraint;
import abscon.instance.components.PPredicate;
import abscon.instance.components.PRelation;
import abscon.instance.components.PSoftRelation;
import abscon.instance.components.PVariable;
import abscon.instance.components.PWeightedSum;
import abscon.instance.components.Task;
import abscon.instance.intension.EvaluationManager;

public class InstanceCheckerParser {
	private InstanceCheckerEngine engine;

	private String instanceName;

	private String format;

	private String type;

	private Set<String> allNameIdentifiers;

	private Map<String, PDomain> mapOfDomains;

	private Map<String, PVariable> mapOfVariables;

	private Map<String, PRelation> mapOfRelations;

	private Map<String, PFunction> mapOfFunctions;

	private Map<String, PPredicate> mapOfPredicates;

	private Map<String, PConstraint> mapOfConstraints;

	private int maxConstraintArity;

	private String[] domainNames;

	private String[] variableNames;

	private String[] relationNames;

	private String[] functionNames;

	private String[] predicateNames;

	private String[] constraintNames;

	private boolean competitionControl = false;

	// private int maximalCost;

	public int getMaxConstraintRAity() {
		return maxConstraintArity;
	}

	private List<PRelation> newRelations;

	public List<PRelation> getNewRelations() {
		return newRelations;
	}

	private Map<String, String> constraintsToNewRelations;

	class FormatException extends Exception {
		private static final long serialVersionUID = -4742071198645091018L;

		public FormatException(String message) {
			super(message);
		}
	}

	public String getName() {
		return instanceName;
	}

	public Map<String, PRelation> getRelationsMap() {
		return mapOfRelations;
	}

	public Map<String, PFunction> getFunctionsMap() {
		return mapOfFunctions;
	}

	public Map<String, PPredicate> getPredicatesMap() {
		return mapOfPredicates;
	}

	public Map<String, String> getConstraintsToNewRelations() {
		return constraintsToNewRelations;
	}

	public boolean hasCanonicalNames() {
		for (int i = 0; i < domainNames.length; i++)
			if (!domainNames[i].equals(InstanceTokens.getDomainNameFor(i)))
				return false;
		for (int i = 0; i < variableNames.length; i++)
			if (!variableNames[i].equals(InstanceTokens.getVariableNameFor(i)))
				return false;
		if (relationNames != null)
			for (int i = 0; i < relationNames.length; i++)
				if (!relationNames[i].equals(InstanceTokens.getRelationNameFor(i))) {
					System.out.println(" the " + i + "th relation is called " + relationNames[i]);
					return false;
				}
		if (functionNames != null) {
			for (int i = 0; i < functionNames.length; i++)
				if (!functionNames[i].equals(InstanceTokens.getFunctionNameFor(i)))
					return false;
			for (PFunction function : mapOfFunctions.values()) {
				String[] formalParameters = function.getFormalParameters();
				for (int i = 0; i < formalParameters.length; i++)
					if (!formalParameters[i].equals(InstanceTokens.getParameterNameFor(i)))
						return false;
			}
		}
		if (predicateNames != null) {
			for (int i = 0; i < predicateNames.length; i++)
				if (!predicateNames[i].equals(InstanceTokens.getPredicateNameFor(i)))
					return false;
			for (PPredicate predicate : mapOfPredicates.values()) {
				String[] formalParameters = predicate.getFormalParameters();
				for (int i = 0; i < formalParameters.length; i++)
					if (!formalParameters[i].equals(InstanceTokens.getParameterNameFor(i)))
						return false;
			}
		}
		for (int i = 0; i < constraintNames.length; i++)
			if (!constraintNames[i].equals(InstanceTokens.getConstraintNameFor(i)))
				return false;
		return true;
	}

	private int parseInt(String attribute, String value) throws FormatException {
		try {
			return Integer.parseInt(value);
		} catch (NumberFormatException e) {
			throw new FormatException(attribute + " is not given an integer value (or does not exist)");
		}
	}

	private int parsePositiveInt(String attribute, String value) throws FormatException {
		int v = parseInt(attribute, value);
		if (v < 0)
			throw new FormatException(attribute + " is not strictly positive");
		return v;
	}

	private int parseStrictlyPositiveInt(String attribute, String value) throws FormatException {
		int v = parseInt(attribute, value);
		if (v <= 0)
			throw new FormatException(attribute + " is not strictly positive");
		return v;
	}

	private String nextToken(String constraintName, StringTokenizer st) throws FormatException {
		try {
			return st.nextToken();
		} catch (Exception e) {
			throw new FormatException("Ill-formed Parameters of " + constraintName);
		}
	}

	private void checkAndRecord(String identifier, String context) throws FormatException {
		if (identifier.length() == 0)
			throw new FormatException("Missing name attribute for a " + context + ".");
		for (int i = 0; i < identifier.length(); i++) {
			char c = identifier.charAt(i);
			if (c != '_' && !Character.isLetterOrDigit(c) && c != '-')
				throw new FormatException("Invalid name attribute " + identifier + " for a " + context);
		}
		if (allNameIdentifiers.contains(identifier))
			throw new FormatException("Invalid name attribute " + identifier + " as it occurs at least twice in the description of the instance");
		allNameIdentifiers.add(identifier);
	}

	private void parsePresentation(Element presentationElement) throws FormatException {
		if (presentationElement == null)
			throw new FormatException("The element presentation is absent.");

		instanceName = presentationElement.getAttribute(InstanceTokens.NAME);
		if (!instanceName.equals("") && !instanceName.equals("?"))
			checkAndRecord(instanceName, "presentation");

		format = presentationElement.getAttribute(InstanceTokens.FORMAT);
		if (!format.equals(InstanceTokens.XCSP_2_0) && !format.equals(InstanceTokens.XCSP_2_1))
			throw new FormatException("The value of the attribute format of presentation is not valid as it is different from XCSP 2.0 and XCSP 2.1.");
		type = presentationElement.getAttribute(InstanceTokens.TYPE.trim());
		type = type.length() == 0 || type.equals("?") ? InstanceTokens.CSP : type;
		if (!type.equals(InstanceTokens.CSP) && !type.equals(InstanceTokens.WCSP))
			throw new FormatException("The value of the attribute type of presentation is not valid as it is different from CSP and WCSP.");
		if (type.equals(InstanceTokens.WCSP) && !format.equals(InstanceTokens.XCSP_2_1))
			throw new FormatException("For WCSP, only format " + InstanceTokens.XCSP_2_1 + " is currently accepted.");

		if (competitionControl) {
			if (instanceName.length() != 0 && !instanceName.equals("?"))
				throw new FormatException("For the 2008 competition, the value of the attribute name of presentation must be absent or set to ?.");
			String max = presentationElement.getAttribute(InstanceTokens.MAX_CONSTRAINT_ARITY);
			if (max.length() == 0)
				throw new FormatException("For the 2008 competition, the attribute maxConstraintArity must be present.");
			if (presentationElement.getTextContent().length() != 0)
				throw new FormatException("For the 2008 competition, the content of the element presentation must be empty.");
			String solution = presentationElement.getAttribute(InstanceTokens.SOLUTION);
			if (solution.length() != 0)
				throw new FormatException("For the 2008 competition, the attribute solution of presentation must be removed.");
			String nbSolutions = presentationElement.getAttribute(InstanceTokens.NB_SOLUTIONS);
			if (nbSolutions.length() != 0)
				throw new FormatException("For the 2008 competition, the attribute nbSolutions of presentation must be removed.");
		}
	}

	private int[] parseDomainValues(String domainName, int nbValues, String s) throws FormatException {
		int[] values = new int[nbValues];
		StringTokenizer st = new StringTokenizer(s);
		int cnt = 0;
		while (st.hasMoreTokens() && cnt < values.length) {
			String token = st.nextToken();
			int position = token.indexOf(InstanceTokens.DISCRETE_INTERVAL_SEPARATOR);
			if (position == -1)
				values[cnt++] = parseInt("pb with value " + token + " in domain " + domainName, token);
			else {
				int min = parseInt("pb with min value of interval " + token + " in domain " + domainName, token.substring(0, position));
				int max = parseInt("pb with max value of interval " + token + " in domain " + domainName, token.substring(position + InstanceTokens.DISCRETE_INTERVAL_SEPARATOR.length()));
				if (max < min)
					throw new FormatException("pb with interval (max > min) " + token + " in domain " + domainName);
				if (cnt + (max - min) >= values.length)
					throw new FormatException("The number of values in the domain " + domainName + " is different from the value of nbValues.");
				for (int i = min; i <= max; i++)
					values[cnt++] = i;
			}
		}
		if (st.hasMoreTokens())
			throw new FormatException("The number of values in the domain " + domainName + " is different from the value of nbValues.");
		for (int i = 1; i < values.length; i++)
			if (values[i - 1] >= values[i])
				throw new FormatException("In domain " + domainName + ", the two values " + values[i - 1] + " and " + values[i] + " are either equal or not given in increasing order");
		return values;
	}

	private PDomain parseDomain(Element domainElement) throws FormatException {
		String name = domainElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "domain");
		int nbValues = parseStrictlyPositiveInt(InstanceTokens.NB_VALUES + " of domain " + name + " ", domainElement.getAttribute(InstanceTokens.NB_VALUES));
		return new PDomain(name, parseDomainValues(name, nbValues, domainElement.getTextContent()));
	}

	private void parseDomains(Element domainsElement) throws FormatException {
		if (domainsElement == null)
			throw new FormatException("The element domains is absent.");

		mapOfDomains = new HashMap<String, PDomain>();
		int nbDomains = parseStrictlyPositiveInt(InstanceTokens.NB_DOMAINS + " of element domains ", domainsElement.getAttribute(InstanceTokens.NB_DOMAINS));
		domainNames = new String[nbDomains];

		NodeList nodeList = domainsElement.getElementsByTagName(InstanceTokens.DOMAIN);
		if (nbDomains != nodeList.getLength())
			throw new FormatException("the value of nbDomains does not correspond to the number of domains");
		for (int i = 0; i < nodeList.getLength(); i++) {
			PDomain domain = parseDomain((Element) nodeList.item(i));
			mapOfDomains.put(domain.getName(), domain);
			domainNames[i] = domain.getName();
		}
	}

	private PVariable parseVariable(Element variableElement) throws FormatException {
		String name = variableElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "variable");
		String domainName = variableElement.getAttribute(InstanceTokens.DOMAIN);
		PDomain domain = mapOfDomains.get(domainName);
		if (domain == null)
			throw new FormatException("variable " + name + " has an unknown associated domain " + domainName);
		return new PVariable(name, domain);
	}

	private void parseVariables(Element variablesElement) throws FormatException {
		if (variablesElement == null)
			throw new FormatException("The element variables is absent.");

		mapOfVariables = new HashMap<String, PVariable>();
		int nbVariables = parseStrictlyPositiveInt(InstanceTokens.NB_VARIABLES + " of element variables ", variablesElement.getAttribute(InstanceTokens.NB_VARIABLES));
		variableNames = new String[nbVariables];

		NodeList nodeList = variablesElement.getElementsByTagName(InstanceTokens.VARIABLE);
		if (nbVariables != nodeList.getLength())
			throw new FormatException("the value of nbVariables does not correspond to the number of variables");
		for (int i = 0; i < nodeList.getLength(); i++) {
			PVariable variable = parseVariable((Element) nodeList.item(i));
			mapOfVariables.put(variable.getName(), variable);
			variableNames[i] = variable.getName();
		}
	}

	private String listOf(int[] t, int limit) {
		String s = "(";
		for (int i = 0; i < limit; i++)
			s += t[i] + (i < limit - 1 ? " " : ")");
		return s;
	}

	private String getStringOf(int[] t) {
		StringBuffer sb = new StringBuffer();
		sb.append('(');
		for (int i = 0; i < t.length; i++)
			sb.append(t[i] + (i < t.length - 1 ? "," : ""));
		sb.append(')');
		return sb.toString();
	}

	private int[] weights;

	private int[][] parseTuples(String name, int nbTuples, int arity, String semantics, String textContent) throws FormatException {
		int[][] tuples = new int[nbTuples][arity];
		if (semantics.equals(InstanceTokens.SOFT))
			weights = new int[nbTuples];
		int currentCost = -2;

		StringTokenizer st1 = new StringTokenizer(textContent, InstanceTokens.TUPLES_SEPARATOR);
		int i = 0;
		while (st1.hasMoreTokens() && i < nbTuples) {
			StringTokenizer st2 = new StringTokenizer(st1.nextToken().trim());
			String token = st2.nextToken();
			if (semantics.equals(InstanceTokens.SOFT)) {
				int costFlagPosition = token.lastIndexOf(InstanceTokens.COST_SEPARATOR);
				if (costFlagPosition != -1) {
					currentCost = parseInt("Problem with the cost in " + token + " of relation " + name, token.substring(0, costFlagPosition));
					token = token.substring(costFlagPosition + 1);
				}
				if (currentCost < 0)
					throw new FormatException("No cost (or a negative cost) is associated with a tuple in relation " + name);
				weights[i] = currentCost;
			}
			tuples[i][0] = parseInt("Problem with the token " + token + " in relation " + name, token);
			int j = 1;
			while (st2.hasMoreTokens() && j < arity)
				tuples[i][j++] = parseInt("Problem  with a tuple in relation " + name, st2.nextToken());
			if (j < arity || st2.hasMoreTokens())
				throw new FormatException("There is a problem with tuple starting with " + listOf(tuples[i], j) + " in relation " + name + ". It has a size different from the indicated arity.");
			if (i > 0 && Toolkit.lexicographicComparator.compare(tuples[i - 1], tuples[i]) >= 0)
				throw new FormatException("In relation " + name + ", the two tuples " + getStringOf(tuples[i - 1]) + " and " + getStringOf(tuples[i])
						+ " are either equal or not given in lexicographic order: ");
			i++;
		}
		if (i < nbTuples || st1.hasMoreTokens())
			throw new FormatException("The number of tuples in relation " + name + (i < nbTuples ? ", which is " + i : ", which is greater than " + nbTuples) + ", is different from nbTuples = "
					+ nbTuples);
		return tuples;
	}

	private PRelation parseRelation(Element relationElement) throws FormatException {
		String name = relationElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "relation");

		int arity = parseStrictlyPositiveInt(InstanceTokens.ARITY + " of relation " + name + " ", relationElement.getAttribute(InstanceTokens.ARITY));
		int nbTuples = parsePositiveInt(InstanceTokens.NB_TUPLES + " of relation " + name + " ", relationElement.getAttribute(InstanceTokens.NB_TUPLES));
		String semantics = relationElement.getAttribute(InstanceTokens.SEMANTICS);
		if (semantics.equals(InstanceTokens.SOFT)) {
			if (!type.equals(InstanceTokens.WCSP))
				throw new FormatException("There is a soft relation whose name is " + name + " in the instance but the type is not declared as WCSP");
		} else if (!semantics.equals(InstanceTokens.SUPPORTS) && !semantics.equals(InstanceTokens.CONFLICTS))
			throw new FormatException("There is a bad (or missing) value of the semantics attribute for the relation " + name);
		int[][] tuples = parseTuples(name, nbTuples, arity, semantics, relationElement.getTextContent());
		if (semantics.equals(InstanceTokens.SOFT)) {
			String s = relationElement.getAttribute(InstanceTokens.DEFAULT_COST);
			int defaultCost = s.equals(InstanceTokens.INFINITY) ? Integer.MAX_VALUE : parsePositiveInt("pb with defaultCost " + s + " of relation " + name, s);
			return new PSoftRelation(name, arity, nbTuples, semantics, tuples, weights, defaultCost);
		} else
			return new PRelation(name, arity, nbTuples, semantics, tuples);
	}

	private void parseRelations(Element relationsElement) throws FormatException {
		mapOfRelations = new HashMap<String, PRelation>();
		if (relationsElement == null) {
			relationNames = new String[0];
			return;
		}
		int nbRelations = parsePositiveInt(InstanceTokens.NB_RELATIONS + " of element relations ", relationsElement.getAttribute(InstanceTokens.NB_RELATIONS));
		relationNames = new String[nbRelations];

		NodeList nodeList = relationsElement.getElementsByTagName(InstanceTokens.RELATION);
		if (nbRelations != nodeList.getLength())
			throw new FormatException("the value of nbRelations does not correspond to the number of relations which is " + nodeList.getLength());
		for (int i = 0; i < nodeList.getLength(); i++) {
			PRelation relation = parseRelation((Element) nodeList.item(i));
			mapOfRelations.put(relation.getName(), relation);
			relationNames[i] = relation.getName();
		}
	}

	private PFunction parseFunction(Element functionElement) throws FormatException {
		String name = functionElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "function");

		Element parameters = XMLManager.getElementByTagNameFrom(functionElement, InstanceTokens.PARAMETERS, 0);
		if (parameters == null)
			throw new FormatException("Missing parameters child element in function " + name);
		Element expression = XMLManager.getElementByTagNameFrom(functionElement, InstanceTokens.EXPRESSION, 0);
		if (expression == null)
			throw new FormatException("Missing expression child element in function " + name);
		Element functional = XMLManager.getElementByTagNameFrom(expression, InstanceTokens.FUNCTIONAL, 0);
		if (functional == null)
			throw new FormatException("Missing functional child element in function " + name);
		try {
			PFunction function = new PFunction(name, parameters.getTextContent(), functional.getTextContent());
			if (function.getFormalParameters() == null)
				throw new FormatException("Function " + name + " involves twice a formal parameter ");
			EvaluationManager evaluationManager = new EvaluationManager(function.getUniversalPostfixExpression());
			if (!evaluationManager.controlArityOfEvaluators() || !evaluationManager.controlTypeOfEvaluators(false))
				throw new FormatException("ill-formed expression " + functional.getTextContent() + " of function " + name);
			return function;
		} catch (Exception e) {
			throw new FormatException("ill-formed expression " + functional.getTextContent() + " of function " + name);
		}
	}

	private void parseFunctions(Element functionsElement) throws FormatException {
		mapOfFunctions = new HashMap<String, PFunction>();
		if (functionsElement == null) {
			functionNames = new String[0];
			return;
		}
		if (!type.equals(InstanceTokens.WCSP))
			throw new FormatException("There is a function in the instance but the type is not declared as WCSP");

		int nbFunctions = parsePositiveInt(InstanceTokens.NB_FUNCTIONS + " of element functions ", functionsElement.getAttribute(InstanceTokens.NB_FUNCTIONS));
		functionNames = new String[nbFunctions];

		NodeList nodeList = functionsElement.getElementsByTagName(InstanceTokens.FUNCTION);
		if (nbFunctions != nodeList.getLength())
			throw new FormatException("the value of nbFunctions does not correspond to the number of functions");
		for (int i = 0; i < nodeList.getLength(); i++) {
			PFunction function = parseFunction((Element) nodeList.item(i));
			mapOfFunctions.put(function.getName(), function);
			functionNames[i] = function.getName();
		}
	}

	private PPredicate parsePredicate(Element predicateElement) throws FormatException {
		String name = predicateElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "predicate");

		Element parameters = XMLManager.getElementByTagNameFrom(predicateElement, InstanceTokens.PARAMETERS, 0);
		if (parameters == null)
			throw new FormatException("Missing parameters child element in predicate " + name);
		Element expression = XMLManager.getElementByTagNameFrom(predicateElement, InstanceTokens.EXPRESSION, 0);
		if (expression == null)
			throw new FormatException("Missing expression child element in predicate " + name);
		Element functional = XMLManager.getElementByTagNameFrom(expression, InstanceTokens.FUNCTIONAL, 0);
		if (functional == null)
			throw new FormatException("Missing functional child element in predicate " + name);
		try {
			PPredicate predicate = new PPredicate(name, parameters.getTextContent(), functional.getTextContent());
			if (predicate.getFormalParameters() == null)
				throw new FormatException("Predicate " + name + " involves twice a formal parameter ");
			EvaluationManager evaluationManager = new EvaluationManager(predicate.getUniversalPostfixExpression());
			if (!evaluationManager.controlArityOfEvaluators() || !evaluationManager.controlTypeOfEvaluators(true))
				throw new FormatException("ill-formed expression " + functional.getTextContent() + " of predicate " + name);
			return predicate;
		} catch (Exception e) {
			throw new FormatException("ill-formed expression " + functional.getTextContent() + " of predicate " + name);
		}
	}

	private void parsePredicates(Element predicatesElement) throws FormatException {
		mapOfPredicates = new HashMap<String, PPredicate>();
		if (predicatesElement == null) {
			predicateNames = new String[0];
			return;
		}

		int nbPredicates = parsePositiveInt(InstanceTokens.NB_PREDICATES + " of element predicates ", predicatesElement.getAttribute(InstanceTokens.NB_PREDICATES));
		predicateNames = new String[nbPredicates];

		NodeList nodeList = predicatesElement.getElementsByTagName(InstanceTokens.PREDICATE);
		if (nbPredicates != nodeList.getLength())
			throw new FormatException("the value of nbPredicates does not correspond to the number of predicates");
		for (int i = 0; i < nodeList.getLength(); i++) {
			PPredicate predicate = parsePredicate((Element) nodeList.item(i));
			mapOfPredicates.put(predicate.getName(), predicate);
			predicateNames[i] = predicate.getName();
		}
	}

	private int searchIn(String s, PVariable[] variables) {
		for (int i = 0; i < variables.length; i++)
			if (variables[i].getName().equals(s))
				return i;
		return -1;
	}

	private void checkEffectiveParameters(String name, PVariable[] variables, String[] parameters) throws FormatException {
		boolean[] found = new boolean[variables.length];
		for (String token : parameters) {
			Long l = Toolkit.parseLong(token);
			if (l != null)
				continue;
			int position = searchIn(token, variables);
			if (position != -1)
				found[position] = true;
			else
				throw new FormatException("The effective parameter " + token + " does not correspond to an integer or an involved variable of constraint " + name);
		}
		for (int i = 0; i < found.length; i++)
			if (!found[i])
				throw new FormatException("The variable " + variables[i].getName() + " does not occur in the list of effective parameters of constraint " + name);
	}

	private void controlInvolvedVariablesWrtScope(Set<PVariable> involvedVariablesInParameters, PVariable[] scope, String name) throws FormatException {
		if (involvedVariablesInParameters.size() != scope.length)
			throw new FormatException("The number of variables occuring in scope is different from the number of variables occuring in parameters of constraint " + name);
		for (int i = 0; i < scope.length; i++)
			if (!involvedVariablesInParameters.contains(scope[i]))
				throw new FormatException("One variable of the scope of constraint " + name + " does not occur in parameters.");
	}

	private PConstraint parseElementConstraint(String name, PVariable[] scope, Element parameters) throws FormatException {
		StringTokenizer st = new StringTokenizer(Toolkit.insertWhitespaceAround(parameters.getTextContent(), InstanceTokens.BRACKETS));
		Set<PVariable> involvedVariablesInParameters = new HashSet<PVariable>();
		PVariable index = mapOfVariables.get(nextToken(name, st)); // index is necessarily a variable

		involvedVariablesInParameters.add(index);
		if (!nextToken(name, st).equals("["))
			throw new FormatException("One should find [ as second token of the parameters of constraint " + name);
		List<Object> table = new ArrayList<Object>();
		String token = nextToken(name, st);
		while (!token.equals("]")) {
			Object object = mapOfVariables.get(token);
			if (object == null)
				object = parseInt("Pb with a token in parameters of table of constraint Element " + name, token);
			else
				involvedVariablesInParameters.add((PVariable) object);
			table.add(object);
			token = nextToken(name, st);
		}
		token = nextToken(name, st);
		Object value = mapOfVariables.get(token);
		if (value == null)
			value = parseInt("Pb with the value token in parameters of constraint Element " + name, token);
		else
			involvedVariablesInParameters.add((PVariable) value);
		if (st.hasMoreTokens())
			throw new FormatException("Too many tokens in the parameters of constraint " + name);
		controlInvolvedVariablesWrtScope(involvedVariablesInParameters, scope, name);

		if (!index.getDomain().controlValueRanging(1, table.size()))
			throw new FormatException("The domain of variable index not valid in constraint " + name);

		return new PElement(name, scope, index, table.toArray(new Object[table.size()]), value);
	}

	private PConstraint parseWeightedSumConstraint(String name, PVariable[] scope, Element parameters) throws FormatException {
		NodeList nodeList = parameters.getChildNodes();
		if (nodeList.getLength() != 3)
			throw new FormatException("Ill-formed parameters of constraint " + name);
		StringTokenizer st = new StringTokenizer(Toolkit.insertWhitespaceAround(nodeList.item(0).getTextContent(), InstanceTokens.BRACKETS));
		int[] coeffs = new int[scope.length];
		if (!nextToken(name, st).equals("["))
			throw new FormatException("One should find [ as first token of the parameters of constraint " + name);
		String token = nextToken(name, st);
		while (!token.equals("]")) {
			if (!token.equals("{"))
				throw new FormatException("One should find { as first token of a pair (coefficient, variable) in constraint " + name);
			String coeffToken = nextToken(name, st);
			int position = searchIn(nextToken(name, st), scope);
			if (position == -1)
				throw new FormatException("Ill-formed parameters of constraint " + name);
			// System.out.println(coeffToken + " " + position);
			coeffs[position] += parseInt("One coefficient " + token + " of the parameters of constraint " + name, coeffToken);
			if (!nextToken(name, st).equals("}"))
				throw new FormatException("One should find } as last token of a pair (coefficient, variable) in constraint " + name);
			token = nextToken(name, st);
		}
		for (int i = 0; i < coeffs.length; i++)
			if (coeffs[i] == 0)
				throw new FormatException("One variable is not associated with a coefficient in constraint " + name);
		RelationalOperator operator = RelationalOperator.getRelationalOperatorFor(nodeList.item(1).getNodeName());
		if (operator == null)
			throw new FormatException("Relational operator in parameters of constraint " + name);
		int limit = parseInt("limit value for weightedSum constraint " + name, nodeList.item(2).getTextContent().trim());
		return new PWeightedSum(name, scope, coeffs, operator, limit);
	}

	private String buildStringRepresentationOf(Element parameters) {
		NodeList nodeList = parameters.getChildNodes();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node.getNodeName().equals(InstanceTokens.NIL)) {
				sb.append(" ");
				sb.append(InstanceTokens.NIL);
				sb.append(" ");
			} else
				sb.append(Toolkit.insertWhitespaceAround(node.getTextContent(), InstanceTokens.BRACKETS));
		}
		return sb.toString();
	}

	private PConstraint parseCumulativeConstraint(String name, PVariable[] involvedVariables, Element parameters) throws FormatException {
		StringTokenizer st = new StringTokenizer(buildStringRepresentationOf(parameters));
		if (!nextToken(name, st).equals("["))
			throw new FormatException("One should find [ as first token of the parameters of constraint " + name);
		Set<PVariable> involvedVariablesInParameters = new HashSet<PVariable>();
		String token = nextToken(name, st);
		List<Task> tasks = new ArrayList<Task>();
		while (!token.equals("]")) {
			if (!token.equals("{"))
				throw new FormatException("One should find { as first token of a task definition in constraint " + name);
			token = nextToken(name, st);
			Object origin = mapOfVariables.get(token);
			if (origin == null)
				origin = token.equals(InstanceTokens.NIL) ? null : parseInt("origin field " + token + " in the parameters of constraint " + name, token);
			else
				involvedVariablesInParameters.add((PVariable) origin);
			token = nextToken(name, st);
			Object duration = mapOfVariables.get(token);
			if (duration == null)
				duration = token.equals(InstanceTokens.NIL) ? null : parseInt("duration field " + token + " in the parameters of constraint " + name, token);
			else
				involvedVariablesInParameters.add((PVariable) duration);
			token = nextToken(name, st);
			Object end = mapOfVariables.get(token);
			if (end == null)
				end = token.equals(InstanceTokens.NIL) ? null : parseInt("end field " + token + " in the parameters of constraint " + name, token);
			else
				involvedVariablesInParameters.add((PVariable) end);
			token = nextToken(name, st);
			Object height = mapOfVariables.get(token);
			if (height == null)
				height = parseInt("height field " + token + " in the parameters of constraint " + name, token);
			else
				involvedVariablesInParameters.add((PVariable) height);
			token = nextToken(name, st);
			if (!token.equals("}"))
				throw new FormatException("One should find } as last token of a task definition in constraint " + name);
			if (origin == null && duration == null || origin == null && end == null || duration == null && end == null)
				throw new FormatException("Only one field may be absent in {origin,duration,end} in a task definition of constraint " + name);
			tasks.add(new Task(origin, duration, end, height));
			token = nextToken(name, st);
		}
		controlInvolvedVariablesWrtScope(involvedVariablesInParameters, involvedVariables, name);
		int limit = parseInt("limit value for cumulative constraint " + name, nextToken(name, st));
		if (st.hasMoreTokens())
			throw new FormatException("Too many tokens in the parameters of constraint " + name);
		return new PCumulative(name, involvedVariables, tasks.toArray(new Task[tasks.size()]), limit);
	}

	private PConstraint parseConstraint(Element constraintElement) throws FormatException {
		String name = constraintElement.getAttribute(InstanceTokens.NAME);
		checkAndRecord(name, "constraint");

		int arity = parseStrictlyPositiveInt(InstanceTokens.ARITY + " of constraint " + name + " ", constraintElement.getAttribute(InstanceTokens.ARITY));
		String scope = constraintElement.getAttribute(InstanceTokens.SCOPE);
		if (scope.length() == 0)
			throw new FormatException("There is no scope attribute for constraint " + name);
		String reference = constraintElement.getAttribute(InstanceTokens.REFERENCE);
		if (reference.length() == 0)
			throw new FormatException("There is no reference attribute for constraint " + name);

		StringTokenizer st = new StringTokenizer(scope);
		PVariable[] involvedVariables = new PVariable[st.countTokens()];
		if (arity != involvedVariables.length)
			throw new FormatException("The number of variables in the scope of constraint " + name + " does not correspond to the indicated arity");

		for (int i = 0; i < involvedVariables.length; i++) {
			String variableName = st.nextToken();
			PVariable variable = mapOfVariables.get(variableName);
			if (variable == null)
				throw new FormatException("constraint " + name + " involves the unknown variable " + variableName);
			boolean found = false;
			for (int j = 0; j < i; j++)
				if (involvedVariables[j] == variable)
					found = true;
			if (found)
				throw new FormatException("Constraint " + name + " involves twice the variable " + variableName);
			involvedVariables[i] = variable;
		}

		if (mapOfRelations.containsKey(reference)) {
			if (constraintElement.getElementsByTagName(InstanceTokens.PARAMETERS).item(0) != null)
				throw new FormatException("No parameters expected for the constraint " + name + " defined in extension.");

			if (arity != mapOfRelations.get(reference).getArity())
				throw new FormatException("The arity of constraint " + name + " does not correspond to the arity of the referenced relation.");
			return new PExtensionConstraint(name, involvedVariables, mapOfRelations.get(reference));
		}

		if (mapOfFunctions.containsKey(reference)) {
			Element parameters = (Element) constraintElement.getElementsByTagName(InstanceTokens.PARAMETERS).item(0);
			String[] effectiveParameters = Toolkit.buildTokensFromString(parameters.getTextContent());
			if (effectiveParameters.length != mapOfFunctions.get(reference).getFormalParameters().length)
				throw new FormatException("The number of effective parameters of constraint " + name + " does not correspond to the number of formal parameters of the referenced function.");
			checkEffectiveParameters(name, involvedVariables, effectiveParameters);
			return new PIntensionConstraint(name, involvedVariables, mapOfFunctions.get(reference), parameters.getTextContent());
		}

		if (mapOfPredicates.containsKey(reference)) {
			Element parameters = XMLManager.getElementByTagNameFrom(constraintElement, InstanceTokens.PARAMETERS, 0);
			String[] effectiveParameters = Toolkit.buildTokensFromString(parameters.getTextContent());
			if (effectiveParameters.length != mapOfPredicates.get(reference).getFormalParameters().length)
				throw new FormatException("The number of effective parameters of constraint " + name + " does not correspond to the number of formal parameters of the referenced predicate.");
			checkEffectiveParameters(name, involvedVariables, effectiveParameters);
			return new PIntensionConstraint(name, involvedVariables, mapOfPredicates.get(reference), parameters.getTextContent());
		}

		String lreference = reference.toLowerCase();
		Element parameters = (Element) constraintElement.getElementsByTagName(InstanceTokens.PARAMETERS).item(0);
		if (lreference.equals(InstanceTokens.getLowerCaseGlobalNameOf(InstanceTokens.ALL_DIFFERENT)))
			return new PAllDifferent(name, involvedVariables);
		if (lreference.equals(InstanceTokens.getLowerCaseGlobalNameOf(InstanceTokens.ELEMENT)))
			return parseElementConstraint(name, involvedVariables, parameters);
		if (lreference.equals(InstanceTokens.getLowerCaseGlobalNameOf(InstanceTokens.WEIGHTED_SUM)))
			return parseWeightedSumConstraint(name, involvedVariables, parameters);
		if (lreference.equals(InstanceTokens.getLowerCaseGlobalNameOf(InstanceTokens.CUMULATIVE)))
			return parseCumulativeConstraint(name, involvedVariables, parameters);

		throw new FormatException("There is an unknown reference " + reference + " for constraint " + name);
	}

	private void parseConstraints(Element constraintsElement) throws FormatException {
		if (constraintsElement == null)
			throw new FormatException("The element constraints is absent.");

		mapOfConstraints = new HashMap<String, PConstraint>();
		int nbConstraints = parseStrictlyPositiveInt(InstanceTokens.NB_CONSTRAINTS + " of element constraints ", constraintsElement.getAttribute(InstanceTokens.NB_CONSTRAINTS));
		constraintNames = new String[nbConstraints];

		NodeList nodeList = constraintsElement.getElementsByTagName(InstanceTokens.CONSTRAINT);
		if (nbConstraints != nodeList.getLength())
			throw new FormatException("the value of nbConstraints does not correspond to the number of constraints which is " + nodeList.getLength());
		maxConstraintArity = -1;
		for (int i = 0; i < nodeList.getLength(); i++) {
			PConstraint constraint = parseConstraint((Element) nodeList.item(i));
			mapOfConstraints.put(constraint.getName(), constraint);
			constraintNames[i] = constraint.getName();
			if (constraint.getArity() > maxConstraintArity)
				maxConstraintArity = constraint.getArity();
		}

		if (type.equals(InstanceTokens.WCSP)) {
			String s = constraintsElement.getAttribute(InstanceTokens.MAXIMAL_COST);
			int maximalCost = s.equals(InstanceTokens.INFINITY) ? Integer.MAX_VALUE : parseStrictlyPositiveInt("pb with maximalCost " + s, s);
			s = constraintsElement.getAttribute(InstanceTokens.INITIAL_COST);
			int initialCost = s.equals("") ? 0 : parsePositiveInt("pb with initialCost " + s, s);
			if (initialCost >= maximalCost)
				throw new FormatException("InitialCost is greater than maximalCost");
			if (competitionControl && maximalCost == Integer.MAX_VALUE)
				// if (competitionControl && (initialCost != 0 || maximalCost == Integer.MAX_VALUE))
				throw new FormatException("InitialCost or maximalCost does not respect restrictions of the 2008 competition");

			// for (PRelation relation : mapOfRelations.values()) {
			// int max = relation.getDefaultCost();
			// for (int w : relation.getWeights())
			// if (w > max)
			// max = w;
			// if (max > maximalCost)
			// throw new FormatException("A tuple (or defaultCost) in relation " + relation.getName() + " has a cost strictly greater than maximalCost");
			// }
		}
	}

	private void controlOrderOfElements(Document document) throws FormatException {
		if (!DocumentModifier.areOrderedChilds(document, InstanceTokens.PRESENTATION, InstanceTokens.DOMAINS))
			throw new FormatException("Element <presentation> should be before element <domains>");
		if (!DocumentModifier.areOrderedChilds(document, InstanceTokens.DOMAINS, InstanceTokens.VARIABLES))
			throw new FormatException("Element <domains> should be before element <variables>");
		if (DocumentModifier.isPresentChild(document, InstanceTokens.RELATIONS) && !DocumentModifier.areOrderedChilds(document, InstanceTokens.VARIABLES, InstanceTokens.RELATIONS))
			throw new FormatException("Element <variables> should be before element <relations>");
		if (DocumentModifier.isPresentChild(document, InstanceTokens.PREDICATES) && !DocumentModifier.areOrderedChilds(document, InstanceTokens.VARIABLES, InstanceTokens.PREDICATES))
			throw new FormatException("Element <variables> should be before element <predicates>");
		if (DocumentModifier.isPresentChild(document, InstanceTokens.RELATIONS) && !DocumentModifier.areOrderedChilds(document, InstanceTokens.RELATIONS, InstanceTokens.CONSTRAINTS))
			throw new FormatException("Element <relations> should be before element <constraints>");
		if (DocumentModifier.isPresentChild(document, InstanceTokens.PREDICATES) && !DocumentModifier.areOrderedChilds(document, InstanceTokens.PREDICATES, InstanceTokens.CONSTRAINTS))
			throw new FormatException("Element <predicates> should be before element <constraints>");
	}

	public InstanceCheckerParser(InstanceCheckerEngine logic, Document document, boolean competitionControl) throws FormatException {
		this.engine = logic;
		this.competitionControl = competitionControl;
		controlOrderOfElements(document);

		allNameIdentifiers = new HashSet<String>();
		parsePresentation(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.PRESENTATION));
		logic.spot();
		parseDomains(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.DOMAINS));
		logic.spot();
		parseVariables(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.VARIABLES));
		logic.spot();
		parseRelations(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.RELATIONS));
		logic.spot();
		parseFunctions(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.FUNCTIONS));
		logic.spot();
		parsePredicates(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.PREDICATES));
		logic.spot();
		parseConstraints(XMLManager.getFirstElementByTagNameFromRoot(document, InstanceTokens.CONSTRAINTS));
	}

	private boolean isDomainReferenced(PDomain domain) {
		for (PVariable variable : mapOfVariables.values())
			if (variable.getDomain() == domain)
				return true;
		return false;
	}

	private void checkValidityOfDomains() throws FormatException {
		for (PDomain domain : mapOfDomains.values())
			if (!isDomainReferenced(domain))
				throw new FormatException("the domain " + domain.getName() + " is never referenced by a variable");
		if (competitionControl)
			for (int i = 0; i < domainNames.length; i++)
				if (!domainNames[i].equals(InstanceTokens.getDomainNameFor(i)))
					throw new FormatException("the " + (i + 1) + "th domain is not called " + InstanceTokens.getDomainNameFor(i));
	}

	private void checkValidityOfVariables() throws FormatException {
		if (competitionControl)
			for (int i = 0; i < variableNames.length; i++)
				if (!variableNames[i].equals(InstanceTokens.getVariableNameFor(i)))
					throw new FormatException("the " + (i + 1) + "th variable is not called " + InstanceTokens.getVariableNameFor(i));
	}

	private boolean isRelationReferenced(PRelation relation) {
		for (PConstraint constraint : mapOfConstraints.values())
			if (constraint instanceof PExtensionConstraint && ((PExtensionConstraint) constraint).getRelation() == relation)
				return true;
		return false;
	}

	private void checkValidityOfRelations() throws FormatException {
		for (PRelation relation : mapOfRelations.values())
			if (!isRelationReferenced(relation))
				throw new FormatException("the relation " + relation.getName() + " is never referenced by a constraint");
		if (competitionControl)
			for (int i = 0; i < relationNames.length; i++)
				if (!relationNames[i].equals(InstanceTokens.getRelationNameFor(i)))
					throw new FormatException("the " + (i + 1) + "th relation is not called " + InstanceTokens.getRelationNameFor(i));
	}

	private boolean isPredicateReferenced(PPredicate predicate) {
		for (PConstraint constraint : mapOfConstraints.values())
			if (constraint instanceof PIntensionConstraint && ((PIntensionConstraint) constraint).getFunction() == predicate)
				return true;
		return false;
	}

	private void checkValidityOfPredicates() throws FormatException {
		for (PPredicate predicate : mapOfPredicates.values())
			if (!isPredicateReferenced(predicate))
				throw new FormatException("the predicate " + predicate.getName() + " is never referenced by a constraint");

		if (competitionControl) {
			for (int i = 0; i < predicateNames.length; i++)
				if (!predicateNames[i].equals(InstanceTokens.getPredicateNameFor(i)))
					throw new FormatException("the " + (i + 1) + "th predicate is not called " + InstanceTokens.getPredicateNameFor(i));
			for (PPredicate predicate : mapOfPredicates.values()) {
				String[] formalParameters = predicate.getFormalParameters();
				for (int i = 0; i < formalParameters.length; i++)
					if (!formalParameters[i].equals(InstanceTokens.getParameterNameFor(i)))
						throw new FormatException("the " + (i + 1) + "th formal parameter of " + predicate.getName() + " is not called " + InstanceTokens.getParameterNameFor(i));
			}
		}
	}

	private void checkValidityOfConstraints() throws FormatException {
		for (PConstraint constraint : mapOfConstraints.values()) {
			if (!(constraint instanceof PExtensionConstraint))
				continue;
			PVariable[] scope = constraint.getScope();
			PRelation relation = ((PExtensionConstraint) constraint).getRelation();
			int[][] tuples = relation.getTuples();
			for (int[] tuple : tuples)
				for (int i = 0; i < tuple.length; i++)
					if (!scope[i].getDomain().contains(tuple[i]))
						throw new FormatException("The value " + tuple[i] + " that belongs to a tuple of the referenced relation does not belong to the domain of the variable " + scope[i].getName()
								+ " involved in constraint " + constraint.getName());
		}

		if (competitionControl) {
			for (int i = 0; i < constraintNames.length; i++)
				if (!constraintNames[i].equals(InstanceTokens.getConstraintNameFor(i)))
					throw new FormatException("the " + (i + 1) + "th constraint is not called " + InstanceTokens.getConstraintNameFor(i));

			// for (int i = 0; i < cons.length - 1; i++)
			// for (int j = i + 1; j < cons.length; j++)
			// if (cons[i].hasSimilarSetOfVariables(cons[j].getVariables()))
			// throw new FormatException("contraints " + cons[i].getName() + " and " + cons[j].getName() + " involves the same set of variables");

			long sumL = 0;
			double sumD = 0;
			for (PConstraint constraint : mapOfConstraints.values()) {
				if (constraint.getArity() < 1)
					throw new FormatException("contraint " + constraint.getName() + " has an arity less than 1");
				if (!constraint.isGuaranteedToBeDivisionByZeroFree())
					throw new FormatException("the constraint " + constraint.getName() + " is not guaranteed (by our rough analysis) to be division by zero free");
				if (!constraint.isGuaranteedToBeOverflowFree())
					throw new FormatException("the constraint " + constraint.getName() + " is not guaranteed (by our rough analysis) to be overflow free");
				sumL += constraint.getMaximalCost();
				sumD += constraint.getMaximalCost();
			}
			// System.out.println(sumL);
			if (sumL != sumD || Double.isInfinite(sumD))
				throw new FormatException("the instance may involve an overflow computation when considering the maximal cost of an instantiation");
		}
	}

	public void checkValidity() throws FormatException {
		checkValidityOfDomains();
		engine.spot();
		checkValidityOfVariables();
		engine.spot();
		checkValidityOfRelations();
		engine.spot();
		checkValidityOfPredicates();
		engine.spot();
		checkValidityOfConstraints();
	}

	private String semantics;

	private int[][] buildTuplesOf(PConstraint constraint, String[] canonicalPredicate) {
		int[][] values = new int[constraint.getArity()][];
		for (int i = 0; i < values.length; i++)
			values[i] = constraint.getScope()[i].getDomain().getValues();

		List<int[]> supports = new LinkedList<int[]>();
		List<int[]> conflicts = new LinkedList<int[]>();

		EvaluationManager evaluationManager = new EvaluationManager(canonicalPredicate);

		LexicographicIterator lexicographicIterator = new LexicographicIterator(values);
		int[] tuple = lexicographicIterator.getFirstTuple();
		while (tuple != null) {
			if (evaluationManager.evaluate(tuple) == 1)
				supports.add(tuple.clone());
			else
				conflicts.add(tuple.clone());
			tuple = lexicographicIterator.getNextTupleAfter(tuple);
		}

		if (supports.size() <= conflicts.size()) {
			semantics = InstanceTokens.SUPPORTS;
			return supports.toArray(new int[0][0]);
		}
		semantics = InstanceTokens.CONFLICTS;
		return conflicts.toArray(new int[0][0]);
	}

	private PRelation getSimilarRelation(int arity, int nbTuples, String semantics, int[][] tuples) {
		for (PRelation relation : mapOfRelations.values())
			if (!(relation instanceof PSoftRelation) && relation.isSimilarTo(arity, nbTuples, semantics, tuples))
				return relation;
		for (PRelation relation : newRelations)
			if (!(relation instanceof PSoftRelation) && relation.isSimilarTo(arity, nbTuples, semantics, tuples))
				return relation;
		return null;
	}

	private int defaultCost;

	private int[][] buildSoftTuplesOf(PConstraint constraint, String[] canonicalPredicate) throws FormatException {
		int[][] values = new int[constraint.getArity()][];
		double size = 1;
		for (int i = 0; i < values.length; i++) {
			values[i] = constraint.getScope()[i].getDomain().getValues();
			size *= values[i].length;
		}
		if (size > 100000)
			throw new FormatException("the constraint " + constraint.getName() + " is too difficult to translate (more than 100000 tuples to consider)");

		int[][] tuples = new int[(int) size][];
		weights = new int[(int) size];
		Map<Integer, Integer> map = new HashMap<Integer, Integer>();

		EvaluationManager evaluationManager = new EvaluationManager(canonicalPredicate);

		LexicographicIterator lexicographicIterator = new LexicographicIterator(values);
		int[] tuple = lexicographicIterator.getFirstTuple();
		int cnt = 0;
		defaultCost = -1;
		int defaultCostCnt = -1;
		while (tuple != null) {
			int weight = (int) (evaluationManager.evaluate(tuple));
			tuples[cnt] = tuple.clone();
			weights[cnt] = weight;
			Integer i = map.get(weight);
			if (i == null)
				i = 0;
			map.put(weight, i + 1);
			if (i + 1 > defaultCostCnt) {
				defaultCost = weight;
				defaultCostCnt = i + 1;
			}
			tuple = lexicographicIterator.getNextTupleAfter(tuple);
			cnt++;
		}
		int[][] t = new int[(int) size - defaultCostCnt][];
		int[] w = new int[(int) size - defaultCostCnt];
		cnt = 0;
		for (int i = 0; i < tuples.length; i++) {
			if (weights[i] == defaultCost)
				continue;
			t[cnt] = tuples[i];
			w[cnt] = weights[i];
			cnt++;
		}
		weights = w;
		return t;
	}

	private PRelation getSimilarSoftRelation(int arity, int nbTuples, String semantics, int[][] tuples, int[] weights, int defaultCost) {
		for (PRelation relation : mapOfRelations.values())
			if (relation instanceof PSoftRelation && ((PSoftRelation) relation).isSimilarTo(arity, nbTuples, semantics, tuples, weights, defaultCost))
				return relation;
		for (PRelation relation : newRelations)
			if (relation instanceof PSoftRelation && ((PSoftRelation) relation).isSimilarTo(arity, nbTuples, semantics, tuples, weights, defaultCost))
				return relation;
		return null;
	}

	private int getRelationNameFrom(int limit) {
		while (mapOfRelations.containsKey(InstanceTokens.getRelationNameFor(limit)))
			limit++;
		return limit;
	}

	private int convert(PIntensionConstraint constraint, int limit) throws FormatException {
		int arity = constraint.getArity();
		if (!(constraint.getFunction() instanceof PPredicate)) {
			int[][] tuples = buildSoftTuplesOf(constraint, constraint.getUniversalPostfixExpression());
			String semantics = InstanceTokens.SOFT;

			PRelation relation = getSimilarSoftRelation(arity, tuples.length, semantics, tuples, weights, defaultCost);
			if (relation == null) {
				limit = getRelationNameFrom(limit);
				relation = new PSoftRelation(InstanceTokens.getRelationNameFor(limit), arity, tuples.length, semantics, tuples, weights, defaultCost);
				newRelations.add(relation);
				limit++;
			}
			constraintsToNewRelations.put(constraint.getName(), relation.getName());
			PExtensionConstraint c = new PExtensionConstraint(constraint.getName(), constraint.getScope(), relation);
			mapOfConstraints.put(c.getName(), c);
			// constraint.setRelation(relation);
			return limit;
		}

		int[][] tuples = buildTuplesOf(constraint, constraint.getUniversalPostfixExpression());
		PRelation relation = getSimilarRelation(arity, tuples.length, semantics, tuples);
		if (relation == null) {
			limit = getRelationNameFrom(limit);
			relation = new PRelation(InstanceTokens.getRelationNameFor(limit), arity, tuples.length, semantics, tuples);
			newRelations.add(relation);
			limit++;
		}
		constraintsToNewRelations.put(constraint.getName(), relation.getName());
		PExtensionConstraint c = new PExtensionConstraint(constraint.getName(), constraint.getScope(), relation);
		mapOfConstraints.put(c.getName(), c);
		// constraint.setRelation(relation);
		return limit;
	}

	public void convertToExtension() throws FormatException {
		if (mapOfFunctions.size() == 0 && mapOfPredicates.size() == 0)
			return;

		newRelations = new ArrayList<PRelation>();
		constraintsToNewRelations = new HashMap<String, String>();

		int nbConvertions = 0;
		for (PConstraint constraint : mapOfConstraints.values())
			if (constraint instanceof PIntensionConstraint)
				nbConvertions++;

		int cpt = 0;
		int spotLimit = 1 + (nbConvertions / 20);
		int limit = 0;
		for (PConstraint constraint : mapOfConstraints.values()) {
			if (!(constraint instanceof PIntensionConstraint))
				continue;
			cpt++;
			limit = convert((PIntensionConstraint) constraint, limit);
			if (cpt % spotLimit == 0)
				engine.spot();
		}
	}

	public void updateStructures() {
		mapOfFunctions.clear();
		functionNames = new String[0];
		mapOfPredicates.clear();
		predicateNames = new String[0];

		int nbOldRelations = mapOfRelations.size();
		String[] t = new String[nbOldRelations + newRelations.size()];

		for (int i = 0; i < nbOldRelations; i++)
			t[i] = relationNames[i];
		Iterator<PRelation> it = newRelations.iterator();
		for (int i = nbOldRelations; i < t.length; i++) {
			PRelation relation = it.next();
			t[i] = relation.getName();
			mapOfRelations.put(relation.getName(), relation);
		}
		relationNames = t;
	}
}
