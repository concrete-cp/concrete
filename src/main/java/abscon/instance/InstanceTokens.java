package abscon.instance;

public class InstanceTokens {
	public static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

	public static final String INSTANCE_SCHEMA_1_1 = "instance_1_1.xsd";

	public static final String INSTANCE_SCHEMA_2_0 = "instance_2_0.xsd";

	public static final String INSTANCE_STYLESHEET_1_1 = "instance_1_1.xsl";

	public static final String INSTANCE_STYLESHEET_2_0 = "instance_2_0.xsl";

	public static final String INSTANCE_STYLESHEET_2_1 = "instance_2_1.xsl";

	public static final String SAT = "SAT";

	public static final String UNSAT = "UNSAT";

	public static final String SOL = "SOL";

	public static final String INSTANCE = "instance";

	public static final String WHITE_SPACE = " \t\n\r\f";

	public static final String BRACKETS = "[]{}";

	/**
	 * Used as an attribute to identify domains, variables, relations, predicates and constraints.
	 */
	public static final String NAME = "name";

	// description (presentation)

	public static final String DESCRIPTION = "description";

	public static final String PRESENTATION = "presentation";

	public static final String MAX_CONSTRAINT_ARITY = "maxConstraintArity";

	public static final String MIN_VIOLATED_CONSTRAINTS = "minViolatedConstraints";

	public static final String SOLUTION = "solution";

	public static final String NB_SOLUTIONS = "nbSolutions";

	public static final String FORMAT = "format";

	public static final String XCSP = "XCSP ";

	public static final String XCSP_2_0 = "XCSP 2.0";

	public static final String XCSP_2_1 = "XCSP 2.1";

	public static final String TYPE = "type";

	public static final String CSP = "CSP";

	public static final String WCSP = "WCSP";

	public static final String QCSP = "QCSP";

	public static final String QCSP_PLUS = "QCSP+";

	// domains

	public static final String DOMAINS = "domains";

	public static final String NB_DOMAINS = "nbDomains";

	public static final String DOMAIN = "domain";

	public static final String NB_VALUES = "nbValues";

	public static final String VALUES = "values";

	public final static String VALUE_SEPARATOR = " ";

	public final static String DISCRETE_INTERVAL_START = "";

	public final static String DISCRETE_INTERVAL_SEPARATOR = "..";

	public final static String DISCRETE_INTERVAL_END = "";

	// variables

	public static final String VARIABLES = "variables";

	public static final String NB_VARIABLES = "nbVariables";

	public static final String VARIABLE = "variable";

	// relations

	public static final String RELATIONS = "relations";

	public static final String NB_RELATIONS = "nbRelations";

	public static final String RELATION = "relation";

	public static final String SUPPORTS = "supports";

	public static final String NB_SUPPORTS = "nbSupports";

	public static final String CONFLICTS = "conflicts";

	public static final String TUPLES_SEPARATOR = "|";

	public static final String SOFT = "soft";

	public static final String INFINITY = "infinity";

	public static final String DEFAULT_COST = "defaultCost";

	public static final String COST_SEPARATOR = ":";

	public static final String NB_CONFLICTS = "nbConflicts";

	public static final String ARITY = "arity";

	public static final String NB_TUPLES = "nbTuples";

	public static final String SEMANTICS = "semantics";

	// functions

	public static final String FUNCTIONS = "functions";

	public static final String NB_FUNCTIONS = "nbFunctions";

	public static final String FUNCTION = "function";

	// predicates

	public static final String PREDICATES = "predicates";

	public static final String NB_PREDICATES = "nbPredicates";

	public static final String PREDICATE = "predicate";

	public static final String EXPRESSION = "expression";

	public static final String FUNCTIONAL = "functional";

	public static final String POSTFIX_EXPRESSION = "postfixExpression";

	public static final String PARAMETERS = "parameters";

	public static final String LIST = "list";

	// global constraints

	public static final String PREFIX_GLOBAL = "global:";

	public static final String ALL_DIFFERENT = "allDifferent";

	public static final String WEIGHTED_SUM = "weightedSum";

	public static final String CUMULATIVE = "cumulative";

	public static final String ELEMENT = "element";

	public static final String NIL = "nil";

	public static String getGlobalNameOf(String s) {
		return PREFIX_GLOBAL + s;
	}

	public static String getLowerCaseGlobalNameOf(String s) {
		return PREFIX_GLOBAL + s.toLowerCase();
	}

	// constraints

	public static final String CONSTRAINTS = "constraints";

	public static final String NB_CONSTRAINTS = "nbConstraints";

	public static final String INITIAL_COST = "initialCost";

	public static final String MAXIMAL_COST = "maximalCost";

	public static final String CONSTRAINT = "constraint";

	public static final String SCOPE = "scope";

	public static final String REFERENCE = "reference";

	public static final String CST = "cst";

	public static final String VAR = "var";

	public static final String VALUE = "value";

	// names of elements

	public static final String DOMAIN_PREFIX = "D";

	public static final String VARIABLE_PREFIX = "V";

	public static final String RELATION_PREFIX = "R";

	public static final String FUNCTION_PREFIX = "F";

	public static final String PREDICATE_PREFIX = "P";

	public static final String CONSTRAINT_PREFIX = "C";

	public static final String PARAMETER_PREFIX = "X";

	public static String getDomainNameFor(int id) {
		return DOMAIN_PREFIX + id;
	}

	public static String getVariableNameFor(int id) {
		return VARIABLE_PREFIX + id;
	}

	public static String getRelationNameFor(int id) {
		return RELATION_PREFIX + id;
	}

	public static String getFunctionNameFor(int id) {
		return FUNCTION_PREFIX + id;
	}
	
	public static String getPredicateNameFor(int id) {
		return PREDICATE_PREFIX + id;
	}

	public static String getConstraintNameFor(int id) {
		return CONSTRAINT_PREFIX + id;
	}

	public static String getParameterNameFor(int id) {
		return PARAMETER_PREFIX + id;
	}

	public static boolean isCanonicalName(String prefix, String name) {
		return name.startsWith(prefix) && Toolkit.isInteger(name.substring(prefix.length()));
	}
}
