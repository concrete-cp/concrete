package abscon.instance;

public class PredicateTokens {
	public final static String ADD = "add";

	public final static String SUB = "sub";

	public final static String MUL = "mul";

	public final static String DIV = "div";

	public final static String MOD = "mod";

	public final static String POW = "pow";

	public final static String NEG = "neg";

	public final static String ABS = "abs";

	public final static String MIN = "min";

	public final static String MAX = "max";

	public final static String IF = "if";

	public final static String TRUE = "true";

	public final static String FALSE = "false";

	public final static String NOT = "not";

	public final static String AND = "and";

	public final static String OR = "or";

	public final static String XOR = "xor";

	public final static String IFF = "iff";

	public final static String EQ = "eq";

	public final static String NE = "ne";

	public final static String GT = "gt";

	public final static String GE = "ge";

	public final static String LT = "lt";

	public final static String LE = "le";

	public final static String ID = "id";

	public final static String VAR = "var";

	public final static String CST = "cst";


	public enum RelationalOperator {
		EQ, NE, GE, GT, LE, LT;

		public static String getStringFor(RelationalOperator operator) {
			if (operator == EQ)
				return PredicateTokens.EQ;
			if (operator == NE)
				return PredicateTokens.NE;
			if (operator == GE)
				return PredicateTokens.GE;
			if (operator == GT)
				return PredicateTokens.GT;
			if (operator == LE)
				return PredicateTokens.LE;
			if (operator == LT)
				return PredicateTokens.LT;
			return null;
		}	
		
		public static RelationalOperator getRelationalOperatorFor(String string) {
			if (string.equals(PredicateTokens.EQ))
				return EQ;
			if (string.equals(PredicateTokens.NE))
				return NE;
			if (string.equals(PredicateTokens.GE))
				return GE;
			if (string.equals(PredicateTokens.GT))
				return GT;
			if (string.equals(PredicateTokens.LE))
				return LE;
			if (string.equals(PredicateTokens.LT))
				return LT;
			return null;
		}
	}
	
	
	
	private static String function(String functionName, Object operand) {
		return operand + " " + functionName + " ";
	}

	private static String function(String functionName, Object operand1, Object operand2) {
		return operand2 + " " + operand1 + " " + functionName + " ";
	}

	public static String function(String functionName, Object operand1, Object operand2, Object operand3) {
		return operand3 + " " + operand2 + " " + operand1 + " " + functionName + " ";
	}

	public static String function(String functionName, Object[] operands) {
		String s = "";
		for (int i = operands.length - 1; i >= 0; i--)
			s = s + operands[i] + " ";
		return s + functionName + " ";
	}

	public static String eq(Object operand1, Object operand2) {
		return function(EQ, operand1, operand2);
	}

	public static String eq(Object operand1, Object operand2, Object operand3) {
		return function(EQ, operand1, operand2, operand3);
	}

	public static String eq(Object[] operands) {
		return function(EQ, operands);
	}

	public static String ne(Object operand1, Object operand2) {
		return function(NE, operand1, operand2);
	}

	public static String gt(Object operand1, Object operand2) {
		return function(GT, operand1, operand2);
	}

	public static String ge(Object operand1, Object operand2) {
		return function(GE, operand1, operand2);
	}

	public static String lt(Object operand1, Object operand2) {
		return function(LT, operand1, operand2);
	}

	public static String le(Object operand1, Object operand2) {
		return function(LE, operand1, operand2);
	}

	public static String not(Object operand1) {
		return function(NOT, operand1);
	}

	public static String and(Object operand1, Object operand2) {
		return function(AND, operand1, operand2);
	}

	public static String and(Object operand1, Object operand2, Object operand3) {
		return function(AND, operand1, operand2, operand3);
	}

	public static String or(Object operand1, Object operand2) {
		return function(OR, operand1, operand2);
	}

	public static String or(Object operand1, Object operand2, Object operand3) {
		return function(OR, operand1, operand2, operand3);
	}

	public static String functionIf(Object operand1, Object operand2, Object operand3) {
		return function(IF, operand1, operand2, operand3);
	}

	public static String abs(Object operand) {
		return function(ABS, operand);
	}

	public static String div(Object operand1, Object operand2) {
		return function(DIV, operand1, operand2);
	}

	public static String mul(Object operand1, Object operand2) {
		return function(MUL, operand1, operand2);
	}

	public static String mod(Object operand1, Object operand2) {
		return function(MOD, operand1, operand2);
	}

	public static String sub(Object operand1, Object operand2) {
		return function(SUB, operand1, operand2);
	}

	public static String add(Object operand1) {
		return function(ADD, operand1);
	}

	public static String add(Object operand1, Object operand2) {
		return function(ADD, operand1, operand2);
	}

	public static String pow(Object operand1, Object operand2) {
		return function(POW, operand1, operand2);
	}

	public static String id(Object operand) {
		return function(ID, operand);
	}
}