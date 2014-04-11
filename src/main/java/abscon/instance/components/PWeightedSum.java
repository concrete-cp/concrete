package abscon.instance.components;

import abscon.instance.PredicateTokens.RelationalOperator;

public class PWeightedSum extends PGlobalConstraint {
	private int[] coeffs;

	private RelationalOperator operator;

	private int limit;

	public int[] getCoeffs() {
		return coeffs;
	}

	public RelationalOperator getOperator() {
		return operator;
	}

	public PWeightedSum(String name, PVariable[] scope, int[] coeffs, RelationalOperator operator, int limit) {
		super(name, scope);
		this.coeffs = coeffs;
		this.operator = operator;
		this.limit = limit;
	}

	public long computeCostOf(int[] tuple) {
		int sum = 0;
		for (int i = 0; i < coeffs.length; i++)
			sum += coeffs[i] * tuple[i];
		boolean satisfied = operator == RelationalOperator.EQ ? sum == limit : operator == RelationalOperator.NE ? sum != limit : operator == RelationalOperator.GE ? sum >= limit : operator == RelationalOperator.GT ? sum > limit
				: operator == RelationalOperator.LE ? sum <= limit : sum < limit;
		return satisfied ? 0 : 1;
	}

	public String toString() {
		String s = super.toString() + " : weightedSum\n\t";
		for (int i = 0; i < coeffs.length; i++)
			s += coeffs[i] + "*" + scope[i].getName() + " ";
		s += RelationalOperator.getStringFor(operator) + " " + limit;
		return s;
	}

	public boolean isGuaranteedToBeOverflowFree() {
		int sumL = 0;
		double sumD = 0;

		for (int i = 0; i < scope.length; i++) {
			int[] values = scope[i].getDomain().getValues();
			int maxAbsoluteValue = Math.max(Math.abs(values[0]), Math.abs(values[values.length - 1]));
			sumL+=Math.abs(coeffs[i])*maxAbsoluteValue;
			sumD+=Math.abs(coeffs[i])*maxAbsoluteValue;
		}
		if (sumL != sumD || Double.isInfinite(sumD))
			return false;
		return true;
	}
}
