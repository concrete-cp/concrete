package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.IntVariable;

public class ProdHeuristic implements Heuristic {

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	public ProdHeuristic(VariableHeuristic variableHeuristic,
			ValueHeuristic valueHeuristic) {
		this.variableHeuristic = variableHeuristic;
		this.valueHeuristic = valueHeuristic;
	}

	public Pair selectPair(Problem problem) {
		IntVariable bestVariable = null;
		for (IntVariable v : problem.getVariables()) {
			if (!v.isAssigned()
					&& (bestVariable == null || (v.getDomainSize() != 1 && compare(
							v, bestVariable) > 0))) {
				bestVariable = v;
			}
		}

		return new Pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
	}

	public int compare(final IntVariable variable1, final IntVariable variable2) {

		final VariableHeuristic variableHeuristic = this.variableHeuristic;
		final ValueHeuristic valueHeuristic = this.valueHeuristic;

		final double result = variableHeuristic.getScore(variable1)
				* valueHeuristic.getScore(variable1, valueHeuristic
						.selectIndex(variable1))
				- variableHeuristic.getScore(variable2)
				* valueHeuristic.getScore(variable2, valueHeuristic
						.selectIndex(variable2));

		// final double result = wdegOnDom.getScore(variable1)
		// - wdegOnDom.getScore(variable2)
		// ;

		if (result == 0) {
			return variable1.getId() - variable2.getId();
		}

		return result > 0 ? 1 : -1;
	}

	public void compute() {
		valueHeuristic.compute();
	}

	public String toString() {
		return variableHeuristic + " * " + valueHeuristic;
	}

	@Override
	public VariableHeuristic getVariableHeuristic() {
		return variableHeuristic;
	}

	@Override
	public ValueHeuristic getValueHeuristic() {
		return valueHeuristic;
	}
}
