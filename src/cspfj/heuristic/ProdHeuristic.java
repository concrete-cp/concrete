package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class ProdHeuristic implements Heuristic {

	private final Variable[] variables;

	private final VariableHeuristic variableHeuristic;

	private final ValueHeuristic valueHeuristic;

	public ProdHeuristic(Problem problem, VariableHeuristic variableHeuristic,
			ValueHeuristic valueHeuristic) {
		this.variableHeuristic = variableHeuristic;
		this.valueHeuristic = valueHeuristic;
		this.variables = problem.getVariables();
	}

	public long selectPair() {
		Variable bestVariable = null;
		for (Variable v : variables) {
			if (!v.isAssigned()
					&& (bestVariable == null || (v.getDomainSize() != 1 && compare(
							v, bestVariable) > 0))) {
				bestVariable = v;
			}
		}

		return Pair.pair(bestVariable, valueHeuristic.selectIndex(bestVariable));
	}

	public int compare(final Variable variable1, final Variable variable2) {

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
}