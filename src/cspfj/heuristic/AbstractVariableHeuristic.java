package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

	final protected Variable[] variables;

	public AbstractVariableHeuristic(Problem problem) {
		variables = problem.getVariables();
	}

	public Variable selectVariable() {
		Variable bestVariable = null;

		for (Variable v : variables) {
			if (!v.isAssigned()
					&& (bestVariable == null || (v.getDomainSize() != 1 && compare(
							v, bestVariable) < 0))) {
				bestVariable = v;
			}
		}

		return bestVariable;
	}
}
