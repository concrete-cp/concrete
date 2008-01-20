package cspfj.heuristic;

import cspfj.problem.Variable;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

	public Variable selectVariable(final Variable[] variables) {
		Variable bestVariable = null;

		for (Variable v : variables) {
			if (v.getDomainSize() > 1
					&& (bestVariable == null || compare(v, bestVariable) < 0)) {
				bestVariable = v;
			}
		}

		if (bestVariable == null) {
			for (Variable v : variables) {
				if (!v.isAssigned()) {
					return v;
				}
			}
		}

		return bestVariable;
	}

	public final int compare(final Variable variable1, final Variable variable2) {
//		if (variable1 == null) {
//			return 1;
//		}
//		if (variable2 == null) {
//			return -1;
//		}
		final double result = getScore(variable2) - getScore(variable1);
		if (result == 0) {
			return 0 ;//variable1.getId() - variable2.getId();
		}

		return result > 0 ? 1 : -1;
	}

}
