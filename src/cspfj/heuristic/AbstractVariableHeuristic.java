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
			if (v.getDomainSize() > 1
					&& (bestVariable == null || compare(
							v, bestVariable) < 0)) {
				bestVariable = v;
			}
		}

		if (bestVariable == null) {
			for (Variable v: variables) {
				if (!v.isAssigned()) {
					return v;
				}
			}
		}
		
		return bestVariable;
	}
    
    public final int compare(final Variable variable1, final Variable variable2) {
        final float result = getScore(variable2) - getScore(variable1);
        if (result == 0) {
            return variable1.getId() - variable2.getId();
        }

        return result > 0 ? 1 : -1;
    }
}
