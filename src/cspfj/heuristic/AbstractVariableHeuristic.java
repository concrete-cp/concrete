package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Variable;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

	private final static Random random = new Random();
	
	public Variable selectVariable(final Variable[] variables) {
		Variable bestVariable = null;

		for (Variable v : variables) {
			if (v.getDomainSize() > 1
					&& (bestVariable == null || compare(v, bestVariable) > 0)) {
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
		// if (variable1 == null) {
		// return 1;
		// }
		// if (variable2 == null) {
		// return -1;
		// }
		return Double.compare(getScore(variable1), getScore(variable2));
	}
	
	protected static double poisson(final double moy) {
		return -moy * Math.log(random.nextDouble());
	}

}
