package cspfj.heuristic;

import java.util.Random;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractVariableHeuristic implements VariableHeuristic {

	private final static Random random = new Random(0);

	protected final Problem problem;

	public AbstractVariableHeuristic(final Problem problem) {
		this.problem = problem;
	}

	public Variable selectVariable(final Variable[] variables) {
		int ties = 1;
		Variable bestVariable = null;
		int i = variables.length;
		while (--i >= 0) {
			if (!variables[i].isAssigned()) {
				bestVariable = variables[i];
				break;
			}
		}

		while (--i >= 0) {
			if (variables[i].getDomainSize() > 1) {
				final int comp = compare(variables[i], bestVariable);
				if (comp > 0 || (comp == 0 && ++ties * random.nextFloat() > 1)) {
					bestVariable = variables[i];
					ties = 1;
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
