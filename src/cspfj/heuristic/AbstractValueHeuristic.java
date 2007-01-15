package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractValueHeuristic implements ValueHeuristic {

	final protected Problem problem;

	public AbstractValueHeuristic(Problem problem) {
		this.problem = problem;
	}

	/**
	 * @return Le prochain index à assigner
	 */
	public int selectIndex(final Variable variable) {
		int bestValue = -1;

		for (int i = variable.getFirst(); i >= 0; i = variable
				.getNext(i)) {
			if (bestValue < 0 || compare(variable, i, bestValue) > 0) {
				bestValue = i;
			}
		}

		return bestValue;
	}

	public final int compare(final Variable variable, final int index1,
			final int index2) {
		return getScore(variable, index1) > getScore(variable, index2) ? 1 : -1;
	}
}
