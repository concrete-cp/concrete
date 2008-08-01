package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractValueHeuristic implements ValueHeuristic {

	final protected Problem problem;

	public AbstractValueHeuristic(final Problem problem) {
		this.problem = problem;
	}

	/**
	 * @return Le prochain index à assigner
	 */
	public int selectIndex(final Variable variable) {

		int bestValue = variable.getFirst();
		if (bestValue < 0) {
			return -1;
		}

		double bestScore = getScore(variable, bestValue);

		for (int i = variable.getNext(bestValue); i >= 0; i = variable
				.getNext(i)) {
			final double score = getScore(variable, i);
			if (Double.compare(getScore(variable, i), bestScore) > 0) {
				bestValue = i;
				bestScore = score;
			}
		}

		return bestValue;
	}
}
