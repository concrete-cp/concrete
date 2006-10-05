package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractValueHeuristic implements ValueHeuristic {

	final protected Variable[] variables;

	public AbstractValueHeuristic(Problem problem) {
		variables = problem.getVariables();

		for (Variable variable : variables) {
			init(variable);
		}
	}

	/**
	 * @return Le prochain index à assigner
	 */
	public int selectIndex(final Variable variable) {
//		assert domainSize > 0;
//
//		final int[] order = this.order;
//
//		for (int i = 0; i < order.length; i++) {
//			if (isPresent(order[i])) {
//				return order[i];
//			}
//		}
		return -1;
	}

}
