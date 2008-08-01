package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class WValue extends AbstractValueHeuristic {

	public WValue(final Problem problem, final boolean ff) {
		super(problem);

		for (Variable v : problem.getVariables()) {
			v.attachWeights(new int[v.getDomain().length]);
		}
	}

	public void compute() {
		// TODO Auto-generated method stub

	}

	public double getScore(final Variable variable, final int index) {
		return variable.getWeight(index);
	}
}
