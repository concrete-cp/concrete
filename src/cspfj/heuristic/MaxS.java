package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class MaxS extends AbstractStaticValueHeuristic {

	public MaxS(Problem problem) {
		super(problem);
	}

	public float getScore(final Variable variable, final int index) {
		return variable.getNbSupports(index);
	}

}
