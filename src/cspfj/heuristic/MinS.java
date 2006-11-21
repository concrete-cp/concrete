package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class MinS extends AbstractStaticValueHeuristic {

	public MinS(Problem problem) {
		super(problem);
	}

	public double getScore(final Variable variable, final int index) {
		return -variable.getNbSupports(index);
	}

}
