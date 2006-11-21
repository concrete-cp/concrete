package cspfj.heuristic;

import cspfj.problem.Problem;

public final class MaxI extends JW {

	public MaxI(Problem problem) {
		super(problem);
	}

	@Override
	protected double w(final int supports) {
		return supports;
	}

}
