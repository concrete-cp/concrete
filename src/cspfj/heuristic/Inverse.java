package cspfj.heuristic;

import cspfj.problem.Problem;

public final class Inverse extends JW {

	public Inverse(Problem problem, boolean failFirst) {
		super(problem, failFirst);
	}

	@Override
	protected double w(final int supports) {
		return -supports;
	}

}
