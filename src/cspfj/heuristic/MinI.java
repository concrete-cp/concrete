package cspfj.heuristic;

import cspfj.problem.Problem;

public final class MinI extends JW {

	public MinI(Problem problem) {
		super(problem);
	}

	@Override
	protected double w(final int supports) {
		return -supports;
	}

}
