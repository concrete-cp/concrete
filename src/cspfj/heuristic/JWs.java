package cspfj.heuristic;

import cspfj.problem.Problem;

public final class JWs extends JW {

	public JWs(Problem problem) {
		super(problem);
	}

	@Override
	protected double w(final int supports) {
		return -supports;
	}

}
