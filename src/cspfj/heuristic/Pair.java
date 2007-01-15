package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Pair {

	private static int maxV;

	private static Problem problem;

	private Pair() {
		super();
	}

	public static void setProblem(final Problem problem) {
		maxV = problem.getMaxDomainSize();
		Pair.problem = problem;
	}

	public static long pair(final Variable variable, final int index) {
		return variable.getId() * maxV + index;
	}

	public static Variable variable(final long pair) {
		return problem.getVariable((int)(pair / maxV));
	}

	public static int index(final long pair) {
		return (int)(pair % maxV);
	}
}
