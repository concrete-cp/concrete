package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class Pair {

	private static int maxV;

	private static Problem problem;

	private Pair() {
		super();
	}

	public static void setProblem(final Problem problem) {
		maxV = problem.getMaxDomainSize();
		Pair.problem = problem;
	}

	public static int pair(final Variable variable, final int index) {
		return variable.getId() * maxV + index;
	}

	public static Variable variable(final int pair) {
		return problem.getVariable(pair / maxV);
	}

	public static int index(final int pair) {
		return pair % maxV;
	}
}
