package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Pair {

	private Pair() {
		super();
	}

	public static long pair(final Variable variable, final int index, final Problem problem) {
		return variable.getId() * problem.getMaxDomainSize() + index;
	}

	public static Variable variable(final long pair, final Problem problem) {
		return problem.getVariable(variableId(pair, problem));
	}
	
	public static int variableId(final long pair, final Problem problem) {
		return (int)(pair / problem.getMaxDomainSize()) ;
	}

	public static int index(final long pair, final Problem problem) {
		return (int)(pair % problem.getMaxDomainSize());
	}
}
