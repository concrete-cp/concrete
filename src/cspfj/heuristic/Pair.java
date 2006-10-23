package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Pair implements Comparable<Pair> {

	private final Variable variable;

	private final int value;

	private static VariableHeuristic heuristic;

	public Pair(Variable variable, int value) {
		super();
		this.variable = variable;
		this.value = value;
	}

	public int getValue() {
		return value;
	}

	public Variable getVariable() {
		return variable;
	}

	public int compareTo(final Pair arg0) {
		final int wdeg = heuristic.compare(variable, arg0.getVariable());

		if (wdeg == 0) {
			return arg0.getValue() - value ;
		}
		return wdeg;
	}
	
	public static void setHeuristic(final VariableHeuristic heur) {
		heuristic = heur ;
	}

}
