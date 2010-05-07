package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Pair {

	private final Variable variable;

	private final int index;

	public Pair(final Variable variable, final int index) {
		super();
		this.variable = variable;
		this.index = index;
	}

	public Variable getVariable() {
		return variable;
	}

	public int getIndex() {
		return index;
	}

	public String toString() {
		return "(" + variable + ", " + variable.getValue(index) + ")";
	}

}
