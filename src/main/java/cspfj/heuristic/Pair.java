package cspfj.heuristic;

import cspfj.problem.Variable;

public class Pair {

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

}
