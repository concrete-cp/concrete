package cspfj.heuristic;

import cspfj.problem.IntVariable;

public class Pair {

	private final IntVariable variable;

	private final int index;

	public Pair(final IntVariable variable, final int index) {
		super();
		this.variable = variable;
		this.index = index;
	}

	public IntVariable getVariable() {
		return variable;
	}

	public int getIndex() {
		return index;
	}

}
