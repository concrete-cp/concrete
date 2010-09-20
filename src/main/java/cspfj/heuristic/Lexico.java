package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Lexico implements ValueHeuristic {

	public Lexico() {
	}

	public double getScore(final Variable var, final int index) {
		return index;
	}

	public String toString() {
		return "lexico";
	}

	@Override
	public void compute() {
		// Nothing to compute
	}

	@Override
	public int selectIndex(final Variable variable) {
		return variable.getFirst();
	}
}
