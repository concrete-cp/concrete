package cspfj.heuristic;

import cspfj.problem.Variable;

public final class Lexico implements ValueHeuristic {

	private final boolean reverse;

	public Lexico(boolean reverse) {
		this.reverse = reverse;
	}

	public double getScore(final Variable var, final int index) {
		return reverse ? -index : index;
	}

	public String toString() {
		return (reverse ? "min" : "max") + "-lexico";
	}

	@Override
	public void compute() {
		// TODO Auto-generated method stub

	}

	@Override
	public int selectIndex(Variable variable) {
		return reverse ? variable.getLast() : variable.getFirst();
	}
}
