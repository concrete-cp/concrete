package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class Lexico extends AbstractStaticValueHeuristic {

    public Lexico(Problem problem, boolean failFirst) {
        super(problem, failFirst);
    }

    public double getScore(final Variable var, final int index) {
        return -index;
    }

	public String toString() {
		return (isFailFirst() ? "min" : "max") + "-lexico";
	}
}
