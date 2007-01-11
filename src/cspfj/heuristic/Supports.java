package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Supports extends AbstractStaticValueHeuristic {

	public Supports(Problem problem, boolean failFirst) {
		super(problem, failFirst);
	}

	public double getScore(final Variable variable, final int index) {
		return variable.getNbSupports(index);
	}

	public String toString() {
		return (isFailFirst() ? "min" : "max") + "-supports";
	}
}
