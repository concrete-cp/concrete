package cspfj.heuristic;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DDegOnDom extends AbstractVariableHeuristic {

	public DDegOnDom(final Problem problem) {
		super(problem);
	}

	public double getScore(final Variable variable) {
		return dDeg(variable) / variable.getDomainSize();
	}

	private double dDeg(final Variable variable) {
		double count = 0;

		for (Constraint c : variable.getInvolvingConstraints()) {
			count += WDeg.nbUnboundVariables(c);
		}
		return count;
	}

	public String toString() {
		return "max-ddeg/dom";
	}

}
