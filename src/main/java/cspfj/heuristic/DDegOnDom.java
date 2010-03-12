package cspfj.heuristic;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.IntVariable;

public final class DDegOnDom extends AbstractVariableHeuristic {

	public DDegOnDom(Problem problem) {
		super(problem);
	}

	public double getScore(final IntVariable variable) {
        return dDeg(variable) / variable.getDomainSize();
    }
	


	private double dDeg(final IntVariable variable) {
		double count = 0;

		for (Constraint c : variable.getInvolvingConstraints()) {
			if (c.isBound(variable)) {
				count++;
			}
		}
		return count;
	}

	
	public String toString() {
		return "max-ddeg/dom";
	}

}
