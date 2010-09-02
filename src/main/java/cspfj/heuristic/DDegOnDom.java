package cspfj.heuristic;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DDegOnDom extends AbstractRandVariableHeuristic {

    public DDegOnDom(final Problem problem) {
        super(problem);
    }

    public double getScore(final Variable variable) {
        return dDeg(variable) / variable.getDomainSize();
    }

    private double dDeg(final Variable variable) {
        double count = 0;

        for (Constraint c : variable.getInvolvingConstraints()) {
            if (!c.isEntailed()) {
                count += WDeg.nbUnboundVariables(c) - 1;
            }
        }
        return count;
    }

    public String toString() {
        return "max-ddeg/dom";
    }

}
