package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public final class AbsDiff extends AbstractAC3Constraint {

    public AbsDiff(final Variable result, final Variable v0, final Variable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == Math.abs(getValue(1) - getValue(2));
    }

    public String toString() {
        return getVariable(0) + " = |" + getVariable(1) + " - "
                + getVariable(2) + "|";
    }

    @Override
    public int getEvaluation(final int rev) {
        final int d0 = getVariable(0).getDomainSize();
        final int d1 = getVariable(1).getDomainSize();
        final int d2 = getVariable(2).getDomainSize();
        return d0 * d1 + d0 * d2 + d1 * d2;
    }
}
