package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public final class ReifiedNeq extends AbstractAC3Constraint {

    public ReifiedNeq(final Variable result, final Variable v0,
            final Variable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == (getValue(1) != getValue(2) ? 1 : 0);
    }

    public String toString() {
        return getVariable(0) + " = " + getVariable(1) + " /= "
                + getVariable(2);
    }

    @Override
    public int getEvaluation(final int reviseCount) {
        int size = 1;
        for (Variable v : getScope()) {
            size *= v.getDomainSize();
        }
        return size;
    }
}
