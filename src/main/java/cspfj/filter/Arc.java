package cspfj.filter;

import cspfj.constraint.Constraint;

public final class Arc {
    private final Constraint constraint;
    private final int position;

    public Arc(Constraint constraint, int position) {
        this.constraint = constraint;
        this.position = position;
    }

    public Constraint getConstraint() {
        return constraint;
    }

    public int getVariablePosition() {
        return position;
    }
}
