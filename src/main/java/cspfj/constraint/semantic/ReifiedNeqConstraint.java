package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.IntVariable;

public class ReifiedNeqConstraint extends AbstractAC3Constraint {

    public ReifiedNeqConstraint(IntVariable result, IntVariable v0, IntVariable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == (getValue(1) != getValue(2) ? 1 : 0);
    }

    public static boolean generate(IntVariable[] signature) {
        return false;
    }
}
