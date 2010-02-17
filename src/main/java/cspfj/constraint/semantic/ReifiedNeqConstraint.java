package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public class ReifiedNeqConstraint extends AbstractAC3Constraint {

    public ReifiedNeqConstraint(Variable result, Variable v0, Variable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == (getValue(1) != getValue(2) ? 1 : 0);
    }

    public static boolean generate(Variable[] signature) {
        return false;
    }
}
