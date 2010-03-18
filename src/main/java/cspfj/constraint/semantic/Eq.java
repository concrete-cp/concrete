package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Eq extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("eq", Eq.class);
    }

    public Eq(Variable v0, Variable v1) {
        super(v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == getValue(1);
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        if (!(constraint instanceof GeneralConstraint)) {
            return false;
        }

        final Variable[] scope = ConstraintManager.getSolverVariables(
                constraint.getScope(), problem);
        Variable notNull = null;
        for (Variable v : scope) {
            if (v.getDomain() != null) {
                notNull = v;
            }
        }
        if (notNull == null) {
            return false;
        }
        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(notNull.getDomain().allValues()));
            }
        }
        problem.addConstraint(new Eq(scope[0], scope[1]));
        return true;
    }

    public String toString() {
        return getVariable(0) + " == " + getVariable(1);
    }
}
