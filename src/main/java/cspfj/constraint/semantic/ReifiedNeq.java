package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;

public final class ReifiedNeq extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("ne", ReifiedNeq.class);
    }

    public ReifiedNeq(Variable result, Variable v0, Variable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == (getValue(1) != getValue(2) ? 1 : 0);
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        if (!(constraint instanceof FunctionalConstraint)) {
            return false;
        }
        final FunctionalConstraint funcConstraint = (FunctionalConstraint) constraint;
        final Variable result = problem.getSolverVariable(funcConstraint
                .getResultVariable());
        final Variable v0 = problem.getSolverVariable(funcConstraint
                .getArguments()[0]);
        if (v0.getDomain() == null) {
            return false;
        }
        final Variable v1 = problem.getSolverVariable(funcConstraint
                .getArguments()[1]);
        if (v1.getDomain() == null) {
            return false;
        }
        if (result.getDomain() == null) {
            result.setDomain(new BitVectorDomain(0, 1));
        }
        problem.addConstraint(new ReifiedNeq(result, v0, v1));
        return true;
    }

    public String toString() {
        return getVariable(0) + " = " + getVariable(1) + " /= "
                + getVariable(2);
    }
}
