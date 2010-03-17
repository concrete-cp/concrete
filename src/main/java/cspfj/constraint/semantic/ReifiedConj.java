package cspfj.constraint.semantic;

import java.util.List;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Arrays2;
import cspom.constraint.CSPOMConstraint;

public final class ReifiedConj extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("and", ReifiedConj.class);
    }

    public ReifiedConj(Variable result, Variable... conj) {
        super(Arrays2.addBefore(result, conj, new Variable[conj.length + 1]));
    }

    @Override
    public boolean check() {
        return getValue(0) == conjonction();
    }

    private int conjonction() {
        for (int i = getArity(); --i >= 1;) {
            if (getValue(i) == 0) {
                return 0;
            }
        }
        return 1;
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {

        final Variable[] scope = ConstraintManager.getSolverVariables(
                constraint.getScope(), problem);

        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(0, 1));
            }
        }
        problem.addConstraint(new ReifiedConj(scope[0], scope[1], scope[2]));
        return true;
    }

    public String toString() {
        return getVariable(0) + " = " + getVariable(1) + " /\\ "
                + getVariable(2);
    }
}
