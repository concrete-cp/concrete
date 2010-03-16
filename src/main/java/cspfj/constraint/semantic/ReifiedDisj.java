package cspfj.constraint.semantic;

import java.util.List;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Arrays2;
import cspom.constraint.CSPOMConstraint;

public final class ReifiedDisj extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("or", ReifiedDisj.class);
    }

    public ReifiedDisj(Variable result, Variable... disj) {
        super(Arrays2.addBefore(result, disj, new Variable[disj.length + 1]));
    }

    @Override
    public boolean check() {
        return getValue(0) == disjunction();
    }

    private int disjunction() {
        for (int i = getArity(); --i >= 1;) {
            if (getValue(i) == 1) {
                return 1;
            }
        }
        return 0;
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {

        final List<Variable> scope = ConstraintManager.getSolverVariables(
                constraint.getScope(), problem);

        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(0, 1));
            }
        }
        problem.addConstraint(new ReifiedDisj(scope.get(0), scope.get(1), scope
                .get(2)));
        return true;
    }

    public String toString() {
        return getVariable(0) + " = " + getVariable(1) + " \\/ "
                + getVariable(2);
    }
}
