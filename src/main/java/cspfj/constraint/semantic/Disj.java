package cspfj.constraint.semantic;

import java.util.Arrays;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Disj extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("or", Disj.class);
    }

    public Disj(Variable... disj) {
        super(disj);
    }

    @Override
    public boolean check() {
        for (int i = getArity(); --i >= 0;) {
            if (getValue(i) == 1) {
                return true;
            }
        }
        return false;
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        if (!(constraint instanceof GeneralConstraint)) {
            return false;
        }

        final Variable[] scope = ConstraintManager.getSolverVariables(
                constraint.getScope(), problem);

        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(0, 1));
            }
        }
        problem.addConstraint(new Disj(scope));
        return true;
    }

    public String toString() {
        return "\\/" + Arrays.toString(getScope());
    }

//    @Override
//    public boolean revise(RevisionHandler revisator, int reviseCount) {
//        Variable uninstanciated = null;
//        for (Variable v : getScope()) {
//            if (v.getDomainSize() > 1) {
//                if (uninstanciated == null) {
//                    uninstanciated = v;
//                } else {
//                    return true;
//                }
//            } else if (v.getValue(v.getFirst()) == 1) {
//                return true;
//            }
//        }
//        if (uninstanciated == null) {
//            return false;
//        }
//        uninstanciated.remove(0);
//        revisator.revised(this, uninstanciated);
//        return true;
//    }
}
