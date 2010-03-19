package cspfj.constraint.semantic;

import java.util.Arrays;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Disj extends AbstractConstraint {

    static {
        ConstraintManager.register("or", Disj.class);
    }

    private int watch1 = -1;
    private int watch2 = -1;

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

    @Override
    public boolean revise(RevisionHandler revisator, int reviseCount) {
        if (watch1 < 0 || isFalse(getVariable(watch1).getDomain())) {
            watch1 = seekWatch(watch2);
            if (watch1 < 0) {
                if (watch2 < 0) {
                    return false;
                }
                if (setTrue(getVariable(watch2).getDomain())) {
                    revisator.revised(this, getVariable(watch2));
                    return true;
                }
            }
        }
        if (watch2 < 0 || isFalse(getVariable(watch2).getDomain())) {
            watch2 = seekWatch(watch1);
            if (watch2 < 0) {
                if (watch1 < 0) {
                    return false;
                }
                if (setTrue(getVariable(watch1).getDomain())) {
                    revisator.revised(this, getVariable(watch1));
                }
            }
        }
        return true;
    }

    private static boolean isFalse(final Domain dom) {
        return dom.size() == 1 && dom.value(dom.first()) == 0;
    }

    private static boolean setTrue(final Domain dom) {
        if (dom.size() == 1) {
            return false;
        }
        dom.remove(0);
        return true;
    }

    private int seekWatch(int excluding) {
        for (int i = 0; i < getArity(); i++) {
            if (i != excluding) {
                final Domain dom = getVariable(i).getDomain();
                if (dom.size() == 2 || dom.value(dom.first()) == 1) {
                    return i;
                }
            }
        }
        return -1;
    }
}
