package cspfj.constraint.semantic;

import cspfj.constraint.AbstractFastPVRConstraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Neq extends AbstractFastPVRConstraint {

    static {
        ConstraintManager.register("ne", Neq.class);
    }

    /**
     * Corresponding scope is reversed!
     */
    private final int[][] corresponding;

    public Neq(Variable v0, Variable v1) {
        super(v0, v1);
        final Domain v0Dom = v0.getDomain();
        final Domain v1Dom = v1.getDomain();
        corresponding = new int[][] { new int[v1Dom.maxSize()],
                new int[v0Dom.maxSize()] };

        for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
            corresponding[1][i] = v1Dom.index(v0Dom.value(i));
        }
        for (int i = v1Dom.first(); i >= 0; i = v1Dom.next(i)) {
            corresponding[0][i] = v0Dom.index(v1Dom.value(i));
        }
    }

    @Override
    public boolean check() {
        return getValue(0) != getValue(1);
    }

    @Override
    public boolean revise(int i) {
        final Domain otherDom = getVariable(1 - i).getDomain();
        if (otherDom.size() == 1) {
            final Domain dom = getVariable(i).getDomain();
            final int index = corresponding[i][otherDom.first()];
            if (index >= 0 && dom.present(index)) {
                dom.remove(index);
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
                return false;
            }
        }
        problem.addConstraint(new Neq(scope[0], scope[1]));
        return true;
    }

    public String toString() {
        return getVariable(0) + " /= " + getVariable(1);
    }

    public int getEvaluation(int revise) {
        return getArity();
    }

}
