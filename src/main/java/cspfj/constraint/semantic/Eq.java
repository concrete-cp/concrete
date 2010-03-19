package cspfj.constraint.semantic;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Eq extends AbstractPVRConstraint {

    static {
        ConstraintManager.register("eq", Eq.class);
    }

    private final int[][] corresponding;

    public Eq(Variable v0, Variable v1) {
        super(v0, v1);
        final Domain v0Dom = v0.getDomain();
        final Domain v1Dom = v1.getDomain();
        corresponding = new int[][] { new int[v0Dom.maxSize()],
                new int[v1Dom.maxSize()] };

        for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
            corresponding[0][i] = v1Dom.index(v0Dom.value(i));
        }
        for (int i = v1Dom.first(); i >= 0; i = v1Dom.next(i)) {
            corresponding[1][i] = v0Dom.index(v1Dom.value(i));
        }
    }

    @Override
    public boolean check() {
        return getValue(0) == getValue(1);
    }

    public boolean revise(int position) {
        boolean change = false;
        final Domain dom = getVariable(position).getDomain();
        final Domain otherDom = getVariable(1 - position).getDomain();
        final int[] correspond = corresponding[position];
        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            final int index = correspond[i];
            if (index < 0 || !otherDom.present(index)) {
                dom.remove(i);
                change = true;
            }
        }
        return change;
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
