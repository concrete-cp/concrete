package cspfj.constraint.semantic;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class Neq extends AbstractPVRConstraint {

    static {
        ConstraintManager.register("ne", Neq.class);
    }

    public Neq(Variable v0, Variable v1) {
        super(v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) != getValue(1);
    }

    @Override
    public boolean revise(int i) {
        final Domain dom = getVariable(1 - i).getDomain();
        if (dom.size() == 1) {
            final int val = dom.value(dom.first());
            final Domain otherDom = getVariable(i).getDomain();
            final int index = otherDom.index(val);
            if (index >= 0 && otherDom.present(index)) {
                otherDom.remove(index);
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

}
