package cspfj.constraint.semantic;

import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

public final class Abs extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("abs", Abs.class);
    }

    public Abs(Variable result, Variable v0) {
        super(result, v0);
    }

    @Override
    public boolean check() {
        return getValue(0) == Math.abs(getValue(1));
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        final Variable v0 = problem
                .getSolverVariable(constraint.getVariable(1));
        final Variable result = problem.getSolverVariable(constraint
                .getVariable(0));

        if (v0.getDomain() == null) {

            if (result.getDomain() == null) {
                return false;
            }

            final SortedSet<Integer> values = new TreeSet<Integer>();
            for (int i : result.getDomain().allValues()) {
                values.add(i);
                values.add(-i);
            }
            v0.setDomain(new BitVectorDomain(IntLinkedList
                    .intCollectionToArray(values)));

        } else if (result.getDomain() == null) {

            final SortedSet<Integer> values = new TreeSet<Integer>();
            for (int i : v0.getDomain().allValues()) {
                values.add(Math.abs(i));
            }
            result.setDomain(new BitVectorDomain(IntLinkedList
                    .intCollectionToArray(values)));

        }
        problem.addConstraint(new Abs(result, v0));
        return true;
    }

    public String toString() {
        return getVariable(0) + " = |" + getVariable(1) + "|";
    }

}
