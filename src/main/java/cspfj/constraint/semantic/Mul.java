package cspfj.constraint.semantic;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

public final class Mul extends AbstractAC3Constraint {

    static {
        ConstraintManager.register("mul", Mul.class);
    }

    public Mul(Variable result, Variable v0, Variable v1) {
        super(result, v0, v1);
    }

    @Override
    public boolean check() {
        return getValue(0) == (getValue(1) * getValue(2));
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        final Variable result;
        final Variable v0;
        final Variable v1;

        result = problem.getSolverVariable(constraint.getVariable(0));
        v0 = problem.getSolverVariable(constraint.getVariable(1));
        v1 = problem.getSolverVariable(constraint.getVariable(2));

        int nulls = 0;
        for (Variable v : Arrays.asList(result, v0, v1)) {
            if (v.getDomain() == null) {
                nulls++;
            }
        }

        if (nulls > 1) {
            return false;
        } else if (nulls == 1) {
            if (result.getDomain() == null) {
                final SortedSet<Integer> values = new TreeSet<Integer>();
                for (int i : v0.getDomain().allValues()) {
                    for (int j : v1.getDomain().allValues()) {
                        values.add(i * j);
                    }
                }
                result.setDomain(new BitVectorDomain(IntLinkedList
                        .intCollectionToArray(values)));
            } else if (v0.getDomain() == null) {
                final SortedSet<Integer> values = new TreeSet<Integer>();
                for (int i : result.getDomain().allValues()) {
                    for (int j : v1.getDomain().allValues()) {
                        if (i % j == 0) {
                            values.add(i / j);
                        }
                    }
                }
                v0.setDomain(new BitVectorDomain(IntLinkedList
                        .intCollectionToArray(values)));
            } else if (v1.getDomain() == null) {
                final SortedSet<Integer> values = new TreeSet<Integer>();
                for (int i : result.getDomain().allValues()) {
                    for (int j : v0.getDomain().allValues()) {
                        if (i % j == 0) {
                            values.add(i / j);
                        }
                    }
                }
                v1.setDomain(new BitVectorDomain(IntLinkedList
                        .intCollectionToArray(values)));
            } else {
                throw new IllegalStateException();
            }
        }
        problem.addConstraint(new Mul(result, v0, v1));
        return true;
    }

    public String toString() {
        return getVariable(0) + " = " + getVariable(1) + " * " + getVariable(2);
    }
}
