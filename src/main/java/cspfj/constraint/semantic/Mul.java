package cspfj.constraint.semantic;

import java.util.Arrays;
import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

/**
 * V0 = V1 * V2
 * 
 * @author vion
 * 
 */
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

    @Override
    public boolean findSupport(final int variablePosition, final int index) {
        switch (variablePosition) {
        case 0:
            return findValidTuple0(index);
        case 1:
            return findValidTuple1(index);
        case 2:
            return findValidTuple2(index);
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    private boolean findValidTuple0(final int index) {
        final int val0 = getVariable(0).getValue(index);
        final Domain dom1 = getVariable(1).getDomain();
        final Domain dom2 = getVariable(2).getDomain();
        for (int i = dom1.first(); i >= 0; i = dom1.next(i)) {
            final int val1 = dom1.value(i);
            if ((val1 == 0 && val0 != 0) || val0 % val1 != 0) {
                continue;
            }
            final int j = dom2.index(val0 / val1);
            if (j >= 0 && dom2.present(j)) {
                tuple[0] = index;
                tuple[1] = i;
                tuple[2] = j;
                return true;
            }
        }
        return false;
    }

    private boolean findValidTuple1(final int index) {
        final Domain result = getVariable(0).getDomain();
        final int val = getVariable(1).getValue(index);
        final Domain dom = getVariable(2).getDomain();
        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            final int resIndex = result.index(val * dom.value(i));
            if (resIndex >= 0 && result.present(resIndex)) {
                tuple[0] = resIndex;
                tuple[1] = index;
                tuple[2] = i;
                return true;
            }
        }
        return false;
    }

    private boolean findValidTuple2(final int index) {
        final Domain result = getVariable(0).getDomain();
        final int val = getVariable(2).getValue(index);
        final Domain dom = getVariable(1).getDomain();
        for (int i = dom.first(); i >= 0; i = dom.next(i)) {
            final int resIndex = result.index(val * dom.value(i));
            if (resIndex >= 0 && result.present(resIndex)) {
                tuple[0] = resIndex;
                tuple[1] = i;
                tuple[2] = index;
                return true;
            }
        }
        return false;
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        final Variable result = problem.getSolverVariable(constraint
                .getVariable(0));
        final Variable v0 = problem
                .getSolverVariable(constraint.getVariable(1));
        final Variable v1 = problem
                .getSolverVariable(constraint.getVariable(2));

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
