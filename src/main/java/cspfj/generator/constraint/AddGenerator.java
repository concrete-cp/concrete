package cspfj.generator.constraint;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import cspfj.constraint.semantic.Add;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;

public final class AddGenerator extends AbstractGenerator {

    public AddGenerator(final Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Variable result;
        final Variable v0;
        final Variable v1;
        if ("sub".equals(constraint.getDescription())) {
            result = getSolverVariable(constraint.getVariable(1));
            v0 = getSolverVariable(constraint.getVariable(0));
            v1 = getSolverVariable(constraint.getVariable(2));
        } else if ("add".equals(constraint.getDescription())) {
            result = getSolverVariable(constraint.getVariable(0));
            v0 = getSolverVariable(constraint.getVariable(1));
            v1 = getSolverVariable(constraint.getVariable(2));
        } else {
            throw new IllegalArgumentException("Cannot handle " + constraint);
        }

        int nulls = 0;
        for (Variable v : Arrays.asList(result, v0, v1)) {
            if (v.getDomain() == null) {
                nulls++;
            }
        }

        if (nulls > 1) {
            return false;
        }
        if (nulls == 1) {
            if (result.getDomain() == null) {

                final SortedSet<Integer> values = new TreeSet<Integer>();
                for (int i : v0.getDomain().allValues()) {
                    for (int j : v1.getDomain().allValues()) {
                        values.add(i + j);
                    }
                }
                result.setDomain(new BitVectorDomain(IntLinkedList
                        .intCollectionToArray(values)));

            } else if (v0.getDomain() == null) {

                v0.setDomain(new BitVectorDomain(generateValues(result, v1)));

            } else if (v1.getDomain() == null) {

                v1.setDomain(new BitVectorDomain(generateValues(result, v0)));

            } else {
                
                throw new IllegalStateException();
                
            }
        }
        addConstraint(new Add(result, v0, v1));
        return true;

    }

    private static int[] generateValues(final Variable result,
            final Variable var) {
        final Set<Integer> values = new HashSet<Integer>();
        for (int i : result.getDomain().allValues()) {
            for (int j : var.getDomain().allValues()) {
                values.add(i - j);
            }
        }
        final int[] sortedValues = IntLinkedList.intCollectionToArray(values);
        Arrays.sort(sortedValues);
        return sortedValues;
    }

}
