package cspfj.generator.constraint;

import static com.google.common.collect.Sets.newTreeSet;

import java.util.NoSuchElementException;
import java.util.SortedSet;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.primitives.Ints;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Add;
import cspfj.constraint.semantic.Eq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
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
        if ("sub".equals(constraint.description())) {
            result = getSolverVariable(constraint.getVariable(1));
            v0 = getSolverVariable(constraint.getVariable(0));
            v1 = getSolverVariable(constraint.getVariable(2));
        } else if ("add".equals(constraint.description())) {
            result = getSolverVariable(constraint.getVariable(0));
            v0 = getSolverVariable(constraint.getVariable(1));
            v1 = getSolverVariable(constraint.getVariable(2));
        } else {
            throw new IllegalArgumentException("Cannot handle " + constraint);
        }

        try {
            final Variable nullVariable = Iterables.getOnlyElement(Iterables
                    .filter(ImmutableList.of(result, v0, v1), NULL_DOMAIN));

            if (nullVariable == result) {

                final SortedSet<Integer> values = newTreeSet();
                for (int i : v0.getDomain().allValues()) {
                    for (int j : v1.getDomain().allValues()) {
                        values.add(i + j);
                    }
                }
                result.setDomain(new BitVectorDomain(Ints.toArray(values)));

            } else if (nullVariable == v0) {

                v0.setDomain(new BitVectorDomain(generateValues(result, v1)));

            } else if (nullVariable == v1) {

                v1.setDomain(new BitVectorDomain(generateValues(result, v0)));

            } else {

                throw new IllegalStateException();

            }
        } catch (NoSuchElementException e) {
            // All variables are defined, continue...
        } catch (IllegalArgumentException e) {
            // More than 1 variable is undefined
            return false;
        }

        final Constraint generated;
        if (result.getDomainSize() == 1) {
            generated = new Eq(-1, v0, result.getFirstValue(), v1);
        } else if (v0.getDomainSize() == 1) {
            generated = new Eq(1, v1, v0.getFirstValue(), result);
        } else if (v1.getDomainSize() == 1) {
            generated = new Eq(1, v0, v1.getFirstValue(), result);
        } else {
            generated = new Add(result, v0, v1);
        }
        addConstraint(generated);
        return true;

    }

    private static int[] generateValues(final Variable result,
            final Variable var) {
        final SortedSet<Integer> values = newTreeSet();
        for (int i : result.getDomain().allValues()) {
            for (int j : var.getDomain().allValues()) {
                values.add(i - j);
            }
        }

        return Ints.toArray(values);
    }

}
