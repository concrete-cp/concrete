package cspfj.generator.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import cspfj.constraint.semantic.InInterval;
import cspfj.constraint.semantic.NotInInterval;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.constraint.semantic.SumLeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.IntLinkedList;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class AllDifferentGenerator extends AbstractGenerator {

    private static int allDiff = 0;

    public AllDifferentGenerator(final Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {

        if (!(constraint instanceof GeneralConstraint)) {
            throw new FailedGenerationException(constraint
                    + " is not supported");
        }

        final Variable[] solverVariables = getSolverVariables(constraint
                .getScope());
        if (nullVariable(solverVariables) != null) {
            return false;
        }

        final int[] values = values(solverVariables);

        for (int l = 0; l < values.length; l++) {
            final int lV = values[l];
            for (int u = l; u < values.length
                    && u - l + 1 < solverVariables.length; u++) {
                final int uV = values[u];
                final Collection<Variable> sum = new ArrayList<Variable>();
                for (Variable v : solverVariables) {

                    final int lIndex = v.getDomain().lowest(lV);
                    if (lIndex < 0) {
                        continue;
                    }
                    final int uIndex = v.getDomain().greatest(uV);
                    if (uIndex < 0) {
                        continue;
                    }

                    final Variable aux = addVariable("A" + allDiff + "_"
                            + v.getName() + "_" + lV + "_" + uV,
                            new BooleanDomain());
                    addConstraint(new ReifiedConstraint(aux, InInterval
                            .indexes(v, lIndex, uIndex), NotInInterval.indexes(
                            v, lIndex, uIndex)));
                    sum.add(aux);

                }
                if (sum.size() > u - l + 1) {
                    addConstraint(new SumLeq(u - l + 1, sum
                            .toArray(new Variable[sum.size()])));
                }
            }

        }
        allDiff++;
        return true;
    }

    private static int[] values(final Variable[] variables) {
        final Set<Integer> valueSet = new HashSet<Integer>();
        for (Variable v : variables) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                valueSet.add(v.getValue(i));
            }
        }
        final int[] values = IntLinkedList.intCollectionToArray(valueSet);
        Arrays.sort(values);
        return values;
    }

}
