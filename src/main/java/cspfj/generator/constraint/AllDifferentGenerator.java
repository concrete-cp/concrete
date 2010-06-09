package cspfj.generator.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
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

        final Map<String, Variable> auxVars = new HashMap<String, Variable>();
        final Variable[][][] A = new Variable[solverVariables.length][values.length][values.length];
        for (int i = solverVariables.length; --i >= 0;) {
            final Variable solverVariable = solverVariables[i];
            for (int u = values.length; --u >= 0;) {
                final int uV = values[u];
                for (int l = u; l >= 0 && l > u - solverVariables.length + 1; l--) {

                    final int lV = values[l];

                    final int lIndex = solverVariable.getDomain().lowest(lV);
                    if (lIndex < 0) {
                        continue;
                    }
                    final int uIndex = solverVariable.getDomain().greatest(uV);
                    if (uIndex < 0) {
                        continue;
                    }
                    final String varRef = solverVariable.getName() + "_"
                            + lIndex + "_" + uIndex;
                    Variable auxVar = auxVars.get(varRef);
                    if (auxVar == null) {
                        auxVar = addVariable("A" + allDiff + "_"
                                + solverVariable.getName() + "_"
                                + solverVariable.getValue(lIndex) + "_"
                                + solverVariable.getValue(uIndex),
                                new BooleanDomain());
                        auxVars.put(varRef, auxVar);
                        addConstraint(new ReifiedConstraint(auxVar, InInterval
                                .indexes(solverVariable, lIndex, uIndex),
                                NotInInterval.indexes(solverVariable, lIndex,
                                        uIndex)));
                    }

                    A[i][l][u] = auxVar;

                }
            }
        }

        for (int u = values.length; --u >= 0;) {
            for (int l = u; l >= 0 && l > u - solverVariables.length + 1; l--) {
                final Collection<Variable> sum = new ArrayList<Variable>();
                for (int i = solverVariables.length; --i >= 0;) {
                    if (A[i][l][u] != null) {
                        sum.add(A[i][l][u]);
                    }
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
