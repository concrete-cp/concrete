package cspfj.generator.constraint;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.SortedSet;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.primitives.Ints;

import cspfj.constraint.semantic.InInterval;
import cspfj.constraint.semantic.NotInInterval;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.constraint.semantic.SumLeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class DecomposedAllDifferentGenerator extends AbstractGenerator {

    private static int allDiff = 0;

    public DecomposedAllDifferentGenerator(final Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        Preconditions.checkArgument(constraint instanceof GeneralConstraint,
                "%s is not supported", constraint);

        final List<Variable> solverVariables = Lists.transform(
                constraint.getScope(), cspomToCspfj);
        if (Iterables.any(solverVariables, NULL_DOMAIN)) {
            return false;
        }

        final int[] values = values(solverVariables);
        final Map<VariableInterval, VariableInterval> vis = new HashMap<VariableInterval, VariableInterval>();

        for (int l = 0; l < values.length; l++) {
            final int lV = values[l];
            for (int u = l; u < values.length
                    && u - l + 1 < solverVariables.size(); u++) {
                final int uV = values[u];
                final List<VariableInterval> sum = new ArrayList<VariableInterval>();
                for (Variable v : solverVariables) {

                    final int lIndex = v.getDomain().lowest(lV);
                    if (lIndex < 0) {
                        continue;
                    }
                    final int uIndex = v.getDomain().greatest(uV);
                    if (uIndex < 0) {
                        continue;
                    }

                    final VariableInterval currentVi = new VariableInterval(v,
                            lIndex, uIndex);

                    VariableInterval actualVi = vis.get(currentVi);
                    if (actualVi == null) {
                        actualVi = currentVi;
                        vis.put(actualVi, actualVi);
                    }

                    sum.add(actualVi);

                }
                if (sum.size() > u - l + 1) {
                    final Variable[] scope = new Variable[sum.size()];
                    for (ListIterator<VariableInterval> itr = sum
                            .listIterator(); itr.hasNext();) {
                        scope[itr.nextIndex()] = itr.next().add();
                    }

                    addConstraint(new SumLeq(u - l + 1, scope));
                }
            }

        }
        allDiff++;
        return true;
    }

    private final class VariableInterval {
        private final Variable variable;
        private final int lb;
        private final int ub;
        private Variable auxVariable;

        private VariableInterval(final Variable variable, final int lb,
                final int ub) {
            this.variable = variable;
            this.lb = lb;
            this.ub = ub;
        }

        private Variable add() {
            if (auxVariable == null) {
                auxVariable = addVariable(
                        "_A" + allDiff + "_" + variable.getName() + "_"
                                + variable.getValue(lb) + "_"
                                + variable.getValue(ub), new BooleanDomain());

                addConstraint(new ReifiedConstraint(auxVariable,
                        InInterval.indexes(variable, lb, ub),
                        NotInInterval.indexes(variable, lb, ub)));
            }
            return auxVariable;
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(variable.hashCode(), lb, ub);
        }

        @Override
        public boolean equals(final Object obj) {
            if (!(obj instanceof VariableInterval)) {
                return false;
            }
            final VariableInterval vi = (VariableInterval) obj;
            return variable == vi.variable && lb == vi.lb && ub == vi.ub;
        }

        @Override
        public String toString() {
            return "(" + variable + ", " + variable.getValue(lb) + ", "
                    + variable.getValue(ub) + ")";
        }
    }

    private static int[] values(final List<Variable> variables) {
        final SortedSet<Integer> valueSet = Sets.newTreeSet();
        for (Variable v : variables) {
            for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {
                valueSet.add(v.getValue(i));
            }
        }
        return Ints.toArray(valueSet);
    }

}
