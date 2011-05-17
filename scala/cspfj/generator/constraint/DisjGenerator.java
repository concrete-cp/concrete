package cspfj.generator.constraint;

import java.util.Arrays;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.primitives.Booleans;

import cspfj.constraint.semantic.Disjunction;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;

public final class DisjGenerator extends AbstractGenerator {

    public DisjGenerator(final Problem problem) {
        super(problem);
    }

    private void generateReifiedOr(final List<Variable> scope,
            final FunctionalConstraint constraint) {
        /*
         * Reified disjunction is converted to CNF :
         * 
         * a = b v c v d...
         * 
         * <=>
         * 
         * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
         */
        Preconditions
                .checkArgument(constraint.parameters() == null,
                        "Negative literals in reified disjunctions are currently not supported");

        final boolean[] reverses = new boolean[scope.size()];

        reverses[0] = true;
        addConstraint(new Disjunction(
                scope.toArray(new Variable[scope.size()]), reverses));

        final Variable result = getSolverVariable(constraint
                .result());

        for (CSPOMVariable<?> v : constraint.getArguments()) {
            addConstraint(new Disjunction(new Variable[] { result,
                    getSolverVariable(v) }, new boolean[] { false, true }));
        }
    }

    private void generateReifiedAnd(final List<Variable> scope,
            final FunctionalConstraint constraint) {
        /*
         * Reified conjunction is converted to CNF :
         * 
         * a = b ^ c ^ d...
         * 
         * <=>
         * 
         * (a v -b v -c v -d...) ^ (-a v b) ^ (-a v c) ^ (-a v d) ^ ...
         */
        Preconditions
                .checkArgument(constraint.getParameters() == null,
                        "Negative literals in reified conjuctions are currently not supported");

        final boolean[] reverses = new boolean[scope.size()];

        Arrays.fill(reverses, 1, scope.size(), true);

        addConstraint(new Disjunction(
                scope.toArray(new Variable[scope.size()]), reverses));

        final Variable result = getSolverVariable(constraint
                .getResultVariable());

        for (CSPOMVariable v : constraint.getArguments()) {
            addConstraint(new Disjunction(new Variable[] { result,
                    getSolverVariable(v) }, new boolean[] { true, false }));
        }
    }

    private void generateNeg(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        /*
         * Negation is converted to CNF :
         * 
         * a = -b <=> (a v b) ^ (-a v -b)
         */
        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        final Variable arg = getSolverVariable(Iterables
                .getOnlyElement(constraint.getArguments()));

        addConstraint(new Disjunction(result, arg));
        addConstraint(new Disjunction(new Variable[] { result, arg },
                new boolean[] { true, true }));

    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final List<Variable> scope = Lists.transform(constraint.getScope(),
                cspomToCspfj);

        for (Variable v : scope) {
            booleanDomain(v);
        }

        if ("or".equals(constraint.getDescription())) {

            if (constraint instanceof GeneralConstraint) {
                addConstraint(new Disjunction(scope.toArray(new Variable[scope
                        .size()]), parseParameters(constraint.getParameters())));
            } else if (constraint instanceof FunctionalConstraint) {
                generateReifiedOr(scope, (FunctionalConstraint) constraint);
            } else {
                throw new IllegalArgumentException(
                        "Unhandled constraint type for " + constraint);
            }

        } else if ("not".equals(constraint.getDescription())
                && (constraint instanceof FunctionalConstraint)) {

            generateNeg((FunctionalConstraint) constraint);

        } else if ("and".equals(constraint.getDescription())
                && constraint instanceof FunctionalConstraint) {

            generateReifiedAnd(scope, (FunctionalConstraint) constraint);

        } else {
            throw new IllegalArgumentException("Unhandled constraint type for "
                    + constraint);
        }

        return true;
    }

    private static boolean[] parseParameters(final String parameters) {
        if (parameters == null) {
            return null;
        }
        return Booleans.toArray(ImmutableList.copyOf(Iterables.transform(
                Splitter.on(',').trimResults().split(parameters),
                new Function<String, Boolean>() {
                    @Override
                    public Boolean apply(String input) {
                        return Integer.parseInt(input) > 0;
                    }
                })));
    }
}
