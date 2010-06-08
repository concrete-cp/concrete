package cspfj.generator.constraint;

import java.security.InvalidParameterException;
import java.util.Arrays;

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

    private void generateReify(final Variable[] scope,
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
        final boolean[] parameters = parseParameters(constraint.getParameters());
        if (parameters != null && parameters.length != scope.length - 1) {
            throw new InvalidParameterException(
                    "Incorrect number of parameters");
        }
        final boolean[] reverses = new boolean[scope.length];
        if (parameters != null) {
            System.arraycopy(parameters, 0, reverses, 1, parameters.length);
        }
        reverses[0] = true;
        addConstraint(new Disjunction(scope, reverses));

        final Variable result = getSolverVariable(constraint
                .getResultVariable());

        for (CSPOMVariable v : constraint.getArguments()) {
            addConstraint(new Disjunction(new Variable[] { result,
                    getSolverVariable(v) }, new boolean[] { false, true }));
        }
    }

    private void generateNeg(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        /*
         * Negation is converted to CNF :
         * 
         * a = -b <=> (a v b) ^ (-a v -b)
         */
        if (constraint.getArguments().length != 1) {
            throw new FailedGenerationException(
                    "not() must have only one argument, found "
                            + Arrays.toString(constraint.getArguments()));
        }
        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        final Variable arg = getSolverVariable(constraint.getArguments()[0]);

        addConstraint(new Disjunction(result, arg));
        addConstraint(new Disjunction(new Variable[] { result, arg }, new boolean[] {
                true, true }));

    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Variable[] scope = getSolverVariables(constraint.getScope());

        for (Variable v : scope) {
            booleanDomain(v);
        }

        if ("or".equals(constraint.getDescription())) {

            if (constraint instanceof GeneralConstraint) {
                addConstraint(new Disjunction(scope, parseParameters(constraint
                        .getParameters())));
            } else if (constraint instanceof FunctionalConstraint) {
                generateReify(scope, (FunctionalConstraint) constraint);
            } else {
                throw new IllegalArgumentException(
                        "Unhandled constraint type for " + constraint);
            }

        } else if ("not".equals(constraint.getDescription())) {

            if (!(constraint instanceof FunctionalConstraint)) {
                throw new FailedGenerationException(
                        "Unhandled constraint type for " + constraint);
            }
            generateNeg((FunctionalConstraint) constraint);

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
        final String[] params = parameters.split(",");
        final boolean[] reverses = new boolean[params.length];
        for (int i = params.length; --i >= 0;) {
            reverses[i] = Integer.parseInt(params[i].trim()) > 0;
        }
        return reverses;
    }
}
