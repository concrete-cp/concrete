package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedNeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class NeqGenerator extends AbstractGenerator {

    public NeqGenerator(final Problem problem) {
        super(problem);
    }

    private Constraint generateGeneral(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Variable[] scope = getSolverVariables(constraint.getScope());
        if (scope.length != 2) {
            throw new FailedGenerationException(
                    "Comparison constraints must have exactly two arguments");
        }
        if (nullVariable(scope) != null) {
            return null;
        }
        return new Neq(scope[0], scope[1]);
    }

    private Constraint generateReified(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        final Variable[] arguments = getSolverVariables(constraint
                .getArguments());
        if (arguments.length != 2) {
            throw new FailedGenerationException(
                    "Comparison constraints must have exactly two arguments");
        }
        if (nullVariable(arguments) != null) {
            return null;
        }

        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        booleanDomain(result);

        return new ReifiedNeq(result, arguments[0], arguments[1]);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Constraint generated;
        if (constraint instanceof GeneralConstraint) {
            generated = generateGeneral(constraint);
        } else if (constraint instanceof FunctionalConstraint) {
            generated = generateReified((FunctionalConstraint) constraint);
        } else {
            throw new FailedGenerationException(constraint
                    + " is not supported");
        }
        if (generated == null) {
            return false;
        }
        addConstraint(generated);
        return true;
    }

}
