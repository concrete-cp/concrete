package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Gt;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class GtGenerator extends AbstractGenerator {

    public GtGenerator(final Problem problem) {
        super(problem);
    }

    private Constraint generateGeneral(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final List<Variable> solverVariables = Lists.transform(
                constraint.getScope(), CSPOM_TO_CSP4J);

        if (Iterables.any(solverVariables, NULL_DOMAIN)) {
            return null;
        }

        if ("gt".equals(constraint.getDescription())) {
            return new Gt(solverVariables.get(0), solverVariables.get(1), true);
        }
        if ("ge".equals(constraint.getDescription())) {
            return new Gt(solverVariables.get(0), solverVariables.get(1), false);
        }
        if ("lt".equals(constraint.getDescription())) {
            return new Gt(solverVariables.get(1), solverVariables.get(0), true);
        }
        if ("le".equals(constraint.getDescription())) {
            return new Gt(solverVariables.get(1), solverVariables.get(0), false);
        }

        throw new FailedGenerationException("Unhandled constraint "
                + constraint);
    }

    private Constraint generateReified(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        Preconditions.checkArgument(constraint.getArguments().size() == 2,
                "Comparison constraints must have exactly two arguments");
        final List<Variable> arguments = Lists.transform(
                constraint.getArguments(), CSPOM_TO_CSP4J);

        if (Iterables.any(arguments, NULL_DOMAIN)) {
            return null;
        }

        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        booleanDomain(result);

        if ("gt".equals(constraint.getDescription())) {
            return new ReifiedConstraint(result, new Gt(arguments.get(0),
                    arguments.get(1), true), new Gt(arguments.get(1),
                    arguments.get(0), false));
        }
        if ("ge".equals(constraint.getDescription())) {
            return new ReifiedConstraint(result, new Gt(arguments.get(0),
                    arguments.get(1), false), new Gt(arguments.get(1),
                    arguments.get(0), true));
        }
        if ("lt".equals(constraint.getDescription())) {
            return new ReifiedConstraint(result, new Gt(arguments.get(1),
                    arguments.get(0), true), new Gt(arguments.get(0),
                    arguments.get(1), false));
        }
        if ("le".equals(constraint.getDescription())) {
            return new ReifiedConstraint(result, new Gt(arguments.get(1),
                    arguments.get(0), false), new Gt(arguments.get(0),
                    arguments.get(1), true));
        }

        throw new FailedGenerationException("Unhandled constraint "
                + constraint);

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
            throw new IllegalArgumentException(constraint + " not supported");
        }

        if (generated == null) {
            return false;
        }

        addConstraint(generated);

        return true;
    }
}
