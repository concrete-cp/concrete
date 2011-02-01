package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedConstraint;
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
        Preconditions.checkArgument(constraint.getArity() == 2,
                "Comparison constraints must have exactly two arguments");
        final List<Variable> scope = Lists.transform(constraint.getScope(),
                cspomToCspfj);
        if (Iterables.any(scope, NULL_DOMAIN)) {
            return null;
        }
        return new Neq(scope.get(0), scope.get(1));
    }

    private Constraint generateReified(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        Preconditions.checkArgument(constraint.getArguments().size() == 2,
                "Comparison constraints must have exactly two arguments");
        final List<Variable> arguments = Lists.transform(
                constraint.getArguments(), cspomToCspfj);
        if (Iterables.any(arguments, NULL_DOMAIN)) {
            return null;
        }

        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        booleanDomain(result);

        return new ReifiedConstraint(result, new Neq(arguments.get(0),
                arguments.get(1)), new Eq(arguments.get(0), arguments.get(1)));
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
