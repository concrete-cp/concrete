package cspfj.generator.constraint;

import java.util.List;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.semantic.LexLeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public final class LexLeqGenerator extends AbstractGenerator {

    public LexLeqGenerator(final Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {

        final List<Variable> solverVariables = Lists.transform(
                constraint.getScope(), CSPOM_TO_CSP4J);

        if (Iterables.any(solverVariables, NULL_DOMAIN)) {
            return false;
        }

        addConstraint(new LexLeq(
                solverVariables.toArray(new Variable[solverVariables.size()])));
        return true;

    }

}
