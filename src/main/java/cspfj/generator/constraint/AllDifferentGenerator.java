package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.semantic.AllDifferent;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class AllDifferentGenerator extends AbstractGenerator {

    public AllDifferentGenerator(final Problem problem) {
        super(problem);
    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        Preconditions.checkArgument(constraint instanceof GeneralConstraint,
                "Not supported");
        final List<Variable> solverVariables = Lists.transform(
                constraint.getScope(), CSPOM_TO_CSP4J);
        if (Iterables.any(solverVariables, NULL_DOMAIN)) {
            return false;
        }
        addConstraint(new AllDifferent(
                solverVariables.toArray(new Variable[solverVariables.size()])));
        return true;
    }

}
