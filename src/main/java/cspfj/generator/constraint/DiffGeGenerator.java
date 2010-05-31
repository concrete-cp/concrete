package cspfj.generator.constraint;

import java.util.Arrays;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Gt;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class DiffGeGenerator extends AbstractGenerator {

    public DiffGeGenerator(final Problem problem) {
        super(problem);
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

    private Constraint generateGeneral(final CSPOMConstraint constraint)
            throws FailedGenerationException {
        final Variable v0;
        final Variable v1;
        final Variable bound;
        if ("diffGe".equals(constraint.getDescription())) {
            v0 = getSolverVariable(constraint.getVariable(0));
            v1 = getSolverVariable(constraint.getVariable(1));
            bound = getSolverVariable(constraint.getVariable(2));
        } else {
            throw new IllegalArgumentException("Cannot handle " + constraint);
        }

        if (bound.getDomainSize() != 1) {
            return null;
        }

        for (Variable v : Arrays.asList(v0, v1)) {
            if (v.getDomain() == null) {
                return null;
            }
        }

        return new Gt(v0, bound.getValue(0), v1, false);
    }

    private Constraint generateReified(final FunctionalConstraint constraint)
            throws FailedGenerationException {
        final Variable v0;
        final Variable v1;
        final Variable bound;
        if ("diffGe".equals(constraint.getDescription())) {
            v0 = getSolverVariable(constraint.getVariable(1));
            v1 = getSolverVariable(constraint.getVariable(2));
            bound = getSolverVariable(constraint.getVariable(3));
        } else {
            throw new IllegalArgumentException("Cannot handle " + constraint);
        }

        if (bound.getDomainSize() != 1) {
            return null;
        }

        for (Variable v : Arrays.asList(v0, v1)) {
            if (v.getDomain() == null) {
                return null;
            }
        }

        final Variable result = getSolverVariable(constraint
                .getResultVariable());
        booleanDomain(result);
        return new ReifiedConstraint(result, new Gt(v0, -bound.getValue(0), v1,
                false), new Gt(v1, bound.getValue(0), v0, true));
    }

}
