package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.ReifiedEq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class EqGenerator extends AbstractGenerator {

    public EqGenerator(final Problem problem) {
        super(problem);
    }

    private Constraint generateGeneral(final CSPOMConstraint constraint) {
        final Variable[] scope = getSolverVariables(constraint.getScope());
        Variable notNull = notNull(scope);
        if (notNull == null) {
            return null;
        }
        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(notNull.getDomain().allValues()));
            }
        }
        return new Eq(scope[0], scope[1]);
    }

    private Constraint generateReify(final FunctionalConstraint funcConstraint)
            throws FailedGenerationException {
        final Variable[] arguments = getSolverVariables(funcConstraint
                .getArguments());

        if (nullVariable(arguments) != null) {
            return null;
        }

        final Variable result = getSolverVariable(funcConstraint
                .getResultVariable());

        booleanDomain(result);

        return new ReifiedEq(result, arguments);

    }

    @Override
    public boolean generate(final CSPOMConstraint constraint)
            throws FailedGenerationException {

        final Constraint generated;

        if (constraint instanceof GeneralConstraint) {
            generated = generateGeneral(constraint);
        } else if (constraint instanceof FunctionalConstraint) {
            generated = generateReify((FunctionalConstraint) constraint);
        } else {
            throw new FailedGenerationException(constraint + " not supported");
        }
        if (generated == null) {
            return false;
        }
        addConstraint(generated);
        return true;
    }

    private static Variable notNull(final Variable[] variables) {
        for (Variable v : variables) {
            if (v.getDomain() != null) {
                return v;
            }
        }
        return null;
    }

}
