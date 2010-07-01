package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedConstraint;
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
        if (scope.length != 2) {
            return null;
        }
        if ("eq".equals(constraint.getDescription())) {
            for (Variable v : scope) {
                if (v.getDomain() == null) {
                    v.setDomain(new BitVectorDomain(notNull.getDomain()
                            .allValues()));
                }
            }
            return new Eq(scope[0], scope[1]);
        }

        throw new IllegalArgumentException("Can not generate " + constraint);
    }

    private Constraint generateReify(final FunctionalConstraint funcConstraint)
            throws FailedGenerationException {

        if ("eq".equals(funcConstraint.getDescription())) {
            final Variable[] arguments = getSolverVariables(funcConstraint
                    .getArguments());

            if (nullVariable(arguments) != null) {
                return null;
            }
            if (arguments.length != 2) {
                return null;
            }
            if (!"eq".equals(funcConstraint.getDescription())) {
                throw new IllegalArgumentException("Can not generate "
                        + funcConstraint);
            }
            final Variable result = getSolverVariable(funcConstraint
                    .getResultVariable());

            booleanDomain(result);

            return new ReifiedConstraint(result, new Eq(arguments[0],
                    arguments[1]), new Neq(arguments[0], arguments[1]));
        }
        if ("neg".equals(funcConstraint.getDescription())) {
            final Variable[] scope = getSolverVariables(funcConstraint
                    .getScope());
            Variable notNull = notNull(scope);
            if (notNull == null) {
                return null;
            }
            if (scope.length != 2) {
                return null;
            }
            final int[] negDomain = new int[notNull.getDomainSize()];
            for (int i = negDomain.length; --i >= 0;) {
                negDomain[i] = -notNull.getValue(negDomain.length - i - 1);
            }
            for (Variable v : scope) {
                if (v.getDomain() == null) {
                    v.setDomain(new BitVectorDomain(negDomain));
                }
            }
            return new Eq(-1, scope[0], 0, scope[1]);
        }

        throw new IllegalArgumentException("Can not generate " + funcConstraint);
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
