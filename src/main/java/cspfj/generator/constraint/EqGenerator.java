package cspfj.generator.constraint;

import java.util.List;
import java.util.NoSuchElementException;

import com.google.common.base.Preconditions;
import com.google.common.base.Predicates;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
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
        Preconditions.checkArgument("eq".equals(constraint.getDescription()));
        if (constraint.getScope().size() != 2) {
            return null;
        }
        final List<Variable> scope = Lists.transform(constraint.getScope(),
                cspomToCspfj);
        final Domain referenceDomain;
        try {
            referenceDomain = Iterables
                    .find(scope, Predicates.not(NULL_DOMAIN)).getDomain();
        } catch (NoSuchElementException e) {
            return null;
        }

        for (Variable v : Iterables.filter(scope, NULL_DOMAIN)) {
            v.setDomain(new BitVectorDomain(referenceDomain.allValues()));
        }
        return new Eq(scope.get(0), scope.get(1));

    }

    private Constraint generateReify(final FunctionalConstraint funcConstraint)
            throws FailedGenerationException {

        if ("eq".equals(funcConstraint.getDescription())) {
            final List<Variable> arguments = Lists.transform(
                    funcConstraint.getArguments(), cspomToCspfj);

            if (arguments.size() != 2 || Iterables.any(arguments, NULL_DOMAIN)) {
                return null;
            }

            final Variable result = cspomToCspfj.apply(funcConstraint
                    .getResultVariable());

            booleanDomain(result);

            return new ReifiedConstraint(result, new Eq(arguments.get(0),
                    arguments.get(1)), new Neq(arguments.get(0),
                    arguments.get(1)));
        }
        if ("neg".equals(funcConstraint.getDescription())) {
            if (funcConstraint.getScope().size() != 2) {
                return null;
            }
            final List<Variable> scope = Lists.transform(
                    funcConstraint.getScope(), cspomToCspfj);
            final Domain referenceDomain;
            try {
                referenceDomain = Iterables.find(scope,
                        Predicates.not(NULL_DOMAIN)).getDomain();
            } catch (NoSuchElementException e) {
                return null;
            }

            final int[] negDomain = new int[referenceDomain.size()];
            for (int i = negDomain.length; --i >= 0;) {
                negDomain[i] = -referenceDomain.value(negDomain.length - i - 1);
            }
            for (Variable v : Iterables.filter(scope, NULL_DOMAIN)) {
                v.setDomain(new BitVectorDomain(negDomain));
            }
            return new Eq(-1, scope.get(0), 0, scope.get(1));
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

}
