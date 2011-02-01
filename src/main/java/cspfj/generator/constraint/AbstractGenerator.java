package cspfj.generator.constraint;

import com.google.common.base.Function;
import com.google.common.base.Predicate;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.variable.CSPOMVariable;

public abstract class AbstractGenerator implements Generator {

    public static Predicate<Variable> NULL_DOMAIN = new Predicate<Variable>() {
        @Override
        public boolean apply(Variable arg0) {
            return arg0.getDomain() == null;
        }
    };

    private final Problem problem;

    public Function<CSPOMVariable, Variable> cspomToCspfj = new Function<CSPOMVariable, Variable>() {
        @Override
        public Variable apply(CSPOMVariable input) {
            return problem.getVariable(input.getName());
        }
    };

    public AbstractGenerator(final Problem problem) {
        this.problem = problem;
    }

    public final Variable getSolverVariable(final CSPOMVariable variable) {
        return problem.getVariable(variable.getName());
    }

    public final void addConstraint(final Constraint constraint) {
        problem.addConstraint(constraint);
    }

    public final Variable addVariable(final String name, final Domain domain) {
        return problem.addVariable(name, domain);
    }

    public static void booleanDomain(final Variable variable)
            throws FailedGenerationException {
        if (variable.getDomain() == null) {
            variable.setDomain(new BooleanDomain());
        } else if (!(variable.getDomain() instanceof BooleanDomain)) {
            throw new FailedGenerationException(variable + " must be boolean");
        }
    }
}
