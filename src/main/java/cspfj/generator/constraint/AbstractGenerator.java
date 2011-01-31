package cspfj.generator.constraint;

import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.variable.CSPOMVariable;

public abstract class AbstractGenerator implements Generator {

    private final Problem problem;
    public static Predicate<Variable> NULL_DOMAIN = new Predicate<Variable>() {
        @Override
        public boolean apply(Variable arg0) {
            return arg0.getDomain() == null;
        }
    };

    public Function<CSPOMVariable, Variable> CSPOM_TO_CSP4J = new Function<CSPOMVariable, Variable>() {
        @Override
        public Variable apply(CSPOMVariable input) {
            return problem.getVariable(input.getName());
        }
    };

    public AbstractGenerator(final Problem problem) {
        this.problem = problem;
    }

    //
    // public final Variable[] getSolverVariables(
    // final List<CSPOMVariable> variables) {
    // return Lists.transform(variables, CSPOM_TO_CSP4J).toArray();
    // }

    // public final Variable[] getSolverVariables(final CSPOMVariable[]
    // variables) {
    // final Variable[] solverVariables = new Variable[variables.length];
    // for (int i = variables.length; --i >= 0;) {
    // solverVariables[i] = problem.getVariable(variables[i].getName());
    // }
    // return solverVariables;
    // }

    public final Variable getSolverVariable(final CSPOMVariable variable) {
        return problem.getVariable(variable.getName());
    }

    public final void addConstraint(final Constraint constraint) {
        problem.addConstraint(constraint);
    }

    public final Variable addVariable(final String name, final Domain domain) {
        return problem.addVariable(name, domain);
    }

    // public static Variable nullVariable(final Variable[] array) {
    // for (Variable v : array) {
    // if (v.getDomain() == null) {
    // return v;
    // }
    // }
    // return null;
    // }

    public static void booleanDomain(final Variable variable)
            throws FailedGenerationException {
        if (variable.getDomain() == null) {
            variable.setDomain(new BooleanDomain());
        } else if (!(variable.getDomain() instanceof BooleanDomain)) {
            throw new FailedGenerationException(variable + " must be boolean");
        }
    }
}
