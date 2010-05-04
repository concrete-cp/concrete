package cspfj.generator.constraint;

import java.util.List;
import java.util.ListIterator;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.variable.CSPOMVariable;

public abstract class AbstractGenerator implements Generator {

    private final Problem problem;

    public AbstractGenerator(final Problem problem) {
        this.problem = problem;
    }

    public final Variable[] getSolverVariables(
            final List<CSPOMVariable> variables) {
        final Variable[] solverVariables = new Variable[variables.size()];
        for (ListIterator<CSPOMVariable> itr = variables.listIterator(); itr
                .hasNext();) {
            solverVariables[itr.nextIndex()] = problem.getVariable(itr.next()
                    .getName());
        }
        return solverVariables;
    }

    public final Variable[] getSolverVariables(final CSPOMVariable[] variables) {
        final Variable[] solverVariables = new Variable[variables.length];
        for (int i = variables.length; --i >= 0;) {
            solverVariables[i] = problem.getVariable(variables[i].getName());
        }
        return solverVariables;
    }

    public final Variable getSolverVariable(final CSPOMVariable variable) {
        return problem.getVariable(variable.getName());
    }

    public final void addConstraint(final Constraint constraint) {
        problem.addConstraint(constraint);
    }

    public static Variable nullVariable(final Variable[] array) {
        for (Variable v : array) {
            if (v.getDomain() == null) {
                return v;
            }
        }
        return null;
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
