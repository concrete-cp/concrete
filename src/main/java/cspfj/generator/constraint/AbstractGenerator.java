package cspfj.generator.constraint;

import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.variable.CSPOMVariable;

public abstract class AbstractGenerator implements Generator {

	private final Problem problem;

	public AbstractGenerator(final Problem problem) {
		this.problem = problem;
	}

	public Variable[] getSolverVariables(List<CSPOMVariable> variables) {
		final Variable[] solverVariables = new Variable[variables.size()];
		for (ListIterator<CSPOMVariable> itr = variables.listIterator(); itr
				.hasNext();) {
			solverVariables[itr.nextIndex()] = problem.getVariable(itr.next()
					.getName());
		}
		return solverVariables;
	}

	public Variable[] getSolverVariables(CSPOMVariable[] variables) {
		return getSolverVariables(Arrays.asList(variables));
	}

	public Variable getSolverVariable(CSPOMVariable variable) {
		return problem.getVariable(variable.getName());
	}

	public void addConstraint(Constraint constraint) {
		problem.addConstraint(constraint);
	}

	public static Variable nullVariable(Variable[] array) {
		for (Variable v : array) {
			if (v.getDomain() == null) {
				return v;
			}
		}
		return null;
	}
}
