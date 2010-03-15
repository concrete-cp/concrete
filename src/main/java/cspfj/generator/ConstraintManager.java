package cspfj.generator;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.variable.CSPOMVariable;

public class ConstraintManager {

	private final static Map<String, Class<? extends Constraint>> known = new HashMap<String, Class<? extends Constraint>>();

	public static Class<? extends Constraint> constraint(
			CSPOMConstraint constraint) {
		return known.get(constraint.getDescription());
	}

	public static void register(final String description,
			final Class<? extends Constraint> clazz) {
		if (known.containsKey(description)) {
			throw new IllegalArgumentException(description
					+ " is already registered");
		}
		known.put(description, clazz);
	}

	public static boolean generate(CSPOMConstraint constraint, Problem problem)
			throws FailedGenerationException {
		final Class<? extends Constraint> candidate = constraint(constraint);

		if (candidate == null) {
			throw new FailedGenerationException("No candidate constraint for "
					+ constraint);
		}

		final Method method;
		try {
			method = candidate.getMethod("generate", CSPOMConstraint.class,
					Problem.class);
		} catch (SecurityException e) {
			throw new FailedGenerationException(e);
		} catch (NoSuchMethodException e) {
			throw new FailedGenerationException(e);
		}

		try {
			return (Boolean) method.invoke(null, constraint, problem);
		} catch (IllegalArgumentException e) {
			throw new FailedGenerationException(e);
		} catch (IllegalAccessException e) {
			throw new FailedGenerationException(e);
		} catch (InvocationTargetException e) {
			throw new FailedGenerationException(e);
		}
	}

	public static List<Variable> getSolverVariables(
			List<CSPOMVariable> variables, Problem problem) {
		final List<Variable> solverVariables = new ArrayList<Variable>(
				variables.size());
		for (CSPOMVariable v : variables) {
			solverVariables.add(problem.getSolverVariable(v));
		}
		return solverVariables;
	}
}
