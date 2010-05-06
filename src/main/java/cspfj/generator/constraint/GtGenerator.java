package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Gt;
import cspfj.constraint.semantic.ReifiedConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class GtGenerator extends AbstractGenerator {

	public GtGenerator(final Problem problem) {
		super(problem);
	}

	private Constraint generateGeneral(final CSPOMConstraint constraint)
			throws FailedGenerationException {
		final Variable[] solverVariables = getSolverVariables(constraint
				.getScope());

		if (nullVariable(solverVariables) != null) {
			return null;
		}

		if ("gt".equals(constraint.getDescription())) {
			return new Gt(solverVariables[0], solverVariables[1], true);
		}
		if ("ge".equals(constraint.getDescription())) {
			return new Gt(solverVariables[0], solverVariables[1], false);
		}
		if ("lt".equals(constraint.getDescription())) {
			return new Gt(solverVariables[1], solverVariables[0], true);
		}
		if ("le".equals(constraint.getDescription())) {
			return new Gt(solverVariables[1], solverVariables[0], false);
		}

		throw new FailedGenerationException("Unhandled constraint "
				+ constraint);
	}

	private Constraint generateReified(final FunctionalConstraint constraint)
			throws FailedGenerationException {
		final Variable[] arguments = getSolverVariables(constraint
				.getArguments());
		if (arguments.length != 2) {
			throw new FailedGenerationException(
					"Comparison constraints must have exactly two arguments");
		}
		if (nullVariable(arguments) != null) {
			return null;
		}

		final Variable result = getSolverVariable(constraint
				.getResultVariable());
		booleanDomain(result);

		if ("gt".equals(constraint.getDescription())) {
			return new ReifiedConstraint(result, new Gt(arguments[0],
					arguments[1], true), new Gt(arguments[1], arguments[0],
					false));
		}
		if ("ge".equals(constraint.getDescription())) {
			return new ReifiedConstraint(result, new Gt(arguments[0],
					arguments[1], false), new Gt(arguments[1], arguments[0],
					true));
		}
		if ("lt".equals(constraint.getDescription())) {
			return new ReifiedConstraint(result, new Gt(arguments[1],
					arguments[0], true), new Gt(arguments[0], arguments[1],
					false));
		}
		if ("le".equals(constraint.getDescription())) {
			return new ReifiedConstraint(result, new Gt(arguments[1],
					arguments[0], false), new Gt(arguments[0], arguments[1],
					true));
		}

		throw new FailedGenerationException("Unhandled constraint "
				+ constraint);

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
}
