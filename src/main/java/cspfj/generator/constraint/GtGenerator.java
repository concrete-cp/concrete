package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Gt;
import cspfj.constraint.semantic.ReifiedGt;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public class GtGenerator extends AbstractGenerator {

	public GtGenerator(Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(CSPOMConstraint constraint)
			throws FailedGenerationException {

		final Constraint generated;
		if (constraint instanceof GeneralConstraint) {
			final Variable[] solverVariables = getSolverVariables(constraint
					.getScope());

			for (Variable v : solverVariables) {
				if (v.getDomain() == null) {
					return false;
				}
			}

			if ("gt".equals(constraint.getDescription())) {
				generated = new Gt(solverVariables[0], solverVariables[1], true);
			} else if ("ge".equals(constraint.getDescription())) {
				generated = new Gt(solverVariables[0], solverVariables[1],
						false);
			} else if ("lt".equals(constraint.getDescription())) {
				generated = new Gt(solverVariables[1], solverVariables[0], true);
			} else if ("le".equals(constraint.getDescription())) {
				generated = new Gt(solverVariables[1], solverVariables[0],
						false);
			} else {
				throw new FailedGenerationException("Unhandled constraint "
						+ constraint);
			}

		} else if (constraint instanceof FunctionalConstraint) {

			final FunctionalConstraint fConstraint = (FunctionalConstraint) constraint;

			final Variable[] arguments = getSolverVariables(fConstraint
					.getArguments());
			if (arguments.length != 2) {
				throw new FailedGenerationException(
						"Comparison constraints must have exactly two arguments");
			}
			if (nullVariable(arguments) != null) {
				return false;
			}

			final Variable result = getSolverVariable(fConstraint
					.getResultVariable());

			if (result.getDomain() == null) {
				result.setDomain(new BitVectorDomain(0, 1));
			}

			if ("gt".equals(constraint.getDescription())) {
				generated = new ReifiedGt(result, arguments[0], arguments[1],
						true);
			} else if ("ge".equals(constraint.getDescription())) {
				generated = new ReifiedGt(result, arguments[0], arguments[1],
						false);
			} else if ("lt".equals(constraint.getDescription())) {
				generated = new ReifiedGt(result, arguments[1], arguments[0],
						true);
			} else if ("le".equals(constraint.getDescription())) {
				generated = new ReifiedGt(result, arguments[1], arguments[0],
						false);
			} else {
				throw new FailedGenerationException("Unhandled constraint "
						+ constraint);
			}

		} else {
			throw new IllegalArgumentException(constraint + " not supported");
		}

		addConstraint(generated);

		return true;
	}
}
