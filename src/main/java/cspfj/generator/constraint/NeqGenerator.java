package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Neq;
import cspfj.constraint.semantic.ReifiedNeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public final class NeqGenerator extends AbstractGenerator {

	public NeqGenerator(Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(CSPOMConstraint constraint)
			throws FailedGenerationException {
		final Constraint generated;
		if (constraint instanceof GeneralConstraint) {

			final Variable[] scope = getSolverVariables(constraint.getScope());
			if (scope.length != 2) {
				throw new FailedGenerationException(
						"Comparison constraints must have exactly two arguments");
			}
			if (nullVariable(scope) != null) {
				return false;
			}
			generated = new Neq(scope[0], scope[1]);

		} else if (constraint instanceof FunctionalConstraint) {

			final FunctionalConstraint funcConstraint = (FunctionalConstraint) constraint;

			final Variable[] arguments = getSolverVariables(funcConstraint
					.getArguments());
			if (arguments.length != 2) {
				throw new FailedGenerationException(
						"Comparison constraints must have exactly two arguments");
			}
			if (nullVariable(arguments) != null) {
				return false;
			}

			final Variable result = getSolverVariable(funcConstraint
					.getResultVariable());
			if (result.getDomain() == null) {
				result.setDomain(new BitVectorDomain(0, 1));
			}

			generated = new ReifiedNeq(result, arguments[0], arguments[1]);

		} else {
			throw new FailedGenerationException(constraint
					+ " is not supported");
		}

		addConstraint(generated);
		return true;
	}

}
