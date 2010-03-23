package cspfj.generator.constraint;

import cspfj.constraint.semantic.AllDifferent;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.GeneralConstraint;

public final class AllDifferentGenerator extends AbstractGenerator {

	public AllDifferentGenerator(final Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(final CSPOMConstraint constraint)
			throws FailedGenerationException {

		if (!(constraint instanceof GeneralConstraint)) {
			throw new FailedGenerationException(constraint
					+ " is not supported");
		}

		final Variable[] solverVariables = getSolverVariables(constraint
				.getScope());
		if (nullVariable(solverVariables) != null) {
			return false;
		}
		addConstraint(new AllDifferent(solverVariables));
		return true;
	}

}
