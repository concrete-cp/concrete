package cspfj.generator.constraint;

import cspfj.constraint.semantic.LexLeq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;

public final class LexLeqGenerator extends AbstractGenerator {

	public LexLeqGenerator(final Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(final CSPOMConstraint constraint)
			throws FailedGenerationException {

		Variable[] scope = this.getSolverVariables(constraint.getScope());
		if (nullVariable(scope) != null) {
			return false;
		}

		addConstraint(new LexLeq(scope));
		return true;

	}

}
