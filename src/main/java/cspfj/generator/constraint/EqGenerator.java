package cspfj.generator.constraint;

import cspfj.constraint.Constraint;
import cspfj.constraint.semantic.Eq;
import cspfj.constraint.semantic.ReifiedEq;
import cspfj.exception.FailedGenerationException;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;

public class EqGenerator extends AbstractGenerator {

	public EqGenerator(Problem problem) {
		super(problem);
	}

	@Override
	public boolean generate(CSPOMConstraint constraint)
			throws FailedGenerationException {

		final Constraint generated;

		if (constraint instanceof GeneralConstraint) {

			final Variable[] scope = getSolverVariables(constraint.getScope());
			Variable notNull = notNull(scope);
			if (notNull == null) {
				return false;
			}
			for (Variable v : scope) {
				if (v.getDomain() == null) {
					v.setDomain(new BitVectorDomain(notNull.getDomain()
							.allValues()));
				}
			}
			generated = new Eq(scope[0], scope[1]);

		} else if (constraint instanceof FunctionalConstraint) {

			final FunctionalConstraint funcConstraint = (FunctionalConstraint) constraint;

			final Variable[] arguments = getSolverVariables(funcConstraint
					.getArguments());

			if (nullVariable(arguments) != null) {
				return false;
			}

			final Variable result = getSolverVariable(funcConstraint
					.getResultVariable());

			if (result.getDomain() == null) {
				result.setDomain(new BitVectorDomain(0, 1));
			}
			generated = new ReifiedEq(result, arguments);

		} else {
			throw new IllegalArgumentException(constraint + " not supported");
		}

		addConstraint(generated);
		return true;
	}

	private static Variable notNull(Variable[] variables) {
		for (Variable v : variables) {
			if (v.getDomain() != null) {
				return v;
			}
		}
		return null;
	}

}
