package cspfj.constraint.semantic;

import java.util.Arrays;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.constraint.Constraint;
import cspfj.problem.Variable;
import cspfj.util.Arrays2;

public class ReifiedConstraint extends AbstractAC3Constraint {

	private final Variable controlVariable;
	private final Constraint positiveConstraint;
	private final Constraint negativeConstraint;

	public ReifiedConstraint(final Variable controlVariable,
			final Constraint positiveConstraint,
			final Constraint negativeConstraint) {
		super(Arrays2.addBefore(controlVariable, positiveConstraint.getScope(),
				new Variable[positiveConstraint.getArity() + 1]));

		this.controlVariable = controlVariable;

		if (!Arrays.equals(positiveConstraint.getScope(), negativeConstraint
				.getScope())) {
			throw new IllegalArgumentException(
					"Positive and negative constraints must have the same scope");
		}
		this.positiveConstraint = positiveConstraint;
		this.negativeConstraint = negativeConstraint;

	}

	@Override
	public boolean check() {
		return false;
	}

	@Override
	public int getEvaluation(int reviseCount) {
		// TODO Auto-generated method stub
		return 0;
	}
}
