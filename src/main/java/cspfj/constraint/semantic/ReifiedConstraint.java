package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Variable;
import cspfj.problem.BooleanDomain.Status;
import cspfj.util.Arrays2;

public final class ReifiedConstraint extends AbstractConstraint {

	private final BooleanDomain controlDomain;
	private final Constraint positiveConstraint;
	private final Constraint negativeConstraint;

	public ReifiedConstraint(final Variable controlVariable,
			final Constraint positiveConstraint,
			final Constraint negativeConstraint) {
		super(Arrays2.addBefore(controlVariable, positiveConstraint.getScope(),
				new Variable[positiveConstraint.getArity() + 1]));

		if (!(controlVariable.getDomain() instanceof BooleanDomain)) {
			throw new IllegalArgumentException(
					"Control variable must be boolean");
		}
		this.controlDomain = (BooleanDomain) controlVariable.getDomain();

		this.positiveConstraint = positiveConstraint;
		this.negativeConstraint = negativeConstraint;

	}

	private void push() {
		for (Variable v : getScope()) {
			v.setLevel(level + 1);
		}
		positiveConstraint.setLevel(level + 1);
		negativeConstraint.setLevel(level + 1);
	}

	private void restore() {
		for (Variable v : getScope()) {
			v.restoreLevel(level);
		}
		positiveConstraint.restore(level);
		negativeConstraint.restore(level);
	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		if (controlDomain.getStatus() == Status.UNKNOWN) {
			push();
			final boolean isNegative = !positiveConstraint.revise(revisator,
					reviseCount);
			restore();
			push();
			final boolean isPositive = !positiveConstraint.revise(revisator,
					reviseCount);
			restore();

			if (isNegative) {
				controlDomain.remove(1);
				revisator.revised(this, getVariable(0));
			}
			if (isPositive) {
				controlDomain.remove(0);
				revisator.revised(this, getVariable(0));
			}
		}

		if (controlDomain.getStatus() == Status.TRUE) {
			return positiveConstraint.revise(revisator, reviseCount);
		}
		if (controlDomain.getStatus() == Status.FALSE) {
			return negativeConstraint.revise(revisator, reviseCount);
		}
		return true;
	}

	private int level;

	@Override
	public void setLevel(final int level) {
		super.setLevel(level);
		this.level = level;
	}

	@Override
	public boolean check() {
		System.arraycopy(tuple, 1, positiveConstraint.getTuple(), 0,
				positiveConstraint.getArity());

		return (getValue(0) == 1) == positiveConstraint.check();
	}

	@Override
	public int getEvaluation(final int reviseCount) {
		return 2 * positiveConstraint.getEvaluation(reviseCount);
	}

}
