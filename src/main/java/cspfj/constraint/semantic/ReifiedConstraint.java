package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Variable;
import cspfj.util.Arrays2;

public final class ReifiedConstraint extends AbstractConstraint {

	private final BooleanDomain controlDomain;
	private final Constraint positiveConstraint;
	private final Constraint negativeConstraint;
	private int level;

	private RevisionHandler usualRevisator;
	private RevisionHandler usualReifiedRevisator;

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
		positiveConstraint.fillRemovals(Integer.MAX_VALUE);
		negativeConstraint.fillRemovals(Integer.MAX_VALUE);

	}

	private void push() {
		for (Variable v : positiveConstraint.getScope()) {
			v.setLevel(level + 1);
		}
		positiveConstraint.setLevel(level + 1);
		negativeConstraint.setLevel(level + 1);
	}

	private void restore() {
		for (Variable v : positiveConstraint.getScope()) {
			v.restoreLevel(level);
		}
		positiveConstraint.restore(level);
		negativeConstraint.restore(level);
	}

	private final class ReifiedRevisionHandler implements RevisionHandler {

		private final RevisionHandler reifiedRevisator;

		private ReifiedRevisionHandler(final RevisionHandler reifiedRevisator) {
			this.reifiedRevisator = reifiedRevisator;
		}

		@Override
		public void revised(final Constraint constraint, final Variable variable) {
			reifiedRevisator.revised(ReifiedConstraint.this, variable);
		}

	}

	private final RevisionHandler nullRevisator = new RevisionHandler() {
		@Override
		public void revised(final Constraint constraint, final Variable variable) {
			// Nothing
		}
	};

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		final RevisionHandler reifiedRevisator;
		if (revisator == usualRevisator) {
			reifiedRevisator = usualReifiedRevisator;
		} else {
			reifiedRevisator = new ReifiedRevisionHandler(revisator);
			usualRevisator = revisator;
			usualReifiedRevisator = reifiedRevisator;
		}
		switch (controlDomain.getStatus()) {
		case UNKNOWN:
			push();
			final boolean isNegative = !positiveConstraint.revise(
					nullRevisator, reviseCount);
			restore();
			if (isNegative) {
				controlDomain.remove(1);
				if (!negativeConstraint.revise(reifiedRevisator, reviseCount)) {
					return false;
				}
				revisator.revised(this, getVariable(0));
				return true;
			}

			push();
			final boolean isPositive = !negativeConstraint.revise(
					nullRevisator, reviseCount);
			restore();

			if (isPositive) {
				controlDomain.remove(0);
				if (!positiveConstraint.revise(reifiedRevisator, reviseCount)) {
					return false;
				}
				revisator.revised(this, getVariable(0));
				return true;
			}
			return true;
		case TRUE:
			return positiveConstraint.revise(reifiedRevisator, reviseCount);
		case FALSE:
			return negativeConstraint.revise(reifiedRevisator, reviseCount);
		default:
			throw new IllegalStateException();
		}
	}

	@Override
	public void setLevel(final int newLevel) {
		super.setLevel(newLevel);
		this.level = newLevel;
	}

	@Override
	public void restore(final int newLevel) {
		super.restore(newLevel);
		this.level = newLevel;
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

	@Override
	public String toString() {
		return getVariable(0) + " == (" + positiveConstraint + ")";
	}

}
