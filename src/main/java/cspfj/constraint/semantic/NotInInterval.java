package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class NotInInterval extends AbstractConstraint {

	private final int lb, ub;
	private final Variable variable;

	private NotInInterval(final Variable variable, final int lb, final int ub) {
		super(variable);

		this.variable = variable;
		this.lb = lb;
		this.ub = ub;
	}

	public static NotInInterval values(final Variable variable, final int lb,
			final int ub) {
		final Domain domain = variable.getDomain();
		return new NotInInterval(variable, domain.lowest(lb), domain
				.greatest(ub));
	}

	public static NotInInterval indexes(final Variable variable, final int lb,
			final int ub) {
		return new NotInInterval(variable, lb, ub);
	}

	@Override
	public float getEvaluation() {
		return 0;
	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		boolean changed = false;
		for (int i = lb; 0 <= i && i <= ub; i = variable.getNext(i)) {
			if (variable.isPresent(i)) {
				variable.remove(i);
				changed = true;
			}
		}
		if (changed) {
			if (variable.getDomainSize() <= 0) {
				return false;
			}
			revisator.revised(this, getVariable(0));
		}
		entail();
		return true;
	}

	@Override
	public boolean isConsistent(final int reviseCount) {
		return variable.getFirst() < lb || variable.getLast() > ub;
	}

	@Override
	public boolean check() {
		final int value = getValue(0);
		return value < lb || ub < value;
	}

	@Override
	public String toString() {
		return getVariable(0) + " notin [" + getVariable(0).getValue(lb) + ", "
				+ getVariable(0).getValue(ub) + "]";
	}
}
