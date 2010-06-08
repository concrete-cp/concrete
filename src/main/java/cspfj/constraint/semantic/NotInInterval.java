package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class NotInInterval extends AbstractConstraint {

	private final int lb, ub;
	private final Domain domain;

	public NotInInterval(final Variable variable, final int lb, final int ub) {
		super(variable);

		this.domain = variable.getDomain();
		this.lb = domain.index(lb);
		this.ub = domain.index(ub);
	}

	@Override
	public int getEvaluation(final int reviseCount) {
		return 0;
	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		final int lb;
		if (this.lb < 0) {
			lb = domain.first();
		} else {
			lb = this.lb;
		}
		final int ub;
		if (this.ub < 0) {
			ub = domain.last();
		} else {
			ub = this.ub;
		}
		boolean changed = false;
		for (int i = lb; i <= ub; i = domain.next(i)) {
			domain.remove(i);
			changed = true;
		}
		if (domain.size() <= 0) {
			return false;
		}
		if (changed) {
			revisator.revised(this, getVariable(0));
		}
		entail();
		return true;
	}

	@Override
	public boolean isConsistent(final int reviseCount) {
		return (this.lb > 0 && domain.first() < lb)
				|| (this.ub >= 0 && domain.last() > ub);
	}

	@Override
	public boolean check() {
		final int value = getValue(0);
		return value < lb || ub < value;
	}
	
	@Override
	public String toString() {
		return getVariable(0) + " notin [" + lb + ", " + ub + "]";
	}
}
