package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class SumLeq extends AbstractConstraint {

	private final int bound;

	public SumLeq(final int bound, final Variable... variables) {
		super(variables);
		this.bound = bound;
	}

	@Override
	public boolean check() {
		int sum = 0;
		for (int i = getArity(); --i >= 0;) {
			sum += getValue(i);
			if (sum > bound) {
				return false;
			}
		}

		return true;
	}

	@Override
	public float getEvaluation() {
		return getArity();
	}

	private boolean removeGt(final int value, final Domain dom) {
		final int lb = dom.lowest(value);
		if (lb >= 0) {
			if (dom.value(lb) != value) {
				return dom.removeFrom(lb) > 0;
			}
			return dom.removeFrom(lb + 1) > 0;
		}
		return false;
	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		int min = 0;
		for (int i = getArity(); --i >= 0;) {
			final Domain dom = getVariable(i).getDomain();
			min += dom.value(dom.first());
		}
		final int newBound = bound - min;
		if (newBound < 0) {
			return false;
		}

		int max = 0;
		for (int i = getArity(); --i >= 0;) {
			final Domain dom = getVariable(i).getDomain();
			if (removeGt(newBound + dom.value(dom.first()), dom)) {
				if (dom.size() <= 0) {
					return false;
				}
				revisator.revised(this, getVariable(i));
			}
			max += dom.value(dom.last());
		}

		if (max <= bound) {
			entail();
		}

		return true;
	}

	@Override
	public String toString() {
		final StringBuilder stb = new StringBuilder();
		stb.append(getVariable(0));
		for (int i = 1; i < getArity(); i++) {
			stb.append(" + ").append(getVariable(i));
		}
		stb.append(" <= ").append(bound);
		return stb.toString();
	}
}
