package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Neq extends AbstractConstraint {

	/**
	 * Corresponding scope is reversed!
	 */
	private final int[][] corresponding;

	public Neq(final Variable v0, final Variable v1) {
		super(v0, v1);
		final Domain v0Dom = v0.getDomain();
		final Domain v1Dom = v1.getDomain();
		corresponding = new int[][] { new int[v1Dom.maxSize()],
				new int[v0Dom.maxSize()] };

		for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
			corresponding[1][i] = v1Dom.index(v0Dom.value(i));
		}
		for (int i = v1Dom.first(); i >= 0; i = v1Dom.next(i)) {
			corresponding[0][i] = v0Dom.index(v1Dom.value(i));
		}
	}

	@Override
	public boolean check() {
		return getValue(0) != getValue(1);
	}

	@Override
	public boolean revise(final RevisionHandler revisionHandler,
			final int revisionCount) {
		for (int p = 2; --p >= 0;) {
			final Domain dom = getVariable(p).getDomain();
			if (dom.size() == 1) {
				final Domain otherDom = getVariable(1 - p).getDomain();
				final int index = corresponding[1 - p][dom.first()];
				if (index >= 0 && otherDom.present(index)) {
					if (otherDom.size() == 1) {
						return false;
					}
					otherDom.remove(index);
					revisionHandler.revised(this, getVariable(1 - p));
				}
			}
		}
		if (disjoint()) {
			entail();
		}
		return true;
	}

	private boolean disjoint() {
		final int base;
		if (getVariable(0).getDomainSize() < getVariable(1).getDomainSize()) {
			base = 0;
		} else {
			base = 1;
		}
		final Domain dom0 = getVariable(base).getDomain();
		final Domain dom1 = getVariable(1 - base).getDomain();

		for (int i = dom0.first(); i >= 0; i = dom0.next(i)) {
			final int index = corresponding[1 - base][i];
			if (index >= 0 && dom1.present(index)) {
				return false;
			}
		}
		return true;

	}

	public String toString() {
		return getVariable(0) + " /= " + getVariable(1);
	}

	public int getEvaluation(final int revise) {
		return getArity()
				+ Math.min(getVariable(0).getDomainSize(), getVariable(1)
						.getDomainSize());
	}

}
