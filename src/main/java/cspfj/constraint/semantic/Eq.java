package cspfj.constraint.semantic;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Eq extends AbstractPVRConstraint {

	private final int[][] corresponding;

	public Eq(final Variable v0, final Variable v1) {
		super(v0, v1);
		final Domain v0Dom = v0.getDomain();
		final Domain v1Dom = v1.getDomain();
		corresponding = new int[][] { new int[v0Dom.maxSize()],
				new int[v1Dom.maxSize()] };

		for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
			corresponding[0][i] = v1Dom.index(v0Dom.value(i));
		}
		for (int i = v1Dom.first(); i >= 0; i = v1Dom.next(i)) {
			corresponding[1][i] = v0Dom.index(v1Dom.value(i));
		}
	}

	@Override
	public boolean check() {
		return getValue(0) == getValue(1);
	}

	public boolean revise(final int position) {
		boolean change = false;
		final Domain dom = getVariable(position).getDomain();
		final Domain otherDom = getVariable(1 - position).getDomain();
		final int[] correspond = corresponding[position];
		for (int i = dom.first(); i >= 0; i = dom.next(i)) {
			final int index = correspond[i];
			if (index < 0 || !otherDom.present(index)) {
				dom.remove(i);
				change = true;
			}
		}
		return change;
	}

	public String toString() {
		return getVariable(0) + " == " + getVariable(1);
	}

	@Override
	public int getEvaluation(final int reviseCount) {
		return getVariable(0).getDomainSize() + getVariable(1).getDomainSize();
	}
}
