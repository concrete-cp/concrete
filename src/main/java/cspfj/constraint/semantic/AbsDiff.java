package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class AbsDiff extends AbstractAC3Constraint {

	public AbsDiff(final Variable result, final Variable v0, final Variable v1) {
		super(result, v0, v1);
	}

	@Override
	public boolean check() {
		return getValue(0) == Math.abs(getValue(1) - getValue(2));
	}

	@Override
	public boolean findSupport(final int variablePosition, final int index) {
		switch (variablePosition) {
		case 0:
			return findValidTuple0(index);
		case 1:
			return findValidTuple(index, 1, 2);
		case 2:
			return findValidTuple(index, 2, 1);
		default:
			throw new IndexOutOfBoundsException();
		}
	}

	private boolean findValidTuple0(final int index) {
		final int val0 = getVariable(0).getValue(index);
		final Domain dom1 = getVariable(1).getDomain();
		final Domain dom2 = getVariable(2).getDomain();
		for (int i = dom1.first(); i >= 0; i = dom1.next(i)) {
			final int val1 = dom1.value(i);
			final int j1 = dom2.index(val1 - val0);
			if (j1 >= 0 && dom2.present(j1) && val1 >= dom2.value(j1)) {
				tuple[0] = index;
				tuple[1] = i;
				tuple[2] = j1;
				return true;
			}

			final int j2 = dom2.index(val1 + val0);
			if (j2 >= 0 && dom2.present(j2) && val1 <= dom2.value(j2)) {
				tuple[0] = index;
				tuple[1] = i;
				tuple[2] = j2;
			}
		}
		return false;
	}

	private boolean findValidTuple(final int index, final int pos1,
			final int pos2) {
		final Domain result = getVariable(0).getDomain();
		final int val = getVariable(pos1).getValue(index);
		final Domain dom = getVariable(pos2).getDomain();
		for (int i = dom.first(); i >= 0; i = dom.next(i)) {
			final int resIndex = result.index(Math.abs(val - dom.value(i)));
			if (resIndex >= 0 && result.present(resIndex)) {
				tuple[0] = resIndex;
				tuple[pos1] = index;
				tuple[pos2] = i;
				return true;
			}
		}
		return false;
	}

	public String toString() {
		return getVariable(0) + " = |" + getVariable(1) + " - "
				+ getVariable(2) + "|";
	}

	@Override
	public int getEvaluation(final int rev) {
		final int d0 = getVariable(0).getDomainSize();
		final int d1 = getVariable(1).getDomainSize();
		final int d2 = getVariable(2).getDomainSize();
		return d0 * d1 + d0 * d2 + d1 * d2;
	}
}
