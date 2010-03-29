package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Add extends AbstractAC3Constraint {

	public Add(final Variable result, final Variable v0, final Variable v1) {
		super(result, v0, v1);
	}

	@Override
	public boolean check() {
		return getValue(0) == (getValue(1) + getValue(2));
	}

	@Override
	public boolean findSupport(final int variablePosition, final int index) {
		switch (variablePosition) {
		case 0:
			return findValidTuple0(index);
		case 1:
			return findValidTuple1(index);
		case 2:
			return findValidTuple2(index);
		default:
			throw new IndexOutOfBoundsException();
		}
	}

	private boolean findValidTuple0(final int index) {
		final int val0 = getVariable(0).getValue(index);
		final Domain dom1 = getVariable(1).getDomain();
		final Domain dom2 = getVariable(2).getDomain();
		for (int i = dom1.first(); i >= 0; i = dom1.next(i)) {
			final int j = dom2.index(val0 - dom1.value(i));
			if (j >= 0 && dom2.present(j)) {
				tuple[0] = index;
				tuple[1] = i;
				tuple[2] = j;
				return true;
			}
		}
		return false;
	}

	private boolean findValidTuple1(final int index) {
		final Domain result = getVariable(0).getDomain();
		final int val = getVariable(1).getValue(index);
		final Domain dom = getVariable(2).getDomain();
		for (int i = dom.first(); i >= 0; i = dom.next(i)) {
			final int resIndex = result.index(val + dom.value(i));
			if (resIndex >= 0 && result.present(resIndex)) {
				tuple[0] = resIndex;
				tuple[1] = index;
				tuple[2] = i;
				return true;
			}
		}
		return false;
	}

	private boolean findValidTuple2(final int index) {
		final Domain result = getVariable(0).getDomain();
		final int val = getVariable(2).getValue(index);
		final Domain dom = getVariable(1).getDomain();
		for (int i = dom.first(); i >= 0; i = dom.next(i)) {
			final int resIndex = result.index(val + dom.value(i));
			if (resIndex >= 0 && result.present(resIndex)) {
				tuple[0] = resIndex;
				tuple[1] = i;
				tuple[2] = index;
				return true;
			}
		}
		return false;
	}

	public String toString() {
		return getVariable(0) + " = " + getVariable(1) + " + " + getVariable(2);
	}

	@Override
	public int getEvaluation(final int rev) {
		final int d0 = getVariable(0).getDomainSize();
		final int d1 = getVariable(1).getDomainSize();
		final int d2 = getVariable(2).getDomainSize();
		return d0 * d1 + d0 * d2 + d1 * d2;
	}
}