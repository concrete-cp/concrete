package cspfj.constraint.semantic;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Eq extends AbstractPVRConstraint {

	private final int[][] corresponding;

	private final int a, b;

	/**
	 * Constraint x = y.
	 * 
	 * @param x
	 * @param y
	 */
	public Eq(final Variable x, final Variable y) {
		this(1, x, 0, y);
	}

	/**
	 * Constraint ax + b = y.
	 * 
	 * @param a
	 * @param x
	 * @param b
	 * @param y
	 */
	public Eq(final int a, final Variable x, final int b, final Variable y) {
		super(x, y);
		if (a == 0) {
			throw new IllegalArgumentException("a must be != 0");
		}
		final Domain xDom = x.getDomain();
		final Domain yDom = y.getDomain();
		corresponding = new int[][] { new int[xDom.maxSize()],
				new int[yDom.maxSize()] };

		for (int i = xDom.firstIndex(); i >= 0; i = xDom.next(i)) {
			corresponding[0][i] = yDom.index(a * xDom.value(i) + b);
		}

		for (int i = yDom.firstIndex(); i >= 0; i = yDom.next(i)) {
			final int r = yDom.value(i) - b;
			if (r % a == 0) {
				corresponding[1][i] = xDom.index(r / a);
			} else {
				corresponding[1][i] = -1;
			}
		}
		this.a = a;
		this.b = b;
	}

	@Override
	public boolean check() {
		return a * getValue(0) + b == getValue(1);
	}

	public boolean revise(final int position) {
		boolean change = false;
		final Variable var = getVariable(position);
		final Variable otherVar = getVariable(1 - position);
		final int[] correspond = corresponding[position];
		for (int i = var.getFirst(); i >= 0; i = var.getNext(i)) {
			final int index = correspond[i];
			if (index < 0 || !otherVar.isPresent(index)) {
				var.remove(i);
				change = true;
			}
		}
		return change;
	}

	@Override
	public boolean isConsistent(final int reviseCount) {
		final Domain dom = getVariable(0).getDomain();
		final Domain otherDom = getVariable(1).getDomain();
		final int[] correspond = corresponding[0];
		for (int i = dom.firstIndex(); i >= 0; i = dom.next(i)) {
			final int index = correspond[i];
			if (index >= 0 && otherDom.present(index)) {
				return true;
			}
		}
		return false;
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		if (a != 1) {
			stb.append(a).append('.');
		}
		stb.append(getVariable(0));
		if (b > 0) {
			stb.append(" + ").append(b);
		} else if (b < 0) {
			stb.append(" - ").append(-b);
		}
		return stb.append(" = ").append(getVariable(1)).toString();
	}

	@Override
	public float getEvaluation() {
		return getVariable(0).getDomainSize() + getVariable(1).getDomainSize();
	}
}
