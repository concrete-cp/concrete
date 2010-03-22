package cspfj.constraint.semantic;

import cspfj.constraint.AbstractFastPVRConstraint;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Abs extends AbstractFastPVRConstraint {

	final int[] corresponding1;
	final int[] corresponding2;
	final int[] correspondingR;

	public Abs(Variable result, Variable v0) {
		super(result, v0);
		final Domain resultDom = result.getDomain();
		final Domain v0Dom = v0.getDomain();

		corresponding1 = new int[resultDom.maxSize()];
		corresponding2 = new int[resultDom.maxSize()];
		for (int i = resultDom.first(); i >= 0; i = resultDom.next(i)) {
			final int val = resultDom.value(i);
			corresponding1[i] = v0Dom.index(val);
			corresponding2[i] = v0Dom.index(-val);
		}

		correspondingR = new int[v0Dom.maxSize()];
		for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
			correspondingR[i] = resultDom.index(Math.abs(v0Dom.value(i)));
		}
	}

	@Override
	public boolean check() {
		return getValue(0) == Math.abs(getValue(1));
	}

	private static boolean valid(final int index, final Domain dom) {
		return index >= 0 && dom.present(index);
	}

	@Override
	public boolean revise(int position) {
		boolean change = false;
		if (position == 0) {

			final Domain dom = getVariable(0).getDomain();
			final Domain otherDom = getVariable(1).getDomain();
			for (int i = dom.first(); i >= 0; i = dom.next(i)) {
				if (!valid(corresponding1[i], otherDom)
						&& !valid(corresponding2[i], otherDom)) {
					dom.remove(i);
					change = true;
				}
			}

		} else {

			final Domain dom = getVariable(1).getDomain();
			final Domain otherDom = getVariable(0).getDomain();
			for (int i = dom.first(); i >= 0; i = dom.next(i)) {
				if (!valid(correspondingR[i], otherDom)) {
					dom.remove(i);
					change = true;
				}
			}

		}
		return change;
	}

	public String toString() {
		return getVariable(0) + " = |" + getVariable(1) + "|";
	}

	@Override
	public int getEvaluation(int reviseCount) {
		return getVariable(0).getDomainSize() * 3 / 2
				+ getVariable(1).getDomainSize();
	}
}
