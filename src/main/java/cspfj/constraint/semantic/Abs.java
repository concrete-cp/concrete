package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Abs extends AbstractConstraint {

	private final int[] corresponding1;
	private final int[] corresponding2;
	private final int[] correspondingR;

	public Abs(final Variable result, final Variable v0) {
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

	private static boolean valid(final int index, final Variable var) {
		return index >= 0 && var.isPresent(index);
	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		return revise0(revisator) && revise1(revisator);
	}

	private boolean revise0(final RevisionHandler revisator) {
		boolean change = false;

		final Variable var = getVariable(0);
		final Variable otherVar = getVariable(1);
		for (int i = var.getFirst(); i >= 0; i = var.getNext(i)) {
			if (!valid(corresponding1[i], otherVar)
					&& !valid(corresponding2[i], otherVar)) {
				var.remove(i);
				change = true;
			}
		}

		if (change) {
			if (var.getDomainSize() <= 0) {
				return false;
			} else if (var.getDomainSize() == 1) {
				entail();
			}
			revisator.revised(this, getVariable(0));

		}
		return true;
	}

	private boolean revise1(final RevisionHandler revisator) {
		boolean change = false;
		final Variable var = getVariable(1);
		final Variable otherVar = getVariable(0);
		for (int i = var.getFirst(); i >= 0; i = var.getNext(i)) {
			if (!valid(correspondingR[i], otherVar)) {
				var.remove(i);
				change = true;
			}
		}

		if (change) {
			if (var.getDomainSize() <= 0) {
				return false;
			} else if (var.getDomainSize() == 1) {
				entail();
			}
			revisator.revised(this, getVariable(1));
		}
		return true;
	}

	@Override
	public String toString() {
		return getVariable(0) + " = |" + getVariable(1) + "|";
	}

	@Override
	public float getEvaluation() {
		return getVariable(0).getDomainSize() * 3 / 2
				+ getVariable(1).getDomainSize();
	}
}
