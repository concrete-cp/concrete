package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;
import cspfj.util.Arrays2;

public final class ReifiedEq extends AbstractAC3Constraint {

	public ReifiedEq(final Variable result, final Variable... vars) {
		super(Arrays2.addBefore(result, vars, new Variable[vars.length + 1]));
	}

	@Override
	public boolean check() {
		return getValue(0) == allEqual();
	}

	private int allEqual() {
		final int val = getValue(1);
		for (int i = getArity(); --i >= 2;) {
			if (getValue(i) != val) {
				return 0;
			}
		}
		return 1;
	}

	private String args() {
		int iMax = getArity() - 1;
		if (iMax == 0) {
			return "()";
		}
		final StringBuilder stb = new StringBuilder();
		stb.append('(');
		for (int i = 1;; i++) {
			stb.append(getVariable(i));
			if (i == iMax) {
				return stb.append(')').toString();
			}
			stb.append(", ");
		}

	}

	public String toString() {
		return getVariable(0) + " = eq" + args();
	}
}
