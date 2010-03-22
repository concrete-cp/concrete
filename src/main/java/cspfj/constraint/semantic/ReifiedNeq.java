package cspfj.constraint.semantic;

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public final class ReifiedNeq extends AbstractAC3Constraint {

	public ReifiedNeq(Variable result, Variable v0, Variable v1) {
		super(result, v0, v1);
	}

	@Override
	public boolean check() {
		return getValue(0) == (getValue(1) != getValue(2) ? 1 : 0);
	}

	public String toString() {
		return getVariable(0) + " = " + getVariable(1) + " /= "
				+ getVariable(2);
	}
}
