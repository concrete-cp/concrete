package cspfj.ls;

import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public final class LSConstraint {

	private final Constraint constraint;

	private final int[] tuple;

	private final LSVariable[] scope;

	private int[] positionInVariable;

	public LSConstraint(Constraint c, Map<Variable, LSVariable> lsVariables) {
		constraint = c;
		scope = new LSVariable[c.getArity()];
		for (int i = c.getArity(); --i >= 0;) {
			final Variable var = c.getVariable(i);
			scope[i] = lsVariables.get(var);
			positionInVariable[i] = c.getPosition(var);
		}
		tuple = c.getTuple();
		positionInVariable = new int[c.getArity()];

	}

	public boolean check() {
		for (int i = tuple.length; --i >= 0;) {
			tuple[i] = scope[i].getAssignedIndex();
		}
		return constraint.check();
	}

	public boolean checkWith(int variablePosition, int value) {
		for (int i = tuple.length; --i >= 0;) {
			if (i == variablePosition) {
				tuple[i] = value;
			} else {
				tuple[i] = scope[i].getAssignedIndex();
			}
		}
		return constraint.check();
	}

	public int getPosition(Variable variable) {
		return constraint.getPosition(variable);
	}

	public final int getPositionInVariable(final int position) {
		return positionInVariable[position];
	}

	public final void setPositionInVariable(final int positionInConstraint,
			final int positionInVariable) {
		this.positionInVariable[positionInConstraint] = positionInVariable;
	}

	public LSVariable[] getScope() {
		return scope;
	}

	public String toString() {
		return "ls-" + constraint.toString();
	}
}
