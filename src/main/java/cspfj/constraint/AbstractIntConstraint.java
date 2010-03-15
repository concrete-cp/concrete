package cspfj.constraint;

import cspfj.problem.IntVariable;

public abstract class AbstractIntConstraint extends AbstractConstraint {

	private final IntVariable[] intScope;

	public AbstractIntConstraint(final IntVariable... scope) {
		this(null, scope);
	}

	public AbstractIntConstraint(final String name, final IntVariable... scope) {
		super(name, scope);
		this.intScope = scope;
	}

	public IntVariable getVariable(int position) {
		return intScope[position];
	}

	public IntVariable[] getScope() {
		return intScope;
	}
}
