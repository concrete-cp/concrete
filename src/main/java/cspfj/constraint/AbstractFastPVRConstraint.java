package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractFastPVRConstraint extends AbstractConstraint {

	public AbstractFastPVRConstraint(final Variable... scope) {
		super(scope);
		// removals = new boolean[getArity()];
	}

	public AbstractFastPVRConstraint(final String name, final Variable... scope) {
		super(name, scope);

	}

	/**
	 * Try to filter values from variable getVariable(position).
	 * 
	 * @param position
	 * @return true iff any value has been removed
	 */
	public abstract boolean revise(final int position);

	@Override
	public final boolean revise(final RevisionHandler revisator,
			final int reviseCount) {
		int singletons = 0;
		for (int i = getArity(); --i >= 0;) {
			final Variable variable = getVariable(i);
			// assert (!variable.isAssigned() && skipRevision(i)) ? !revise(i)
			// : true : "Should not skip " + this + ", " + i;
			if (revise(i)) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}
				revisator.revised(this, variable);
			}
			if (variable.getDomainSize() == 1) {
				singletons++;
			}
		}
		if (singletons >= getArity() - 1) {
			entail();
		}
		return true;
	}

}
