package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractPVRConstraint extends
		AbstractArcGrainedConstraint {

	public AbstractPVRConstraint(final Variable... scope) {
		super(scope);
	}

	public AbstractPVRConstraint(final String name, final Variable... scope) {
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
		final int skip = skipRevision(reviseCount);
		for (int i = getArity(); --i >= 0;) {
			final Variable variable = getVariable(i);
			// assert (!variable.isAssigned() && skipRevision(i)) ? !revise(i)
			// : true : "Should not skip " + this + ", " + i;
			if (i != skip && revise(i)) {
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

	private int skipRevision(final int reviseCount) {
		int skip = -1;
		for (int y = getArity(); --y >= 0;) {
			if (getRemovals(y) >= reviseCount) {
				if (skip < 0) {
					skip = y;
				} else {
					return -1;
				}
			}
		}
		// throw new IllegalStateException();
		return -1;
	}
}
