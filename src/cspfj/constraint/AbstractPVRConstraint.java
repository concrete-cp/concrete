package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractPVRConstraint extends AbstractConstraint {

	public AbstractPVRConstraint(final Variable[] scope) {
		super(scope);
		// removals = new boolean[getArity()];
	}

	public AbstractPVRConstraint(final Variable[] scope, final String name) {
		super(scope, name);

	}

	public abstract boolean revise(final int position, final int level);

	@Override
	public boolean revise(final int level, final RevisionHandler revisator) {
		for (int i = getArity(); --i >= 0;) {
			final Variable variable = getVariable(i);
			if (!variable.isAssigned() && !supportCondition(i)
					&& !skipRevision(i) && revise(i, level)) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}
				revisator.revised(this, variable);
			}
		}
		return true;
	}

	private boolean skipRevision(final int variablePosition) {
		// if (!removals[variablePosition]) {
		// return false;
		// }

		// return getRemovals(variablePosition);
		//		
		
//		return false;
//		
		for (int y = getArity(); --y >= 0;) {
			if (y == variablePosition ^ getRemovals(y)) {
				return false;
			}
		}

		return true;
	}

}
